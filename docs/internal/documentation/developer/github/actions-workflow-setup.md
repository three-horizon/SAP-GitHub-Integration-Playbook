# 📄 GitHub Actions Setup for SAP Integration

## 📋 Table of Contents

- [Overview](#overview)
- [Prerequisites](#prerequisites)
- [Setup Checklist](#setup-checklist)
- [GitHub Repository Configuration](#github-repository-configuration)
- [Secrets Configuration](#secrets-configuration)
- [Workflow Configuration](#workflow-configuration)
  - [SAP Code Validation Workflow](#sap-code-validation-workflow)
  - [SAP Deployment Workflow](#sap-deployment-workflow)
  - [Transport Management Workflow](#transport-management-workflow)
- [Advanced Configurations](#advanced-configurations)
- [Monitoring and Troubleshooting](#monitoring-and-troubleshooting)
- [Next Steps](#next-steps)

## 📋 Overview

This guide explains how to set up GitHub Actions workflows for automating SAP development processes. GitHub Actions enables continuous integration, validation, and deployment of ABAP code between GitHub repositories and SAP systems.

## ✅ Prerequisites

Before setting up GitHub Actions for SAP integration, ensure you have:

- GitHub Enterprise account with admin access to repositories
- SAP system with configured abapGit
- Microsoft API Management instance configured for SAP-GitHub integration
- Appropriate access tokens and credentials
- SAP technical user with required authorizations
- Network connectivity between GitHub and API Management

For detailed system requirements, see the [Prerequisites](../getting-started/prerequisites.md) document.

## Setup Checklist

- [ ] Configure GitHub repository settings
- [ ] Create required secrets for authentication
- [ ] Configure branch protection rules
- [ ] Set up ABAP syntax validation workflow
- [ ] Configure deployment workflow for SAP systems
- [ ] Set up transport management workflow
- [ ] Test the complete workflow chain
- [ ] Configure monitoring and notifications

## GitHub Repository Configuration

### 1. Repository Settings

1. Navigate to your repository on GitHub Enterprise
2. Go to "Settings" > "General"
3. Configure the following:
   - Default branch: `main` (or your preferred branch)
   - Allow merge commits, squash merging, and rebase merging as per your strategy
   - Enable "Automatically delete head branches" for cleaner repository maintenance

### 2. Branch Protection Rules

1. Go to "Settings" > "Branches"
2. Click "Add rule"
3. Configure protection for your `main` branch (and optionally `develop`):
   - Require pull request reviews before merging
   - Require status checks to pass before merging
   - Require branches to be up to date before merging
   - Include administrators in these restrictions

## Secrets Configuration

Store sensitive information as GitHub secrets to be used securely in workflows:

1. Go to "Settings" > "Secrets and variables" > "Actions"
2. Click "New repository secret"
3. Add the following secrets:
   - `SAP_API_ENDPOINT`: The API Management endpoint URL
   - `SAP_API_KEY`: API Management subscription key
   - `SAP_USERNAME`: SAP technical user username
   - `SAP_PASSWORD`: SAP technical user password
   - `AZURE_CREDENTIALS`: JSON containing Azure credentials (if using Azure services)

## Workflow Configuration

Create workflow YAML files in the `.github/workflows/` directory of your repository.

### SAP Code Validation Workflow

Create a file named `.github/workflows/sap-validate.yml`:

```yaml
name: SAP Code Validation

on:
  pull_request:
    branches: [ main, develop ]
    paths:
      - 'src/**/*.abap'
      - 'src/**/*.ddls'
      - 'src/**/*.dcls'
      - 'src/**/*.cds'

jobs:
  abap-lint:
    runs-on: ubuntu-latest
    steps:
      - name: Checkout code
        uses: actions/checkout@v3
      
      - name: Setup Node.js
        uses: actions/setup-node@v3
        with:
          node-version: '16'
      
      - name: Install abaplint
        run: npm install -g @abaplint/cli
      
      - name: Run ABAP Lint
        run: abaplint
      
      - name: Check ABAP Syntax via API
        uses: actions/github-script@v6
        with:
          script: |
            const response = await fetch('${{ secrets.SAP_API_ENDPOINT }}/abap/syntax/check', {
              method: 'POST',
              headers: {
                'Content-Type': 'application/json',
                'Ocp-Apim-Subscription-Key': '${{ secrets.SAP_API_KEY }}'
              },
              body: JSON.stringify({
                files: context.payload.pull_request.changed_files
              })
            });
            
            const result = await response.json();
            if (result.status !== 'success') {
              core.setFailed(`ABAP syntax check failed: ${result.message}`);
            }
```

### SAP Deployment Workflow

Create a file named `.github/workflows/sap-deploy.yml`:

```yaml
name: SAP Deployment

on:
  push:
    branches: [ main, develop ]
    paths:
      - 'src/**/*.abap'
      - 'src/**/*.ddls'
      - 'src/**/*.dcls'
      - 'src/**/*.cds'
  
  workflow_dispatch:
    inputs:
      transport_request:
        description: 'Transport request ID'
        required: false
      target_system:
        description: 'Target SAP system'
        required: true
        default: 'DEV'
        type: choice
        options:
          - DEV
          - QAS
          - PRD

env:
  TRANSPORT_REQUEST: ${{ github.event.inputs.transport_request || '' }}
  TARGET_SYSTEM: ${{ github.event.inputs.target_system || 'DEV' }}

jobs:
  deploy-to-sap:
    runs-on: ubuntu-latest
    steps:
      - name: Checkout code
        uses: actions/checkout@v3
      
      - name: Create Transport Request if needed
        if: env.TRANSPORT_REQUEST == ''
        id: create_transport
        run: |
          response=$(curl -X POST "${{ secrets.SAP_API_ENDPOINT }}/transport/create" \
            -H "Content-Type: application/json" \
            -H "Ocp-Apim-Subscription-Key: ${{ secrets.SAP_API_KEY }}" \
            -d '{
              "description": "Changes from GitHub: ${{ github.repository }} - ${{ github.sha }}",
              "owner": "${{ secrets.SAP_USERNAME }}",
              "targetSystem": "${{ env.TARGET_SYSTEM }}"
            }')
            
          echo "Response: $response"
          transport_id=$(echo $response | jq -r '.transportId')
          echo "TRANSPORT_ID=$transport_id" >> $GITHUB_ENV
          echo "transport_id=$transport_id" >> $GITHUB_OUTPUT
      
      - name: Use existing Transport Request
        if: env.TRANSPORT_REQUEST != ''
        run: |
          echo "TRANSPORT_ID=${{ env.TRANSPORT_REQUEST }}" >> $GITHUB_ENV
      
      - name: Push to SAP
        run: |
          curl -X POST "${{ secrets.SAP_API_ENDPOINT }}/abapgit/push" \
            -H "Content-Type: application/json" \
            -H "Ocp-Apim-Subscription-Key: ${{ secrets.SAP_API_KEY }}" \
            -d '{
              "transportRequest": "${{ env.TRANSPORT_ID }}",
              "commitId": "${{ github.sha }}",
              "repository": "${{ github.repository }}",
              "branch": "${{ github.ref_name }}"
            }'
      
      - name: Update Deployment Status
        uses: actions/github-script@v6
        with:
          github-token: ${{ secrets.GITHUB_TOKEN }}
          script: |
            github.rest.repos.createDeployment({
              owner: context.repo.owner,
              repo: context.repo.repo,
              ref: "${{ github.sha }}",
              environment: "SAP-${{ env.TARGET_SYSTEM }}",
              auto_merge: false,
              required_contexts: [],
              description: "Deployed to SAP ${{ env.TARGET_SYSTEM }} with transport ${{ env.TRANSPORT_ID }}"
            }).then(response => {
              github.rest.repos.createDeploymentStatus({
                owner: context.repo.owner,
                repo: context.repo.repo,
                deployment_id: response.data.id,
                state: "success",
                description: "Successfully deployed to SAP ${{ env.TARGET_SYSTEM }}"
              });
            });
```

### Transport Management Workflow

Create a file named `.github/workflows/transport-release.yml`:

```yaml
name: SAP Transport Release

on:
  workflow_dispatch:
    inputs:
      transport_request:
        description: 'Transport request ID'
        required: true
      source_system:
        description: 'Source system'
        required: true
        default: 'DEV'
        type: choice
        options:
          - DEV
          - QAS
      target_system:
        description: 'Target system'
        required: true
        default: 'QAS'
        type: choice
        options:
          - QAS
          - PRD

jobs:
  release-transport:
    runs-on: ubuntu-latest
    steps:
      - name: Release Transport
        id: release
        run: |
          response=$(curl -X POST "${{ secrets.SAP_API_ENDPOINT }}/transport/release" \
            -H "Content-Type: application/json" \
            -H "Ocp-Apim-Subscription-Key: ${{ secrets.SAP_API_KEY }}" \
            -d '{
              "transportId": "${{ github.event.inputs.transport_request }}",
              "sourceSystem": "${{ github.event.inputs.source_system }}",
              "targetSystem": "${{ github.event.inputs.target_system }}"
            }')
            
          echo "Response: $response"
          
          status=$(echo $response | jq -r '.status')
          if [ "$status" != "success" ]; then
            echo "::error::Transport release failed: $(echo $response | jq -r '.message')"
            exit 1
          fi
      
      - name: Update Release Status
        uses: actions/github-script@v6
        with:
          github-token: ${{ secrets.GITHUB_TOKEN }}
          script: |
            github.rest.repos.createDeployment({
              owner: context.repo.owner,
              repo: context.repo.repo,
              ref: "${{ github.sha || 'main' }}",
              environment: "SAP-${{ github.event.inputs.target_system }}",
              auto_merge: false,
              required_contexts: [],
              description: "Released transport ${{ github.event.inputs.transport_request }} to ${{ github.event.inputs.target_system }}"
            }).then(response => {
              github.rest.repos.createDeploymentStatus({
                owner: context.repo.owner,
                repo: context.repo.repo,
                deployment_id: response.data.id,
                state: "success",
                description: "Successfully released to ${{ github.event.inputs.target_system }}"
              });
            });
```

## Advanced Configurations

### Scheduled Background Synchronization

Create a file named `.github/workflows/background-sync.yml`:

```yaml
name: Background Synchronization

on:
  schedule:
    # Run at 2 AM every day
    - cron: '0 2 * * *'

jobs:
  sync-repos:
    runs-on: ubuntu-latest
    steps:
      - name: Trigger Background Sync
        run: |
          curl -X POST "${{ secrets.SAP_API_ENDPOINT }}/abapgit/background-sync" \
            -H "Content-Type: application/json" \
            -H "Ocp-Apim-Subscription-Key: ${{ secrets.SAP_API_KEY }}" \
            -d '{
              "repositories": ["repo1", "repo2"],
              "mode": "pull"
            }'
```

### Workflow Approval Process

For sensitive operations like production deployments, add approval steps:

1. Go to "Settings" > "Environments"
2. Create environments for DEV, QAS, and PRD
3. For QAS and PRD, enable "Required reviewers"
4. Add individuals or teams who can approve deployments
5. Update your workflow to use environments:

```yaml
jobs:
  deploy-to-prod:
    runs-on: ubuntu-latest
    environment: PRD
    # The job will pause until approved
    steps:
      # deployment steps
```

## Monitoring and Troubleshooting

### Logging and Notifications

1. Configure notifications:
   ```yaml
   steps:
     - name: Notify on Slack
       uses: slackapi/slack-github-action@v1.23.0
       with:
         channel-id: 'C123456'
         slack-message: "Deployment to SAP ${{ env.TARGET_SYSTEM }} completed with transport ${{ env.TRANSPORT_ID }}"
       env:
         SLACK_BOT_TOKEN: ${{ secrets.SLACK_BOT_TOKEN }}
   ```

2. View logs:
   - GitHub Actions logs in repository Actions tab
   - API Management logs in Azure portal
   - SAP system logs

### Common Issues and Solutions

1. **Authentication Failures**
   - Verify secret values
   - Check API Management authentication

2. **Transport Errors**
   - Verify transport has correct owner
   - Check transport status in SAP (transaction SE09)

3. **Workflow Failures**
   - Check the exact step that failed
   - Verify all prerequisites are met

## ➡️ Next Steps

After configuring GitHub Actions workflows:

1. Configure [API Management](../api-management/configuration.md) for secure integration
2. Ensure [abapGit](../implementation/abapgit-setup.md) is properly set up in your SAP environment
3. Review the overall [Implementation Plan](../getting-started/implementation-plan.md) for next steps in your project

---

**Last Updated:** 2023-09-01 
---


