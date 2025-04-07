# 📄 Setting Up GitHub Actions Workflows for SAP Development

## 📋 Table of Contents

- [📋 Overview](#overview)
- [✅ Prerequisites](#prerequisites)
- [Step 1: Create the Workflows Directory](#step-1-create-the-workflows-directory)
- [Step 2: Set Up ABAP Syntax Checking Workflow](#step-2-set-up-abap-syntax-checking-workflow)
- [Step 3: Create abaplint Configuration File](#step-3-create-abaplint-configuration-file)
- [Step 4: Set Up SAP Transport Validation Workflow](#step-4-set-up-sap-transport-validation-workflow)
- [Step 5: Configure GitHub Repository Secrets](#step-5-configure-github-repository-secrets)
- [Step 6: Set Up Automated Transport Release Workflow](#step-6-set-up-automated-transport-release-workflow)
- [Step 7: Create a Quality Gate Workflow](#step-7-create-a-quality-gate-workflow)
- [Step 8: Create GitHub Environments for Approvals](#step-8-create-github-environments-for-approvals)
- [Step 9: Set Up Pull Request Template](#step-9-set-up-pull-request-template)
- [Step 10: Test the Complete Workflow](#step-10-test-the-complete-workflow)
- [Validation](#validation)
- [Troubleshooting](#troubleshooting)
- [➡
️ ️ Next Steps](#next-steps)
- [Additional Resources](#additional-resources)


This tutorial provides step-by-step instructions for implementing Continuous Integration/Continuous Deployment (CI/CD) workflows using GitHub Actions specifically tailored for SAP development.

## 📋 Overview

GitHub Actions workflows enable automated testing, validation, and deployment of your SAP code, bringing modern DevOps practices to ABAP development. With proper GitHub Actions setup, you can:

- Automatically validate ABAP syntax and coding standards
- Verify SAP transport requests and dependencies
- Run automated tests for your SAP objects
- Manage the release process across your SAP landscape
- Create audit trails for compliance requirements

This tutorial guides you through setting up essential GitHub Actions workflows for SAP development.

## ✅ Prerequisites

Before you begin, ensure you have:

- [ ] A GitHub repository set up for SAP development (see [Setting Up Your First SAP-GitHub Repository](./first-repository.md))
- [ ] Branch protection rules configured (see [Configuring Branch Protection Rules](./branch-protection.md))
- [ ] Administrative access to your GitHub repository
- [ ] SAP system connection details (host, client, credentials)
- [ ] abapGit installed and configured in your SAP system

## Step 1: Create the Workflows Directory

GitHub Actions workflows are defined as YAML files in the `.github/workflows` directory:

1. In your repository, create a directory structure:
   ```
   .github/
     workflows/
   ```

2. You can do this via the GitHub web interface:
   - Navigate to your repository
   - Click "Add file" → "Create new file"
   - Enter `.github/workflows/abap-lint.yml` as the file name
   - This will automatically create the directory structure

## Step 2: Set Up ABAP Syntax Checking Workflow

Let's create a basic workflow to validate ABAP syntax:

1. Create a file named `.github/workflows/abap-lint.yml` with the following content:

   ```yaml
   name: ABAP Syntax Check
   
   on:
     pull_request:
       branches: [ main, develop ]
       paths:
         - 'src/**/*.abap'
         - 'src/**/*.xml'
   
   jobs:
     abaplint:
       runs-on: ubuntu-latest
       steps:
         - uses: actions/checkout@v3
         
         - name: Run abaplint
           uses: abaplint/actions-abaplint@main
           env:
             GITHUB_TOKEN: ${{ secrets.GITHUB_TOKEN }}
           
         - name: Annotate Code Linting Results
           uses: yuzutech/annotations-action@v0.4.0
           with:
             repo-token: "${{ secrets.GITHUB_TOKEN }}"
             title: "ABAP Lint Results"
             input: ./abaplint.results.json
   ```

2. Commit this file to your repository.

## Step 3: Create abaplint Configuration File

To customize ABAP lint checks for SAP development:

1. Create a file named `abaplint.json` in the root of your repository:

   ```json
   {
     "global": {
       "files": "/src/**/*.*",
       "skipGeneratedGatewayClasses": true,
       "skipGeneratedPersistentClasses": true,
       "skipGeneratedFunctionGroups": true
     },
     "syntax": {
       "version": "v757",
       "errorNamespace": "^Z",
       "globalConstants": [],
       "globalMacros": []
     },
     "rules": {
       "indentation": {
         "severity": "error",
         "alignTryCatch": true,
         "globalClassSkipFirst": false,
         "ignoreExceptions": true,
         "indentationSize": 2
       },
       "begin_end_names": true,
       "check_syntax": true,
       "global_class": true,
       "line_length": {
         "length": 120
       },
       "obsolete_statement": {
         "severity": "error",
         "refresh": true,
         "compute": true,
         "add": true,
         "subtract": true,
         "multiply": true,
         "move": true,
         "divide": true,
         "requested": true
       },
       "parser_error": true,
       "space_before_colon": true
     }
   }
   ```

2. Customize this configuration based on your organization's coding standards.

## Step 4: Set Up SAP Transport Validation Workflow

Now, let's create a workflow to validate SAP transport requests associated with pull requests:

1. Create a file named `.github/workflows/transport-validation.yml`:

   ```yaml
   name: SAP Transport Validation
   
   on:
     pull_request:
       branches: [ main, develop ]
       types: [opened, edited, synchronize, reopened]
   
   jobs:
     validate-transport:
       runs-on: ubuntu-latest
       steps:
         - uses: actions/checkout@v3
         
         - name: Extract transport numbers
           id: extract-transports
           run: |
             TRANSPORTS=$(grep -o '\[TRANSPORT:[A-Z0-9]*\]' "${{ github.event.pull_request.body }}" | cut -d':' -f2 | cut -d']' -f1)
             echo "transports=$TRANSPORTS" >> $GITHUB_OUTPUT
         
         - name: Validate transport
           if: steps.extract-transports.outputs.transports != ''
           uses: your-org/sap-transport-validator@v1  # Replace with your custom action or use community actions
           with:
             sap-host: ${{ secrets.SAP_HOST }}
             sap-client: ${{ secrets.SAP_CLIENT }}
             sap-user: ${{ secrets.SAP_SERVICE_USER }}
             sap-password: ${{ secrets.SAP_SERVICE_PASSWORD }}
             transport-numbers: ${{ steps.extract-transports.outputs.transports }}
   ```

2. This workflow assumes you have a custom GitHub Action for SAP transport validation. You'll either need to create this action or use a community-provided one.

## Step 5: Configure GitHub Repository Secrets

To securely store SAP connection details:

1. Navigate to your repository on GitHub
2. Go to **Settings** → **Secrets and variables** → **Actions**
3. Click **New repository secret**
4. Add the following secrets:
   - `SAP_HOST`: Your SAP system hostname
   - `SAP_CLIENT`: Your SAP client number
   - `SAP_SERVICE_USER`: Service user for SAP integration
   - `SAP_SERVICE_PASSWORD`: Password for service user
5. Click **Add secret** for each

## Step 6: Set Up Automated Transport Release Workflow

Create a workflow that automatically releases transports when PRs are merged:

1. Create a file named `.github/workflows/transport-release.yml`:

   ```yaml
   name: Release SAP Transport
   
   on:
     pull_request:
       types: [closed]
       branches: [main]
   
   jobs:
     release-transport:
       if: github.event.pull_request.merged == true
       runs-on: ubuntu-latest
       steps:
         - name: Extract transport numbers
           id: extract-transports
           run: |
             PR_BODY=$(curl -s -H "Authorization: token ${{ secrets.GITHUB_TOKEN }}" \
                       "https://api.github.com/repos/${{ github.repository }}/pulls/${{ github.event.pull_request.number }}" \
                       | jq -r .body)
             TRANSPORTS=$(echo "$PR_BODY" | grep -o '\[TRANSPORT:[A-Z0-9]*\]' | cut -d':' -f2 | cut -d']' -f1)
             echo "transports=$TRANSPORTS" >> $GITHUB_OUTPUT
         
         - name: Release transport to QAS
           if: steps.extract-transports.outputs.transports != ''
           uses: your-org/sap-transport-releaser@v1  # Replace with your custom action
           with:
             sap-host: ${{ secrets.SAP_HOST }}
             sap-client: ${{ secrets.SAP_CLIENT }}
             sap-user: ${{ secrets.SAP_SERVICE_USER }}
             sap-password: ${{ secrets.SAP_SERVICE_PASSWORD }}
             transport-numbers: ${{ steps.extract-transports.outputs.transports }}
             target-system: QAS
             
         - name: Add comment to PR
           if: steps.extract-transports.outputs.transports != ''
           uses: actions/github-script@v6
           with:
             github-token: ${{ secrets.GITHUB_TOKEN }}
             script: |
               github.rest.issues.createComment({
                 issue_number: context.issue.number,
                 owner: context.repo.owner,
                 repo: context.repo.repo,
                 body: `Transport(s) ${{ steps.extract-transports.outputs.transports }} automatically released to QAS system.`
               })
   ```

## Step 7: Create a Quality Gate Workflow

Set up a workflow that enforces quality checks before promotion to production:

1. Create a file named `.github/workflows/quality-gate.yml`:

   ```yaml
   name: SAP Quality Gate
   
   on:
     workflow_dispatch:
       inputs:
         transport_numbers:
           description: 'Comma-separated transport request numbers'
           required: true
         target_system:
           description: 'Target system for promotion'
           required: true
           default: 'QAS'
           type: choice
           options:
             - QAS
             - PRD
   
   jobs:
     quality-gate:
       runs-on: ubuntu-latest
       environment: ${{ github.event.inputs.target_system }}  # Requires approval
       steps:
         - name: Run SAP quality checks
           uses: your-org/sap-quality-checker@v1  # Replace with your custom action
           with:
             sap-host: ${{ secrets.SAP_HOST }}
             sap-client: ${{ secrets.SAP_CLIENT }}
             sap-user: ${{ secrets.SAP_SERVICE_USER }}
             sap-password: ${{ secrets.SAP_SERVICE_PASSWORD }}
             transport-numbers: ${{ github.event.inputs.transport_numbers }}
             target-system: ${{ github.event.inputs.target_system }}
             
         - name: Promote transport if checks pass
           if: success()
           uses: your-org/sap-transport-promoter@v1  # Replace with your custom action
           with:
             sap-host: ${{ secrets.SAP_HOST }}
             sap-client: ${{ secrets.SAP_CLIENT }}
             sap-user: ${{ secrets.SAP_SERVICE_USER }}
             sap-password: ${{ secrets.SAP_SERVICE_PASSWORD }}
             transport-numbers: ${{ github.event.inputs.transport_numbers }}
             target-system: ${{ github.event.inputs.target_system }}
   ```

## Step 8: Create GitHub Environments for Approvals

To require manual approvals for production deployments:

1. Navigate to your repository on GitHub
2. Go to **Settings** → **Environments**
3. Click **New environment**
4. Enter `QAS` as the name
5. Click **Configure environment**
6. Add required reviewers if needed for QA
7. Click **Save protection rules**
8. Repeat for `PRD` environment with stricter approval requirements

## Step 9: Set Up Pull Request Template

Create a template that includes transport information:

1. Create a file at `.github/PULL_REQUEST_TEMPLATE.md`:

   ```markdown
   ## Description
   
   [Describe your changes here]
   
   ## SAP Transport Requests
   
   [TRANSPORT:DEVK900123]
   
   ## Type of change
   
   - [ ] Bug fix
   - [ ] New feature
   - [ ] Enhancement
   - [ ] Breaking change
   
   ## Testing performed
   
   [Describe testing done]
   
   ## Screenshots (if appropriate)
   
   ## Checklist
   
   - [ ] My code follows SAP naming conventions
   - [ ] I have tested my changes in the development system
   - [ ] I have updated documentation as needed
   - [ ] My changes generate no new warnings
   - [ ] All existing tests passed
   ```

## Step 10: Test the Complete Workflow

Now it's time to test your GitHub Actions setup:

1. Create a feature branch in your repository
2. Make changes to ABAP code
3. Create a pull request with a transport request number
4. Verify that the syntax checking workflow runs
5. Verify that the transport validation workflow runs
6. Get the pull request approved and merge it
7. Verify that the transport release workflow runs
8. Use the manual quality gate workflow to promote the transport to production

## Validation

Your GitHub Actions setup is successful when:

- ABAP syntax is automatically checked on all PRs
- Transport requests are validated before merge
- Transports are automatically released to QAS when PRs are merged
- Production deployments require manual approval
- All workflows execute successfully without errors

## Troubleshooting

### Common Issues

| Issue | Solution |
|-------|----------|
| Workflow not triggering | Check the trigger events in workflow YAML |
| Transport extraction failing | Verify PR template format and extraction script |
| Authentication to SAP failing | Check secret values and service user permissions |
| Custom actions not found | Ensure custom actions are properly created and referenced |
| Missing required status checks | Make sure branch protection requires the correct checks |

### Advanced Troubleshooting

If you encounter persistent issues:

1. Check workflow run logs in the "Actions" tab
2. Enable debug logging by setting the secret `ACTIONS_RUNNER_DEBUG` to `true`
3. Verify network connectivity between GitHub Actions and SAP systems
4. Test SAP connections separately to isolate authentication issues

## ➡️ Next Steps

Now that you've set up basic GitHub Actions workflows, consider:

- [Working with Feature Branches](../development/feature-branches.md) for effective development processes
- [Managing Pull Requests](../development/pull-requests.md) for streamlined reviews
- [Automating Transport Creation](../transport/automated-creation.md) for end-to-end automation
- [Custom Action Development](../advanced/custom-actions.md) for SAP-specific needs

## Additional Resources

- [GitHub Actions Documentation](https://docs.github.com/en/actions)
- [abaplint Documentation](https://abaplint.org/)
- [SAP Transport Management](../../implementation/sap-setup/transport-layer.md)
- [CI/CD Best Practices](../../reference/workflows-reference/cicd-best-practices.md)

---

*Last Updated: [YYYY-MM-DD] for [GitHub Enterprise X.Y.Z, SAP NetWeaver X.Y.Z]* 
---

<details>
<summary><strong>📊 Document Metadata</strong></summary>

- **Last Updated:** 2025-04-07
- **Version:** 1.0.0
- **Status:** Published
</details>
