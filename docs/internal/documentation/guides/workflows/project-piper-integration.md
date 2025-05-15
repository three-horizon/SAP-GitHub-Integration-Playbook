---
title: Project Piper Integration with GitHub Actions
description: Comprehensive guide for integrating Project Piper with GitHub Actions for SAP-specific CI/CD workflows
author: SAP-GitHub Integration Team
date: 2024-05-15
version: 1.0.0
---

# Project Piper Integration with GitHub Actions

## Table of Contents

- [Overview](#overview)
- [Prerequisites](#prerequisites)
- [Project Piper GitHub Action Setup](#project-piper-github-action-setup)
- [GitHub Actions Workflow Examples](#github-actions-workflow-examples)
  - [SAPUI5/Fiori Application Build](#sapui5fiori-application-build)
  - [SAP Cloud Application Programming Model (CAP)](#sap-cloud-application-programming-model-cap)
  - [SAP Cloud Integration](#sap-cloud-integration)
  - [MTA Archive Creation and Deployment](#mta-archive-creation-and-deployment)
- [Configuration Reference](#configuration-reference)
  - [Common Parameters](#common-parameters)
  - [Step-Specific Parameters](#step-specific-parameters)
  - [Secrets and Credentials](#secrets-and-credentials)
- [Integration Patterns](#integration-patterns)
  - [Combining with Other GitHub Actions](#combining-with-other-github-actions)
  - [Matrix Builds](#matrix-builds)
  - [Reusable Workflows](#reusable-workflows)
- [Monitoring and Troubleshooting](#monitoring-and-troubleshooting)
  - [Logs and Debugging](#logs-and-debugging)
  - [Common Issues](#common-issues)
  - [Performance Optimization](#performance-optimization)
- [Security Considerations](#security-considerations)
  - [Authentication](#authentication)
  - [Secret Management](#secret-management)
  - [Least Privilege Principle](#least-privilege-principle)
- [Best Practices](#best-practices)
- [Next Steps](#next-steps)
- [References](#references)

## Overview

Project Piper is SAP's opinionated CI/CD toolset that standardizes and simplifies the build and deployment processes for SAP development projects. This guide demonstrates how to integrate Project Piper with GitHub Actions to create powerful, automated workflows for SAP-specific scenarios.

### Key Benefits

- Standardized build and deployment processes for SAP applications
- Pre-configured steps for SAP-specific tasks
- Integration with SAP Cloud Platform and on-premises SAP systems
- Built-in quality gates and testing capabilities
- Simplified configuration through convention over configuration

## Prerequisites

Before implementing Project Piper with GitHub Actions, ensure you have:

- GitHub repository with your SAP project code
- GitHub Actions enabled for your repository
- Appropriate permissions to create and run workflows
- SAP BTP account (for cloud deployments)
- SAP-specific build tools configured:
  - Node.js 18.x or newer
  - Cloud Foundry CLI (for CAP/BTP deployments)
  - MTA Build Tool (for MTA projects)
  - SAP Cloud SDK (for Java/Node.js applications)

## Project Piper GitHub Action Setup

The `project-piper-action` allows you to run any Project Piper command within GitHub Actions. To use it:

1. Create a `.github/workflows` directory in your repository if it doesn't exist
2. Create a new workflow file (e.g., `sap-build.yml`)
3. Configure the workflow to use the Project Piper GitHub Action

Basic structure of a workflow file:

```yaml
name: SAP Build and Deploy

on:
  push:
    branches: [ main ]
  pull_request:
    branches: [ main ]

jobs:
  build:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      
      - name: Setup Node.js
        uses: actions/setup-node@v3
        with:
          node-version: '18'
          
      - name: Run Project Piper command
        uses: SAP/project-piper-action@v1
        with:
          command: npmExecuteScripts
          flags: --stage-config-url="https://raw.githubusercontent.com/SAP/jenkins-library/master/resources/default_pipeline_environment.yml"
```

## GitHub Actions Workflow Examples

### SAPUI5/Fiori Application Build

This workflow builds and deploys a SAPUI5/Fiori application to SAP BTP:

```yaml
name: SAPUI5/Fiori CI/CD

on:
  push:
    branches: [ main ]
  pull_request:
    branches: [ main ]

jobs:
  build-and-deploy:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      
      - name: Setup Node.js
        uses: actions/setup-node@v3
        with:
          node-version: '18'
          cache: 'npm'
          
      - name: Install dependencies
        run: npm ci
      
      - name: Build UI5 application
        uses: SAP/project-piper-action@v1
        with:
          command: npmExecuteScripts
          flags: --stage=build
          
      - name: Execute UI5 tests
        uses: SAP/project-piper-action@v1
        with:
          command: uiVeri5ExecuteTests
          
      - name: Deploy to SAP BTP
        if: github.event_name == 'push' && github.ref == 'refs/heads/main'
        uses: SAP/project-piper-action@v1
        with:
          command: cloudFoundryDeploy
          flags: --cf-api=${{ secrets.CF_API }} --cf-org=${{ secrets.CF_ORG }} --cf-space=${{ secrets.CF_SPACE }} --cf-username=${{ secrets.CF_USERNAME }} --cf-password=${{ secrets.CF_PASSWORD }}
```

### SAP Cloud Application Programming Model (CAP)

This workflow builds and deploys a CAP application with database migrations:

```yaml
name: CAP Application CI/CD

on:
  push:
    branches: [ main, dev ]
  pull_request:
    branches: [ main ]

jobs:
  build-and-deploy:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      
      - name: Setup Node.js
        uses: actions/setup-node@v3
        with:
          node-version: '18'
          cache: 'npm'
          
      - name: Install dependencies
        run: |
          npm ci
          npm i -g @sap/cds-dk
      
      - name: Build CAP application
        uses: SAP/project-piper-action@v1
        with:
          command: npmExecuteScripts
          flags: --stage=build
          
      - name: Execute CAP tests
        uses: SAP/project-piper-action@v1
        with:
          command: npmExecuteScripts
          flags: --stage=test
          
      - name: Build MTA
        uses: SAP/project-piper-action@v1
        with:
          command: mtaBuild
          flags: --mtar=app.mtar
      
      - name: Deploy to SAP BTP
        if: github.event_name == 'push' && (github.ref == 'refs/heads/main' || github.ref == 'refs/heads/dev')
        uses: SAP/project-piper-action@v1
        with:
          command: cloudFoundryDeploy
          flags: --cf-api=${{ secrets.CF_API }} --cf-org=${{ secrets.CF_ORG }} --cf-space=${{ secrets.CF_SPACE }} --cf-username=${{ secrets.CF_USERNAME }} --cf-password=${{ secrets.CF_PASSWORD }} --mtar=app.mtar
          
      - name: Run database migrations
        if: github.event_name == 'push' && github.ref == 'refs/heads/main'
        uses: SAP/project-piper-action@v1
        with:
          command: cloudFoundryCreateServiceKey
          flags: --cf-service=my-hana-db --cf-service-key=deploy-key --cf-api=${{ secrets.CF_API }} --cf-org=${{ secrets.CF_ORG }} --cf-space=${{ secrets.CF_SPACE }} --cf-username=${{ secrets.CF_USERNAME }} --cf-password=${{ secrets.CF_PASSWORD }}
      
      - name: Execute database migration
        if: github.event_name == 'push' && github.ref == 'refs/heads/main'
        run: cds deploy --to hana:deploy-key
```

### SAP Cloud Integration

This workflow manages SAP Cloud Integration content:

```yaml
name: SAP Cloud Integration CI/CD

on:
  push:
    branches: [ main ]
    paths:
      - 'integration-content/**'

jobs:
  deploy-integration:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      
      - name: Setup Project Piper environment
        uses: SAP/project-piper-action@v1
        with:
          command: setupCommonPipelineEnvironment
      
      - name: Deploy Integration Flow
        uses: SAP/project-piper-action@v1
        with:
          command: integrationArtifactDeploy
          flags: >
            --neo-url=${{ secrets.NEO_URL }}
            --username=${{ secrets.CPI_USERNAME }}
            --password=${{ secrets.CPI_PASSWORD }}
            --integration-flow-name="MyIntegrationFlow"
            --integration-flow-id="MyIntegrationFlow"
            --integration-flow-path="./integration-content/MyIntegrationFlow.zip"
      
      - name: Update Configuration
        uses: SAP/project-piper-action@v1
        with:
          command: integrationArtifactUpdateConfiguration
          flags: >
            --neo-url=${{ secrets.NEO_URL }}
            --username=${{ secrets.CPI_USERNAME }}
            --password=${{ secrets.CPI_PASSWORD }}
            --integration-flow-name="MyIntegrationFlow"
            --integration-flow-id="MyIntegrationFlow"
            --parameter-key="ENDPOINT_URL"
            --parameter-value="${{ secrets.ENDPOINT_URL }}"
```

### MTA Archive Creation and Deployment

This workflow builds and deploys a Multi-Target Application (MTA):

```yaml
name: MTA Build and Deploy

on:
  push:
    branches: [ main ]
  pull_request:
    branches: [ main ]

jobs:
  build-and-deploy:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      
      - name: Setup Node.js
        uses: actions/setup-node@v3
        with:
          node-version: '18'
          
      - name: Install MTA Build Tool
        run: npm install -g mbt
      
      - name: Initialize Project Piper
        uses: SAP/project-piper-action@v1
        with:
          command: setupCommonPipelineEnvironment
          
      - name: Build MTA
        uses: SAP/project-piper-action@v1
        with:
          command: mtaBuild
          flags: --mtar=application.mtar --platform=CF
          
      - name: Deploy to QAS
        if: github.event_name == 'push' && github.ref == 'refs/heads/main'
        uses: SAP/project-piper-action@v1
        with:
          command: cloudFoundryDeploy
          flags: >
            --cf-api=${{ secrets.CF_API_QAS }}
            --cf-org=${{ secrets.CF_ORG_QAS }}
            --cf-space=${{ secrets.CF_SPACE_QAS }}
            --cf-username=${{ secrets.CF_USERNAME }}
            --cf-password=${{ secrets.CF_PASSWORD }}
            --mtar=application.mtar
            --deployment-type=standard
            
      - name: Approval for PROD
        uses: trstringer/manual-approval@v1
        if: github.event_name == 'push' && github.ref == 'refs/heads/main'
        with:
          secret: ${{ github.TOKEN }}
          approvers: admin1,admin2
          minimum-approvals: 1
          issue-title: "Deploy to PROD"
          issue-body: "Please approve deployment to production"
          
      - name: Deploy to PROD
        if: github.event_name == 'push' && github.ref == 'refs/heads/main'
        uses: SAP/project-piper-action@v1
        with:
          command: cloudFoundryDeploy
          flags: >
            --cf-api=${{ secrets.CF_API_PROD }}
            --cf-org=${{ secrets.CF_ORG_PROD }}
            --cf-space=${{ secrets.CF_SPACE_PROD }}
            --cf-username=${{ secrets.CF_USERNAME }}
            --cf-password=${{ secrets.CF_PASSWORD }}
            --mtar=application.mtar
            --deployment-type=blue-green
```

## Configuration Reference

### Common Parameters

Project Piper actions require specific parameters depending on the command. Here are common parameters used across multiple commands:

| Parameter | Description | Required | Default |
|-----------|-------------|----------|---------|
| `command` | Project Piper command to execute | Yes | - |
| `flags` | Command-line flags for the command | No | - |
| `piper-version` | Version of Piper to use | No | Latest |
| `stage-config-url` | URL to the stage configuration | No | Default Piper environment |

### Step-Specific Parameters

Below are parameters for common Project Piper steps:

#### mtaBuild

```yaml
command: mtaBuild
flags: >
  --mtar=myapp.mtar
  --platform=CF
  --build-target=CF
  --mta-extension=./mta-test.mtaext
```

#### cloudFoundryDeploy

```yaml
command: cloudFoundryDeploy
flags: >
  --cf-api=https://api.cf.eu10.hana.ondemand.com
  --cf-org=myorg
  --cf-space=myspace
  --cf-username=${{ secrets.CF_USERNAME }}
  --cf-password=${{ secrets.CF_PASSWORD }}
  --mtar=myapp.mtar
  --deployment-type=standard
```

#### npmExecuteScripts

```yaml
command: npmExecuteScripts
flags: >
  --install
  --runScripts=build,test
  --build-descriptor=package.json
```

#### mavenExecute

```yaml
command: mavenExecute
flags: >
  --goals=clean,install
  --defines='-Dmaven.test.skip=true'
  --project-settings=./settings.xml
```

### Secrets and Credentials

Store sensitive information as GitHub Secrets:

| Secret | Description |
|--------|-------------|
| `CF_API` | Cloud Foundry API endpoint |
| `CF_ORG` | Cloud Foundry organization |
| `CF_SPACE` | Cloud Foundry space |
| `CF_USERNAME` | Cloud Foundry username |
| `CF_PASSWORD` | Cloud Foundry password |
| `NEO_URL` | SAP Neo environment URL |
| `CPI_USERNAME` | SAP Cloud Integration username |
| `CPI_PASSWORD` | SAP Cloud Integration password |
| `DOCKER_USERNAME` | Docker registry username |
| `DOCKER_PASSWORD` | Docker registry password |

## Integration Patterns

### Combining with Other GitHub Actions

Project Piper can be effectively combined with other GitHub Actions:

```yaml
jobs:
  build:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      
      - name: ABAP Test Automation
        uses: SAP-samples/abap-github-actions@v1
        with:
          adt-host: ${{ secrets.ADT_HOST }}
          adt-client: '100'
          adt-user: ${{ secrets.ADT_USER }}
          adt-password: ${{ secrets.ADT_PASSWORD }}
          
      - name: Setup Project Piper
        uses: SAP/project-piper-action@v1
        with:
          command: setupCommonPipelineEnvironment
          
      - name: Build MTA
        uses: SAP/project-piper-action@v1
        with:
          command: mtaBuild
          
      - name: Publish Test Results
        uses: EnricoMi/publish-unit-test-result-action@v2
        if: always()
        with:
          files: ./test-results/**/*.xml
```

### Matrix Builds

Use GitHub Actions matrix to run Piper across multiple environments:

```yaml
jobs:
  deploy:
    runs-on: ubuntu-latest
    strategy:
      matrix:
        environment: [dev, qas, prd]
        include:
          - environment: dev
            cf-api: ${{ secrets.CF_API_DEV }}
            cf-org: ${{ secrets.CF_ORG_DEV }}
            cf-space: ${{ secrets.CF_SPACE_DEV }}
          - environment: qas
            cf-api: ${{ secrets.CF_API_QAS }}
            cf-org: ${{ secrets.CF_ORG_QAS }}
            cf-space: ${{ secrets.CF_SPACE_QAS }}
          - environment: prd
            cf-api: ${{ secrets.CF_API_PRD }}
            cf-org: ${{ secrets.CF_ORG_PRD }}
            cf-space: ${{ secrets.CF_SPACE_PRD }}
            
    steps:
      - uses: actions/checkout@v3
      
      - name: Deploy to ${{ matrix.environment }}
        uses: SAP/project-piper-action@v1
        with:
          command: cloudFoundryDeploy
          flags: >
            --cf-api=${{ matrix.cf-api }}
            --cf-org=${{ matrix.cf-org }}
            --cf-space=${{ matrix.cf-space }}
            --cf-username=${{ secrets.CF_USERNAME }}
            --cf-password=${{ secrets.CF_PASSWORD }}
            --mtar=application.mtar
```

### Reusable Workflows

Create reusable workflows for common Piper patterns:

```yaml
# .github/workflows/reusable-mta-deploy.yml
name: Reusable MTA Deploy

on:
  workflow_call:
    inputs:
      mtar:
        required: true
        type: string
      environment:
        required: true
        type: string
    secrets:
      CF_API:
        required: true
      CF_ORG:
        required: true
      CF_SPACE:
        required: true
      CF_USERNAME:
        required: true
      CF_PASSWORD:
        required: true

jobs:
  deploy:
    runs-on: ubuntu-latest
    steps:
      - name: Deploy MTA
        uses: SAP/project-piper-action@v1
        with:
          command: cloudFoundryDeploy
          flags: >
            --cf-api=${{ secrets.CF_API }}
            --cf-org=${{ secrets.CF_ORG }}
            --cf-space=${{ secrets.CF_SPACE }}
            --cf-username=${{ secrets.CF_USERNAME }}
            --cf-password=${{ secrets.CF_PASSWORD }}
            --mtar=${{ inputs.mtar }}
```

Usage in another workflow:

```yaml
jobs:
  build:
    # Build job that creates the MTA
    
  deploy-dev:
    needs: build
    uses: ./.github/workflows/reusable-mta-deploy.yml
    with:
      mtar: application.mtar
      environment: dev
    secrets:
      CF_API: ${{ secrets.CF_API_DEV }}
      CF_ORG: ${{ secrets.CF_ORG_DEV }}
      CF_SPACE: ${{ secrets.CF_SPACE_DEV }}
      CF_USERNAME: ${{ secrets.CF_USERNAME }}
      CF_PASSWORD: ${{ secrets.CF_PASSWORD }}
```

## Monitoring and Troubleshooting

### Logs and Debugging

Enable detailed logging for Project Piper:

```yaml
steps:
  - name: Setup debug logging
    run: echo "ACTIONS_STEP_DEBUG=true" >> $GITHUB_ENV
    
  - name: Run Piper with debug
    uses: SAP/project-piper-action@v1
    with:
      command: mtaBuild
      flags: --verbose
```

### Common Issues

| Issue | Solution |
|-------|----------|
| Authentication failures | Verify secrets are correctly set and not expired |
| Missing tools | Ensure all required tools are installed in the workflow |
| MTA build failures | Check mtaext files for proper formatting and content |
| Deployment timeouts | Increase timeout settings for larger applications |
| Permission issues | Verify service accounts have proper roles in CF/BTP |

### Performance Optimization

- **Cache dependencies**: Use GitHub Actions caching for node_modules and Maven repositories
- **Skip tests conditionally**: Use conditions to skip tests in certain scenarios
- **Parallel jobs**: Split large workflows into parallel jobs
- **Store artifacts**: Cache build artifacts between jobs

```yaml
steps:
  - name: Cache Node modules
    uses: actions/cache@v3
    with:
      path: node_modules
      key: ${{ runner.os }}-node-${{ hashFiles('**/package-lock.json') }}
      
  - name: Cache Maven repository
    uses: actions/cache@v3
    with:
      path: ~/.m2/repository
      key: ${{ runner.os }}-maven-${{ hashFiles('**/pom.xml') }}
```

## Security Considerations

### Authentication

Secure authentication for SAP systems:

- Use GitHub Secrets for all credentials
- Implement token-based authentication where possible
- Rotate credentials regularly
- Use environment-specific service accounts

### Secret Management

Best practices for managing secrets:

- Never hardcode credentials in workflow files
- Use repository or organization secrets in GitHub
- Consider external secret management systems for enterprise scenarios
- Limit secret access to specific workflows or branches

### Least Privilege Principle

Implement least privilege access:

- Create service accounts with minimal permissions
- Use separate accounts for different environments
- Implement approval workflows for production deployments
- Audit access regularly

```yaml
# Example of approval workflow for production
steps:
  - name: Request approval
    uses: trstringer/manual-approval@v1
    with:
      secret: ${{ github.TOKEN }}
      approvers: user1,user2
      minimum-approvals: 2
      issue-title: "Approve production deployment"
```

## Best Practices

1. **Standardize configurations** across projects using `.pipeline/config.yml`
2. **Version your Piper configuration** in source control
3. **Create reusable workflows** for common patterns
4. **Implement proper environment separation** with different configurations
5. **Incorporate quality gates** with test and scan steps
6. **Document your pipelines** with clear comments and README files
7. **Monitor workflow performance** and optimize as needed
8. **Implement progressive deployment** strategies (dev → test → prod)
9. **Automate rollbacks** for failed deployments
10. **Use container-based builds** for consistency

## Next Steps

After implementing Project Piper with GitHub Actions, consider:

- [Setting up quality gates](static-analysis.md) with additional tools
- [Implementing ABAP unit tests](abap-unit.md) in your workflow
- [Configuring notifications](notifications.md) for pipeline results
- [Automating deployment approvals](advanced-workflows.md#approval-gates)
- [Integrating with SAP Solution Manager](../sap-setup/transport-automation.md)

## References

- [Project Piper Documentation](https://sap.github.io/jenkins-library/)
- [Project Piper GitHub Action](https://github.com/SAP/project-piper-action)
- [GitHub Actions Documentation](https://docs.github.com/en/actions)
- [SAP Cloud SDK Documentation](https://sap.github.io/cloud-sdk/)
- [BTP DevOps Guide](https://help.sap.com/docs/BTP/65de2977205c403bbc107264b8eccf4b/7c0f86a8fa2f4c5da4299cdf0d186130.html)

---

*Last Updated: 2024-05-15* 