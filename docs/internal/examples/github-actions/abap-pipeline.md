---
layout: default
title: GitHub Actions Workflow for ABAP
---

# 🔄 GitHub Actions Workflow for ABAP Development

<div align="center" class="svg-container">
  <!-- Using both object and img as fallback for maximum compatibility -->
  <object type="image/svg+xml" data="\1" style="width: 700px; max-width: 100%;" aria-label="\1">
    <img src="\1" alt="\1" width="700" />
  </object>
  
  *\1*
</div>

This example provides a complete GitHub Actions workflow for automating CI/CD processes with ABAP development, including code quality checks, testing, and transport management.

## 📋 Table of Contents

- [📋 Overview](#overview)
- [✅ Prerequisites](#prerequisites)
- [🔄 Workflow File](#workflow-file)
- [⚙️ Configuration Files](#configuration-files)
- [🔐 Required GitHub Secrets](#required-github-secrets)
- [📊 Workflow Explanation](#workflow-explanation)
- [✨ Best Practices](#best-practices)
- [🔍 Troubleshooting](#troubleshooting)
- [🔌 Extensions and Customizations](#extensions-and-customizations)
- [➡️ Next Steps](#next-steps)

## 📋 Overview

This GitHub Actions workflow helps automate the ABAP development lifecycle by connecting GitHub repositories with SAP ABAP systems. It includes steps for code quality analysis, testing, and integration with SAP transport management.

## ✅ Prerequisites

- GitHub repository with ABAP code (using abapGit or gCTS)
- SAP NetWeaver 7.50 or higher (or SAP BTP, ABAP Environment)
- abaplint configured for ABAP code quality analysis
- SAP system with appropriate authorizations
- Network connectivity between GitHub and SAP systems

## 🔄 Workflow File

Below is a complete GitHub Actions workflow file for ABAP development. Save this as `.github/workflows/abap-ci.yml` in your repository:

```yaml
name: ABAP CI/CD Pipeline

on:
  push:
    branches: [ main, develop ]
    paths:
      - 'src/**/*.abap'
      - '**/*.clas.abap'
      - '**/*.prog.abap'
      - '**/*.fugr.abap'
      - 'abaplint.json'
  pull_request:
    branches: [ main, develop ]
    paths:
      - 'src/**/*.abap'
      - '**/*.clas.abap'
      - '**/*.prog.abap'
      - '**/*.fugr.abap'
      - 'abaplint.json'
  workflow_dispatch:
    inputs:
      environment:
        description: 'Environment to deploy to'
        required: true
        default: 'dev'
        type: choice
        options:
          - dev
          - qa
          - prod

env:
  NODE_VERSION: 16
  TARGET_SYSTEM_DEV: DEV
  TARGET_SYSTEM_QA: QA
  TARGET_SYSTEM_PROD: PRD

jobs:
  lint:
    name: ABAP Lint
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      
      - name: Setup Node.js
        uses: actions/setup-node@v3
        with:
          node-version: ${{ env.NODE_VERSION }}
          cache: 'npm'
      
      - name: Install abaplint
        run: npm install @abaplint/cli -g
      
      - name: Run abaplint
        run: abaplint
      
      - name: Check abaplint results
        run: |
          if [ -f "abaplint.output.json" ]; then
            ERROR_COUNT=$(jq '.issues | length' abaplint.output.json)
            if [ $ERROR_COUNT -gt 0 ]; then
              echo "::error::Found $ERROR_COUNT linting errors"
              jq -r '.issues[] | "::error file=\(.filename),line=\(.start.row),col=\(.start.col)::\(.message)"' abaplint.output.json
              exit 1
            fi
          fi
      
      - name: Upload lint results
        if: always()
        uses: actions/upload-artifact@v3
        with:
          name: abaplint-results
          path: abaplint.output.json
          retention-days: 7

  syntax-check:
    name: ABAP Syntax Check
    needs: lint
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      
      - name: Setup Node.js
        uses: actions/setup-node@v3
        with:
          node-version: ${{ env.NODE_VERSION }}
          cache: 'npm'
      
      - name: Install ABAP remote syntax check
        run: npm install @sap/abap-syntax-check -g
      
      - name: Run syntax check against SAP system
        env:
          SAP_USER: ${{ secrets.SAP_USER }}
          SAP_PASSWORD: ${{ secrets.SAP_PASSWORD }}
          SAP_URL: ${{ secrets.SAP_URL_DEV }}
          SAP_CLIENT: ${{ secrets.SAP_CLIENT }}
        run: |
          abap-syntax-check \
            --user $SAP_USER \
            --password $SAP_PASSWORD \
            --url $SAP_URL \
            --client $SAP_CLIENT \
            --srcfolder ./src \
            --outputfile syntax-results.json
      
      - name: Check syntax results
        run: |
          if [ -f "syntax-results.json" ]; then
            ERROR_COUNT=$(jq '.issues | length' syntax-results.json)
            if [ $ERROR_COUNT -gt 0 ]; then
              echo "::error::Found $ERROR_COUNT syntax errors"
              jq -r '.issues[] | "::error file=\(.filename),line=\(.line)::\(.message)"' syntax-results.json
              exit 1
            fi
          fi
      
      - name: Upload syntax check results
        if: always()
        uses: actions/upload-artifact@v3
        with:
          name: syntax-check-results
          path: syntax-results.json
          retention-days: 7

  unit-tests:
    name: ABAP Unit Tests
    needs: syntax-check
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      
      - name: Setup Node.js
        uses: actions/setup-node@v3
        with:
          node-version: ${{ env.NODE_VERSION }}
          cache: 'npm'
      
      - name: Install ABAP test runner
        run: npm install @sap/abap-test-runner -g
      
      - name: Run ABAP unit tests
        env:
          SAP_USER: ${{ secrets.SAP_USER }}
          SAP_PASSWORD: ${{ secrets.SAP_PASSWORD }}
          SAP_URL: ${{ secrets.SAP_URL_DEV }}
          SAP_CLIENT: ${{ secrets.SAP_CLIENT }}
        run: |
          abap-test-runner \
            --user $SAP_USER \
            --password $SAP_PASSWORD \
            --url $SAP_URL \
            --client $SAP_CLIENT \
            --packages "ZGITHUB_DEMO" \
            --format junit \
            --outputfile test-results.xml
      
      - name: Upload test results
        if: always()
        uses: actions/upload-artifact@v3
        with:
          name: abap-test-results
          path: test-results.xml
          retention-days: 7
      
      - name: Publish test results
        if: always()
        uses: EnricoMi/publish-unit-test-result-action@v2
        with:
          files: test-results.xml
          check_name: ABAP Unit Test Results

  create-transport:
    name: Create Transport Request
    if: github.event_name == 'push' || github.event_name == 'workflow_dispatch'
    needs: unit-tests
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      
      - name: Setup Node.js
        uses: actions/setup-node@v3
        with:
          node-version: ${{ env.NODE_VERSION }}
          cache: 'npm'
      
      - name: Install SAP transport API client
        run: npm install @sap/transport-service-client -g
      
      - name: Create transport request
        id: create-transport
        env:
          SAP_USER: ${{ secrets.SAP_USER }}
          SAP_PASSWORD: ${{ secrets.SAP_PASSWORD }}
          SAP_URL: ${{ secrets.SAP_URL_DEV }}
          SAP_CLIENT: ${{ secrets.SAP_CLIENT }}
          COMMIT_SHA: ${{ github.sha }}
        run: |
          TRANSPORT_ID=$(sap-transport-service \
            --user $SAP_USER \
            --password $SAP_PASSWORD \
            --url $SAP_URL \
            --client $SAP_CLIENT \
            --operation create \
            --description "GitHub CI: $COMMIT_SHA" \
            --owner $SAP_USER)
          
          echo "transport_id=$TRANSPORT_ID" >> $GITHUB_OUTPUT
          echo "Transport request $TRANSPORT_ID created successfully"
      
      - name: Add objects to transport
        env:
          SAP_USER: ${{ secrets.SAP_USER }}
          SAP_PASSWORD: ${{ secrets.SAP_PASSWORD }}
          SAP_URL: ${{ secrets.SAP_URL_DEV }}
          SAP_CLIENT: ${{ secrets.SAP_CLIENT }}
          TRANSPORT_ID: ${{ steps.create-transport.outputs.transport_id }}
        run: |
          # Get list of changed ABAP objects from git
          CHANGED_FILES=$(git diff-tree --no-commit-id --name-only -r ${{ github.sha }} | grep '\.abap$')
          
          # Add objects to transport
          for FILE in $CHANGED_FILES; do
            OBJECT_NAME=$(basename $FILE .abap | cut -d. -f1)
            OBJECT_TYPE=$(basename $FILE | cut -d. -f2)
            
            case $OBJECT_TYPE in
              clas)
                TYPE="CLAS"
                ;;
              prog)
                TYPE="PROG"
                ;;
              fugr)
                TYPE="FUGR"
                ;;
              *)
                TYPE="PROG"
                ;;
            esac
            
            sap-transport-service \
              --user $SAP_USER \
              --password $SAP_PASSWORD \
              --url $SAP_URL \
              --client $SAP_CLIENT \
              --operation add-object \
              --transport $TRANSPORT_ID \
              --object-name $OBJECT_NAME \
              --object-type $TYPE
          done
      
      - name: Release transport
        env:
          SAP_USER: ${{ secrets.SAP_USER }}
          SAP_PASSWORD: ${{ secrets.SAP_PASSWORD }}
          SAP_URL: ${{ secrets.SAP_URL_DEV }}
          SAP_CLIENT: ${{ secrets.SAP_CLIENT }}
          TRANSPORT_ID: ${{ steps.create-transport.outputs.transport_id }}
        run: |
          sap-transport-service \
            --user $SAP_USER \
            --password $SAP_PASSWORD \
            --url $SAP_URL \
            --client $SAP_CLIENT \
            --operation release \
            --transport $TRANSPORT_ID

  deploy-to-qa:
    name: Deploy to QA
    if: (github.event_name == 'push' && github.ref == 'refs/heads/main') || (github.event_name == 'workflow_dispatch' && github.event.inputs.environment == 'qa')
    needs: create-transport
    runs-on: ubuntu-latest
    environment: qa
    steps:
      - uses: actions/checkout@v3
      
      - name: Setup Node.js
        uses: actions/setup-node@v3
        with:
          node-version: ${{ env.NODE_VERSION }}
          cache: 'npm'
      
      - name: Install SAP transport API client
        run: npm install @sap/transport-service-client -g
      
      - name: Import transport to QA system
        env:
          SAP_USER: ${{ secrets.SAP_USER }}
          SAP_PASSWORD: ${{ secrets.SAP_PASSWORD }}
          SAP_URL: ${{ secrets.SAP_URL_QA }}
          SAP_CLIENT: ${{ secrets.SAP_CLIENT }}
          TRANSPORT_ID: ${{ needs.create-transport.outputs.transport_id }}
        run: |
          sap-transport-service \
            --user $SAP_USER \
            --password $SAP_PASSWORD \
            --url $SAP_URL \
            --client $SAP_CLIENT \
            --operation import \
            --transport $TRANSPORT_ID \
            --target-system ${{ env.TARGET_SYSTEM_QA }}
      
      - name: Run integration tests
        env:
          SAP_USER: ${{ secrets.SAP_USER }}
          SAP_PASSWORD: ${{ secrets.SAP_PASSWORD }}
          SAP_URL: ${{ secrets.SAP_URL_QA }}
          SAP_CLIENT: ${{ secrets.SAP_CLIENT }}
        run: |
          abap-test-runner \
            --user $SAP_USER \
            --password $SAP_PASSWORD \
            --url $SAP_URL \
            --client $SAP_CLIENT \
            --packages "ZGITHUB_DEMO" \
            --format junit \
            --include-pattern "ZCL_*_ITEST" \
            --outputfile integration-test-results.xml
      
      - name: Upload integration test results
        if: always()
        uses: actions/upload-artifact@v3
        with:
          name: integration-test-results
          path: integration-test-results.xml
          retention-days: 7

  deploy-to-prod:
    name: Deploy to Production
    if: github.event_name == 'workflow_dispatch' && github.event.inputs.environment == 'prod'
    needs: deploy-to-qa
    runs-on: ubuntu-latest
    environment: production
    steps:
      - uses: actions/checkout@v3
      
      - name: Setup Node.js
        uses: actions/setup-node@v3
        with:
          node-version: ${{ env.NODE_VERSION }}
          cache: 'npm'
      
      - name: Install SAP transport API client
        run: npm install @sap/transport-service-client -g
      
      - name: Import transport to Production system
        env:
          SAP_USER: ${{ secrets.SAP_USER }}
          SAP_PASSWORD: ${{ secrets.SAP_PASSWORD }}
          SAP_URL: ${{ secrets.SAP_URL_PROD }}
          SAP_CLIENT: ${{ secrets.SAP_CLIENT }}
          TRANSPORT_ID: ${{ needs.create-transport.outputs.transport_id }}
        run: |
          sap-transport-service \
            --user $SAP_USER \
            --password $SAP_PASSWORD \
            --url $SAP_URL \
            --client $SAP_CLIENT \
            --operation import \
            --transport $TRANSPORT_ID \
            --target-system ${{ env.TARGET_SYSTEM_PROD }}
      
      - name: Verify deployment
        env:
          SAP_USER: ${{ secrets.SAP_USER }}
          SAP_PASSWORD: ${{ secrets.SAP_PASSWORD }}
          SAP_URL: ${{ secrets.SAP_URL_PROD }}
          SAP_CLIENT: ${{ secrets.SAP_CLIENT }}
        run: |
          # Check transport status
          IMPORT_STATUS=$(sap-transport-service \
            --user $SAP_USER \
            --password $SAP_PASSWORD \
            --url $SAP_URL \
            --client $SAP_CLIENT \
            --operation check-status \
            --transport $TRANSPORT_ID \
            --target-system ${{ env.TARGET_SYSTEM_PROD }})
          
          if [[ "$IMPORT_STATUS" != *"Successfully imported"* ]]; then
            echo "::error::Transport import failed: $IMPORT_STATUS"
            exit 1
          fi
      
      - name: Send deployment notification
        if: success()
        uses: slackapi/slack-github-action@v1.24.0
        with:
          payload: |
            {
              "text": "🚀 ABAP Deployment to Production Successful!",
              "blocks": [
                {
                  "type": "header",
                  "text": {
                    "type": "plain_text",
                    "text": "🚀 ABAP Deployment to Production Successful!"
                  }
                },
                {
                  "type": "section",
                  "fields": [
                    {
                      "type": "mrkdwn",
                      "text": "*Repository:*\n${{ github.repository }}"
                    },
                    {
                      "type": "mrkdwn",
                      "text": "*Commit:*\n${{ github.sha }}"
                    },
                    {
                      "type": "mrkdwn",
                      "text": "*Transport:*\n${{ needs.create-transport.outputs.transport_id }}"
                    }
                  ]
                }
              ]
            }
        env:
          SLACK_WEBHOOK_URL: ${{ secrets.SLACK_WEBHOOK_URL }}
          SLACK_WEBHOOK_TYPE: INCOMING_WEBHOOK
```

## ⚙️ Configuration Files

### abaplint.json

```json
{
  "global": {
    "files": "/**/*.abap",
    "skipGeneratedGatewayClasses": true,
    "skipGeneratedPersistentClasses": true,
    "skipGeneratedFunctionGroups": true
  },
  "syntax": {
    "version": "v753",
    "errorNamespace": "^Z",
    "globalConstants": [],
    "globalMacros": []
  },
  "rules": {
    "7bit_ascii": true,
    "avoid_use": {
      "define": true,
      "endselect": true,
      "execSQL": true,
      "kernelCall": true,
      "communication": true,
      "statics": true,
      "systemCall": true,
      "defaultKey": true,
      "break": true
    },
    "breakpoint": true,
    "begin_end_names": true,
    "check_ddic": true,
    "check_include": true,
    "check_syntax": true,
    "global_class": true,
    "definitions_top": true,
    "double_space": false,
    "empty_line_in_statement": true,
    "empty_statement": true,
    "exit_or_check": true,
    "exporting": true,
    "functional_writing": true,
    "indentation": true,
    "inline_data_old_versions": true,
    "line_length": {
      "length": 120
    },
    "line_only_punc": true,
    "local_class_naming": {
      "local": "^LCL_",
      "test": "^LTCL_"
    },
    "local_variable_names": {
      "local": "^L",
      "parameter": "^I|^C|^P|^T",
      "field": "^M",
      "constant": "^C"
    },
    "max_one_statement": true,
    "message_exists": true,
    "method_length": {
      "statements": 100
    },
    "method_parameter_names": {
      "importing": "^I",
      "returning": "^R",
      "changing": "^C",
      "exporting": "^E",
      "exception": "^X"
    },
    "mix_returning": true,
    "msag_consistency": true,
    "nesting": {
      "depth": 5
    },
    "no_public_attributes": true,
    "object_naming": {
      "clas": "^ZC",
      "intf": "^ZI",
      "prog": "^Z",
      "fugr": "^Z",
      "doma": "^Z",
      "dtel": "^Z",
      "tabl": "^Z",
      "view": "^Z"
    },
    "obsolete_statement": {
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
    "preferred_compare_operator": {
      "badOperators": [
        "EQ",
        "NE",
        "LT",
        "GT",
        "LE",
        "GE"
      ]
    },
    "remove_descriptions": true,
    "sequential_blank": {
      "lines": 4
    },
    "short_case": {
      "length": 1,
      "allow": []
    },
    "space_before_colon": true,
    "start_at_tab": true,
    "superclass_final": true,
    "tabl_enhancement_category": true,
    "type_form_parameters": true,
    "unreachable_code": true,
    "use_new": true,
    "when_others_last": true,
    "whitespace_end": true
  }
}
```

## 🔐 Required GitHub Secrets

Configure the following secrets in your GitHub repository settings:

| Secret Name | Description |
|-------------|-------------|
| `SAP_USER` | SAP system username with appropriate authorizations |
| `SAP_PASSWORD` | SAP system password |
| `SAP_URL_DEV` | URL for the development SAP system |
| `SAP_URL_QA` | URL for the QA SAP system |
| `SAP_URL_PROD` | URL for the production SAP system |
| `SAP_CLIENT` | SAP client number |
| `SLACK_WEBHOOK_URL` | Slack webhook URL for notifications (optional) |

## 📊 Workflow Explanation

### 1. ABAP Linting

The workflow starts with linting ABAP code using abaplint:

- **Static code analysis**: Checks for ABAP coding style and best practices
- **Quality gates**: Fails the workflow if critical issues are found
- **Artifact generation**: Stores lint results for review

### 2. ABAP Syntax Check

After linting, the workflow performs syntax checking against a real SAP system:

- **Remote syntax check**: Validates code against the SAP ABAP compiler
- **Early error detection**: Identifies syntax errors before committing to the system
- **Integration verification**: Ensures compatibility with the target SAP system

### 3. ABAP Unit Tests

The workflow then runs unit tests:

- **Automated testing**: Executes ABAP unit tests in the development system
- **Test results**: Publishes test reports for visibility
- **Quality gate**: Stops the pipeline if tests fail

### 4. Transport Management

For qualified code, the workflow creates and manages transports:

- **Transport creation**: Creates a transport request in the SAP system
- **Object assignment**: Adds changed objects to the transport request
- **Transport release**: Releases the transport for importing to other systems

### 5. Deployment

The workflow includes deployment to QA and production environments with appropriate approvals:

- **QA deployment**: Automatically deploys to QA from the main branch
- **Production deployment**: Manual trigger with approval gate
- **Verification**: Confirms successful deployment
- **Notifications**: Sends notifications upon successful production deployment

## ✨ Best Practices

### Security

1. **Credential Management**
   - Store all SAP credentials securely in GitHub Secrets
   - Use service accounts with minimal privileges
   - Rotate credentials regularly

2. **Environment Protection**
   - Implement approval gates for production deployments
   - Use GitHub Environments for access controls
   - Limit who can trigger production deployments

### Pipeline Optimization

1. **Efficient Testing**
   - Run fast tests early in the pipeline
   - Use test selectors to run only relevant tests
   - Implement test categorization (unit, integration, etc.)

2. **Parallelization**
   - Run independent steps in parallel where possible
   - Use matrix builds for testing across different configurations
   - Optimize build time with caching

### Change Management

1. **Transport Strategy**
   - Link transports to GitHub Pull Requests
   - Implement transport naming conventions that reference GitHub
   - Track deployments in both GitHub and SAP systems

2. **Release Notes**
   - Generate automated release notes from commits
   - Document transport numbers in releases
   - Maintain change history across systems

## 🔍 Troubleshooting

### Common Issues and Solutions

1. **Authentication Failures**
   - **Problem**: SAP system connection fails due to authentication issues
   - **Solution**: Verify credentials are correctly stored in GitHub Secrets

2. **Transport Creation Failures**
   - **Problem**: Unable to create transport request
   - **Solution**: Check if the SAP user has appropriate authorizations

3. **Test Failures**
   - **Problem**: ABAP unit tests fail unexpectedly
   - **Solution**: Examine test logs, ensure test environment is properly set up

4. **Deployment Issues**
   - **Problem**: Transport import fails in target system
   - **Solution**: Check for prerequisite transports, verify system availability

## 🔌 Extensions and Customizations

### Adding Custom Code Metrics

```yaml
# Add this job after the 'lint' job
metrics:
  name: ABAP Code Metrics
  runs-on: ubuntu-latest
  steps:
    - uses: actions/checkout@v3
    
    - name: Setup Node.js
      uses: actions/setup-node@v3
      with:
        node-version: ${{ env.NODE_VERSION }}
    
    - name: Install ABAP metrics tool
      run: npm install @abap/metrics-cli -g
    
    - name: Generate metrics report
      run: |
        abap-metrics \
          --input ./src \
          --output metrics-report.json \
          --format json
    
    - name: Upload metrics report
      uses: actions/upload-artifact@v3
      with:
        name: abap-metrics
        path: metrics-report.json
        retention-days: 90
```

### Integration with Solution Manager

```yaml
# Add this step to the 'create-transport' job
- name: Update Solution Manager
  env:
    SOLMAN_URL: ${{ secrets.SOLMAN_URL }}
    SOLMAN_USER: ${{ secrets.SOLMAN_USER }}
    SOLMAN_PASSWORD: ${{ secrets.SOLMAN_PASSWORD }}
    TRANSPORT_ID: ${{ steps.create-transport.outputs.transport_id }}
    CHANGE_ID: ${{ vars.SOLMAN_CHANGE_ID }}
  run: |
    curl -X POST \
      -u "${SOLMAN_USER}:${SOLMAN_PASSWORD}" \
      -H "Content-Type: application/json" \
      -d "{\"transportId\": \"${TRANSPORT_ID}\", \"changeId\": \"${CHANGE_ID}\", \"commitId\": \"${{ github.sha }}\"}" \
      "${SOLMAN_URL}/api/v1/changes/update-transport"
```

## ➡️ Next Steps

- Integrate with [SAP Solution Manager](../xml-config/system-connectivity.md) for comprehensive change management
- Implement [API Management](../api-management/sap-to-github.md) for secure system connectivity
- Configure [Security Settings](../security-config/oauth-configuration.md) for enhanced authentication

---


