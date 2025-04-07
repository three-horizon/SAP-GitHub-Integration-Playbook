# 📄 CI/CD Workflow Configuration for SAP-GitHub Integration

## 📋 Table of Contents

- [📋 Overview](#overview)
- [✅ Prerequisites](#prerequisites)
- [Basic CI Workflow Configuration](#basic-ci-workflow-configuration)
- [CD Workflow Configuration](#cd-workflow-configuration)
- [Advanced Configuration](#advanced-configuration)
- [Integration with SAP Transport Management](#integration-with-sap-transport-management)
- [Testing Your Workflows](#testing-your-workflows)
- [Troubleshooting](#troubleshooting)
- [Best Practices](#best-practices)
- [➡
️ ️ Next Steps](#next-steps)
- [Additional Resources](#additional-resources)


This guide provides detailed instructions for setting up Continuous Integration (CI) and Continuous Deployment (CD) workflows that bridge GitHub Enterprise with SAP systems.

## 📋 Overview

Effective CI/CD workflows for SAP development create a seamless integration between GitHub's collaborative development capabilities and SAP's transport management system. Well-configured workflows enable:

- Automated quality checks for ABAP code
- Validation of SAP transport requests
- Smooth progression of changes through your SAP landscape
- Comprehensive audit trails and compliance verification
- Consistent deployment practices across teams

## ✅ Prerequisites

Before configuring CI/CD workflows:

- [GitHub Enterprise authentication](../github-setup/authentication.md) configured
- [Branch protection rules](../github-setup/branch-protection.md) established
- [SAP transport layer](../sap-setup/transport-layer.md) properly set up
- GitHub Actions enabled in your GitHub Enterprise instance
- Service accounts with appropriate permissions in both GitHub and SAP

## Basic CI Workflow Configuration

### Setting Up ABAP Syntax Validation

Create a basic CI workflow that runs on every pull request:

1. In your repository, create the directory `.github/workflows/`
2. Create a file named `abap-lint.yml` with the following content:

```yaml
name: ABAP Syntax Validation

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
      
      - name: abaplint
        uses: abaplint/actions-abaplint@main
        env:
          GITHUB_TOKEN: ${{ secrets.GITHUB_TOKEN }}
          
      - name: Publish results
        if: always()
        uses: actions/github-script@v6
        with:
          script: |
            const fs = require('fs');
            const results = JSON.parse(fs.readFileSync('abaplint.results.json', 'utf8'));
            const issues = results.filter(issue => issue.severity === 'error');
            
            if (issues.length > 0) {
              core.setFailed(`Found ${issues.length} critical issues in ABAP code`);
            }
```

3. Create an `abaplint.json` configuration file in the repository root:

```json
{
  "global": {
    "files": "/src/**/*.*",
    "skipGeneratedGatewayClasses": true,
    "skipGeneratedPersistentClasses": true,
    "skipGeneratedFunctionGroups": true
  },
  "syntax": {
    "version": "v753",
    "errorNamespace": "^Z"
  },
  "rules": {
    "indentation": true,
    "begin_end_names": true,
    "check_syntax": true,
    "global_class": true,
    "obsolete_statement": true,
    "parser_error": true,
    "space_before_colon": true,
    "colon_missing_space": true,
    "exit_or_check": true,
    "line_length": {
      "length": 120
    },
    "empty_statement": true,
    "allowed_object_types": true
  }
}
```

### Setting Up Transport Request Validation

Create a workflow to validate SAP transport requests:

1. Create a file named `.github/workflows/transport-validation.yml`:

```yaml
name: SAP Transport Validation

on:
  pull_request:
    branches: [ main, develop ]
    
jobs:
  validate-transport:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      
      - name: Extract transport numbers
        id: extract-transports
        run: |
          TRANSPORTS=$(grep -o '\[TRANSPORT:[A-Z0-9]*\]' ${{ github.event.pull_request.body }} | cut -d':' -f2 | cut -d']' -f1)
          echo "transports=$TRANSPORTS" >> $GITHUB_OUTPUT
      
      - name: Validate transport requests
        if: steps.extract-transports.outputs.transports != ''
        uses: {your-org}/sap-transport-validator@v1
        with:
          sap-host: ${{ secrets.SAP_HOST }}
          sap-client: ${{ secrets.SAP_CLIENT }}
          sap-user: ${{ secrets.SAP_SERVICE_USER }}
          sap-password: ${{ secrets.SAP_SERVICE_PASSWORD }}
          transport-numbers: ${{ steps.extract-transports.outputs.transports }}
```

**Note**: The `sap-transport-validator` action should be created as a custom GitHub Action for your organization. See [Custom GitHub Actions](advanced-workflows.md#custom-github-actions) for details.

## CD Workflow Configuration

### Transport Release Automation

Create a workflow to automatically release transports when PRs are merged:

1. Create a file named `.github/workflows/transport-release.yml`:

```yaml
name: SAP Transport Release

on:
  push:
    branches: [ main ]

jobs:
  release-transport:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
        with:
          fetch-depth: 2
      
      - name: Extract transport numbers from merged PR
        id: extract-transports
        run: |
          PR_NUMBER=$(git log -1 --pretty=%s | grep -o '#[0-9]*' | cut -c 2-)
          if [ -n "$PR_NUMBER" ]; then
            PR_BODY=$(curl -s -H "Authorization: token ${{ secrets.GITHUB_TOKEN }}" \
                      "https://api.github.com/repos/${{ github.repository }}/pulls/$PR_NUMBER" | jq -r .body)
            TRANSPORTS=$(echo "$PR_BODY" | grep -o '\[TRANSPORT:[A-Z0-9]*\]' | cut -d':' -f2 | cut -d']' -f1)
            echo "transports=$TRANSPORTS" >> $GITHUB_OUTPUT
            echo "pr_number=$PR_NUMBER" >> $GITHUB_OUTPUT
          fi
      
      - name: Release transport requests
        if: steps.extract-transports.outputs.transports != ''
        uses: {your-org}/sap-transport-releaser@v1
        with:
          sap-host: ${{ secrets.SAP_HOST }}
          sap-client: ${{ secrets.SAP_CLIENT }}
          sap-user: ${{ secrets.SAP_SERVICE_USER }}
          sap-password: ${{ secrets.SAP_SERVICE_PASSWORD }}
          transport-numbers: ${{ steps.extract-transports.outputs.transports }}
          target-system: QAS
          
      - name: Update PR with transport status
        if: steps.extract-transports.outputs.pr_number != ''
        uses: actions/github-script@v6
        with:
          github-token: ${{ secrets.GITHUB_TOKEN }}
          script: |
            github.rest.issues.createComment({
              issue_number: ${{ steps.extract-transports.outputs.pr_number }},
              owner: context.repo.owner,
              repo: context.repo.repo,
              body: 'Transport requests ${{ steps.extract-transports.outputs.transports }} have been automatically released to QAS.'
            })
```

### Quality Gate Workflow

Create a workflow that runs quality checks before promoting to production:

1. Create a file named `.github/workflows/quality-gate.yml`:

```yaml
name: SAP Quality Gate

on:
  workflow_dispatch:
    inputs:
      transport_numbers:
        description: 'Comma-separated list of transport numbers to promote'
        required: true
      target_system:
        description: 'Target system for promotion (QAS or PRD)'
        required: true
        default: 'QAS'

jobs:
  quality-gate:
    runs-on: ubuntu-latest
    steps:
      - name: Validate inputs
        run: |
          if [[ "${{ github.event.inputs.target_system }}" != "QAS" && "${{ github.event.inputs.target_system }}" != "PRD" ]]; then
            echo "Invalid target system. Must be QAS or PRD."
            exit 1
          fi
      
      - name: Run SAP quality checks
        uses: {your-org}/sap-quality-gate@v1
        with:
          sap-host: ${{ secrets.SAP_HOST }}
          sap-client: ${{ secrets.SAP_CLIENT }}
          sap-user: ${{ secrets.SAP_SERVICE_USER }}
          sap-password: ${{ secrets.SAP_SERVICE_PASSWORD }}
          transport-numbers: ${{ github.event.inputs.transport_numbers }}
          target-system: ${{ github.event.inputs.target_system }}
          
      - name: Promote transports if quality checks pass
        if: success()
        uses: {your-org}/sap-transport-promoter@v1
        with:
          sap-host: ${{ secrets.SAP_HOST }}
          sap-client: ${{ secrets.SAP_CLIENT }}
          sap-user: ${{ secrets.SAP_SERVICE_USER }}
          sap-password: ${{ secrets.SAP_SERVICE_PASSWORD }}
          transport-numbers: ${{ github.event.inputs.transport_numbers }}
          target-system: ${{ github.event.inputs.target_system }}
```

## Advanced Configuration

### Setting Up Secrets and Environment Variables

Configure the necessary secrets in your GitHub repository:

1. Navigate to your repository's **Settings** → **Secrets and variables** → **Actions**
2. Click **New repository secret**
3. Add the following secrets:
   - `SAP_HOST`: The hostname of your SAP system
   - `SAP_CLIENT`: The SAP client number
   - `SAP_SERVICE_USER`: The service user for SAP integration
   - `SAP_SERVICE_PASSWORD`: The password for the service user
   - Additional secrets as needed for your environment

### Configuring Webhooks for Two-Way Integration

Set up webhooks to enable SAP to notify GitHub of events:

1. Create a webhook endpoint in your infrastructure to receive SAP notifications
2. Configure the webhook in GitHub:
   - Go to **Settings** → **Webhooks** → **Add webhook**
   - Set the payload URL to your webhook endpoint
   - Select content type: `application/json`
   - Choose which events to trigger the webhook
   - Set a secret for securing webhook payloads
3. Configure SAP to send notifications to this webhook (see [API Management Setup](../apim-setup/api-endpoints.md))

## Integration with SAP Transport Management

### Linking GitHub Workflows to SAP Transport Management

Create bidirectional integration between GitHub workflows and SAP transports:

1. Configure workflow triggers based on SAP transport events:
   - Create custom event types in GitHub
   - Set up webhook handlers to trigger workflows
2. Configure SAP transport operations triggered by GitHub events:
   - Use API calls from GitHub Actions to SAP API endpoints
   - Implement proper error handling and retry mechanisms

### Automated Import Queue Management

For advanced transport automation:

1. Create a scheduled workflow to manage SAP import queues:

```yaml
name: SAP Import Queue Management

on:
  schedule:
    - cron: '0 * * * *'  # Run hourly
  workflow_dispatch: {}  # Allow manual triggering

jobs:
  manage-import-queue:
    runs-on: ubuntu-latest
    steps:
      - name: Check import queue status
        uses: {your-org}/sap-import-queue-manager@v1
        with:
          sap-host: ${{ secrets.SAP_HOST }}
          sap-client: ${{ secrets.SAP_CLIENT }}
          sap-user: ${{ secrets.SAP_SERVICE_USER }}
          sap-password: ${{ secrets.SAP_SERVICE_PASSWORD }}
          systems: QAS,PRD
          max-queue-time: 120  # Minutes
```

## Testing Your Workflows

To verify your CI/CD configuration:

1. Create a test branch with a simple ABAP change
2. Create a pull request to your main branch
3. Verify that CI checks run automatically
4. Add a transport request number to the PR description
5. Verify that the transport validation workflow runs
6. Merge the PR and verify that the transport release workflow executes
7. Check that the transport is properly released in your SAP system

## Troubleshooting

### Common Issues

| Issue | Resolution |
|-------|------------|
| Workflow fails with connection error | Verify SAP system connectivity and VPN/firewall settings |
| Transport validation fails | Check transport status in SAP and ensure it exists and is valid |
| Missing transport number | Verify PR description format and extraction script |
| Action timeout | Increase timeout limits or optimize actions for performance |
| Permission denied | Check service user authorizations in SAP |

### Logs and Debugging

For workflow troubleshooting:

1. Check GitHub Actions run logs for detailed error messages
2. Review SAP transaction logs (SLG1) for errors on the SAP side
3. Enable debug logging in custom actions by setting secret `ACTIONS_RUNNER_DEBUG` to `true`
4. Test API calls independently using tools like Postman before integrating

## Best Practices

1. **Error Handling**:
   - Implement robust error handling in all workflows
   - Send notifications for failed workflows to responsible teams
   - Create retry mechanisms for transient errors

2. **Transport Management**:
   - Follow SAP best practices for transport sequencing
   - Implement checks for transport dependencies
   - Validate transport content before release

3. **Security**:
   - Regularly rotate service account credentials
   - Use least-privilege principles for service accounts
   - Implement IP restrictions for API access

4. **Documentation**:
   - Document workflow configurations in a central location
   - Create runbooks for common workflow issues
   - Maintain clear ownership of workflows

## ➡️ Next Steps

Once your CI/CD workflows are configured, proceed to:

- [Advanced Workflow Configuration](advanced-workflows.md)
- [Monitoring and Alerting Setup](../../operations/monitoring.md)
- [Developer Training](../../training/developer-guide.md)

## Additional Resources

- [GitHub Actions Documentation](https://docs.github.com/en/actions)
- [SAP Transport Management System Documentation](https://help.sap.com/docs/basis)
- [CI/CD Best Practices](../../best-practices/ci-cd.md)

---

*Last Updated: [YYYY-MM-DD] for [GitHub Enterprise X.Y.Z, SAP NetWeaver X.Y.Z]* 
---

<details>
<summary><strong>📊 Document Metadata</strong></summary>

- **Last Updated:** 2025-04-07
- **Version:** 1.0.0
- **Status:** Published
</details>
