# 📄 SAP Transport Automation with GitHub Integration

## 📋 Table of Contents

- [📋 Overview](#overview)
- [✅ Prerequisites](#prerequisites)
- [Basic Transport Automation](#basic-transport-automation)
- [Advanced Transport Automation](#advanced-transport-automation)
- [Integration with SAP Solution Manager](#integration-with-sap-solution-manager)
- [Transport Status Dashboard](#transport-status-dashboard)
- [SAP Code Deployment Best Practices](#sap-code-deployment-best-practices)
- [Troubleshooting Transport Automation](#troubleshooting-transport-automation)
- [Automating Transport Documentation](#automating-transport-documentation)
- [Best Practices](#best-practices)
- [➡
️ ️ Next Steps](#next-steps)
- [Additional Resources](#additional-resources)


This guide provides detailed instructions for setting up automated transport management that integrates SAP's transport system with GitHub workflows, creating a seamless CI/CD pipeline.

## 📋 Overview

Transport automation is a crucial component of the SAP-GitHub integration, enabling:

- Automated promotion of changes through SAP landscapes
- Synchronization between GitHub PR status and SAP transport status
- Validation of transports before import
- Auditable deployment processes with approval gates
- Reduced manual effort and human error in transport management

## ✅ Prerequisites

Before implementing transport automation:

- Completed [CI/CD workflow configuration](ci-cd-setup.md)
- [SAP transport layer](../sap-setup/transport-layer.md) properly configured
- [GitHub authentication](../github-setup/authentication.md) set up
- Service users with appropriate authorizations in SAP
- GitHub Actions runner with network access to SAP systems

## Basic Transport Automation

### Linking GitHub PRs to SAP Transports

1. Configure PR templates to include transport numbers:
   
   Create a file at `.github/PULL_REQUEST_TEMPLATE.md`:
   
   ```markdown
   ## Description
   
   [Describe the changes]
   
   ## Associated Transport Request(s)
   
   [TRANSPORT:DEVK900123]
   
   ## Testing
   
   [Describe testing performed]
   ```

2. Create a workflow to validate the transport during PR:

   ```yaml
   name: SAP Transport Validation
   
   on:
     pull_request:
       types: [opened, edited, synchronize, reopened]
   
   jobs:
     validate:
       runs-on: ubuntu-latest
       steps:
         - uses: actions/checkout@v3
         
         - name: Extract transport numbers
           id: extract
           run: |
             TRANSPORTS=$(grep -o '\[TRANSPORT:[A-Z0-9]*\]' "${{ github.event.pull_request.body }}" | cut -d':' -f2 | cut -d']' -f1)
             echo "transports=$TRANSPORTS" >> $GITHUB_OUTPUT
         
         - name: Validate transport exists
           if: steps.extract.outputs.transports != ''
           uses: your-org/sap-transport-validator@v1
           with:
             sap-host: ${{ secrets.SAP_HOST }}
             sap-client: ${{ secrets.SAP_CLIENT }}
             sap-user: ${{ secrets.SAP_SERVICE_USER }}
             sap-password: ${{ secrets.SAP_SERVICE_PASSWORD }}
             transport-numbers: ${{ steps.extract.outputs.transports }}
   ```

### Automating Transport Release

Create a workflow that automatically releases transports to QAS when a PR is merged:

```yaml
name: Release Transport to QAS

on:
  pull_request:
    types: [closed]
    branches: [main]

jobs:
  release:
    if: github.event.pull_request.merged == true
    runs-on: ubuntu-latest
    steps:
      - name: Extract transport numbers
        id: extract
        run: |
          TRANSPORTS=$(grep -o '\[TRANSPORT:[A-Z0-9]*\]' "${{ github.event.pull_request.body }}" | cut -d':' -f2 | cut -d']' -f1)
          echo "transports=$TRANSPORTS" >> $GITHUB_OUTPUT
      
      - name: Release transport to QAS
        if: steps.extract.outputs.transports != ''
        uses: your-org/sap-transport-releaser@v1
        with:
          sap-host: ${{ secrets.SAP_HOST }}
          sap-client: ${{ secrets.SAP_CLIENT }}
          sap-user: ${{ secrets.SAP_SERVICE_USER }}
          sap-password: ${{ secrets.SAP_SERVICE_PASSWORD }}
          transport-numbers: ${{ steps.extract.outputs.transports }}
          target-system: QAS
          
      - name: Add comment with status
        if: steps.extract.outputs.transports != ''
        uses: actions/github-script@v6
        with:
          github-token: ${{ secrets.GITHUB_TOKEN }}
          script: |
            github.rest.issues.createComment({
              issue_number: context.issue.number,
              owner: context.repo.owner,
              repo: context.repo.repo,
              body: `Transport(s) ${{ steps.extract.outputs.transports }} released to QAS system.`
            })
```

## Advanced Transport Automation

### Multi-System Transport Management

For managing transports across the entire landscape (DEV → QAS → PRD):

```yaml
name: SAP Transport Promotion Pipeline

on:
  workflow_dispatch:
    inputs:
      transport_numbers:
        description: 'Transport request numbers (comma-separated)'
        required: true
      target_system:
        description: 'Target system for promotion'
        required: true
        type: choice
        options:
          - QAS
          - PRD

jobs:
  validate:
    runs-on: ubuntu-latest
    steps:
      - name: Validate transports
        uses: your-org/sap-transport-validator@v1
        with:
          sap-host: ${{ secrets.SAP_HOST }}
          sap-client: ${{ secrets.SAP_CLIENT }}
          sap-user: ${{ secrets.SAP_SERVICE_USER }}
          sap-password: ${{ secrets.SAP_SERVICE_PASSWORD }}
          transport-numbers: ${{ github.event.inputs.transport_numbers }}
          
  promote-to-qa:
    needs: validate
    if: github.event.inputs.target_system == 'QAS'
    environment: QA  # Requires approval
    runs-on: ubuntu-latest
    steps:
      - name: Release to QA
        uses: your-org/sap-transport-releaser@v1
        with:
          sap-host: ${{ secrets.SAP_HOST }}
          sap-client: ${{ secrets.SAP_CLIENT }}
          sap-user: ${{ secrets.SAP_SERVICE_USER }}
          sap-password: ${{ secrets.SAP_SERVICE_PASSWORD }}
          transport-numbers: ${{ github.event.inputs.transport_numbers }}
          target-system: QAS
          
  promote-to-prod:
    needs: validate
    if: github.event.inputs.target_system == 'PRD'
    environment: Production  # Requires approval
    runs-on: ubuntu-latest
    steps:
      - name: Release to Production
        uses: your-org/sap-transport-releaser@v1
        with:
          sap-host: ${{ secrets.SAP_HOST }}
          sap-client: ${{ secrets.SAP_CLIENT }}
          sap-user: ${{ secrets.SAP_SERVICE_USER }}
          sap-password: ${{ secrets.SAP_SERVICE_PASSWORD }}
          transport-numbers: ${{ github.event.inputs.transport_numbers }}
          target-system: PRD
```

### Automated Import Queue Management

Set up scheduled workflows to manage import queues:

```yaml
name: SAP Import Queue Management

on:
  schedule:
    - cron: '0 */2 * * *'  # Every 2 hours
  workflow_dispatch:

jobs:
  manage-import-queues:
    runs-on: ubuntu-latest
    steps:
      - name: Manage QAS import queue
        uses: your-org/sap-import-queue-manager@v1
        with:
          sap-host: ${{ secrets.SAP_HOST }}
          sap-client: ${{ secrets.SAP_CLIENT }}
          sap-user: ${{ secrets.SAP_SERVICE_USER }}
          sap-password: ${{ secrets.SAP_SERVICE_PASSWORD }}
          system: QAS
          auto-import: true
          maximum-age-hours: 4
          
      - name: Manage PRD import queue
        uses: your-org/sap-import-queue-manager@v1
        with:
          sap-host: ${{ secrets.SAP_HOST }}
          sap-client: ${{ secrets.SAP_CLIENT }}
          sap-user: ${{ secrets.SAP_SERVICE_USER }}
          sap-password: ${{ secrets.SAP_SERVICE_PASSWORD }}
          system: PRD
          auto-import: false  # Only report, don't auto-import in production
          maximum-age-hours: 24
          notification-email: ${{ secrets.ADMIN_EMAIL }}
```

### Transport Dependency Management

Create a workflow that handles transport dependencies:

```yaml
name: SAP Transport Dependency Check

on:
  pull_request:
    types: [opened, edited, synchronize, reopened]

jobs:
  check-dependencies:
    runs-on: ubuntu-latest
    steps:
      - name: Extract transport numbers
        id: extract
        run: |
          TRANSPORTS=$(grep -o '\[TRANSPORT:[A-Z0-9]*\]' "${{ github.event.pull_request.body }}" | cut -d':' -f2 | cut -d']' -f1)
          echo "transports=$TRANSPORTS" >> $GITHUB_OUTPUT
          
      - name: Check transport dependencies
        if: steps.extract.outputs.transports != ''
        uses: your-org/sap-transport-dependency-checker@v1
        with:
          sap-host: ${{ secrets.SAP_HOST }}
          sap-client: ${{ secrets.SAP_CLIENT }}
          sap-user: ${{ secrets.SAP_SERVICE_USER }}
          sap-password: ${{ secrets.SAP_SERVICE_PASSWORD }}
          transport-numbers: ${{ steps.extract.outputs.transports }}
          
      - name: Add comment with dependencies
        if: steps.extract.outputs.transports != ''
        uses: actions/github-script@v6
        with:
          github-token: ${{ secrets.GITHUB_TOKEN }}
          script: |
            const fs = require('fs');
            const dependencies = JSON.parse(fs.readFileSync('dependencies.json', 'utf8'));
            
            let comment = '## Transport Dependencies\n\n';
            if (dependencies.length === 0) {
              comment += 'No dependencies found for this transport.\n';
            } else {
              comment += 'This transport depends on the following transports:\n\n';
              for (const dep of dependencies) {
                comment += `- ${dep.transportId}: ${dep.description} (Status: ${dep.status})\n`;
              }
              comment += '\nAll dependencies must be imported into target systems before this transport.';
            }
            
            github.rest.issues.createComment({
              issue_number: context.issue.number,
              owner: context.repo.owner,
              repo: context.repo.repo,
              body: comment
            });
```

## Integration with SAP Solution Manager

### ChaRM Integration Workflow

Set up integration with SAP Solution Manager Change Request Management:

```yaml
name: SAP ChaRM Integration

on:
  pull_request:
    types: [opened, edited, synchronize, reopened, closed]

jobs:
  update-charm:
    runs-on: ubuntu-latest
    steps:
      - name: Extract Change Document
        id: extract
        run: |
          CHANGE_DOC=$(grep -o 'CHARM:[0-9]*' "${{ github.event.pull_request.body }}" || echo "")
          CHANGE_ID=$(echo "$CHANGE_DOC" | cut -d':' -f2)
          echo "change_id=$CHANGE_ID" >> $GITHUB_OUTPUT
          
      - name: Update ChaRM document
        if: steps.extract.outputs.change_id != ''
        uses: your-org/sap-charm-updater@v1
        with:
          solman-url: ${{ secrets.SOLMAN_URL }}
          solman-user: ${{ secrets.SOLMAN_USER }}
          solman-password: ${{ secrets.SOLMAN_PASSWORD }}
          change-id: ${{ steps.extract.outputs.change_id }}
          github-event: ${{ github.event.action }}
          pr-number: ${{ github.event.pull_request.number }}
          pr-url: ${{ github.event.pull_request.html_url }}
          pr-state: ${{ github.event.pull_request.state }}
          pr-merged: ${{ github.event.pull_request.merged }}
```

## Transport Status Dashboard

### Creating a Transport Dashboard

1. Create a workflow to collect transport data:

```yaml
name: Update Transport Dashboard

on:
  schedule:
    - cron: '0 */6 * * *'  # Every 6 hours
  workflow_dispatch:

jobs:
  update-dashboard:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      
      - name: Collect transport status
        uses: your-org/sap-transport-status-collector@v1
        with:
          sap-host: ${{ secrets.SAP_HOST }}
          sap-client: ${{ secrets.SAP_CLIENT }}
          sap-user: ${{ secrets.SAP_SERVICE_USER }}
          sap-password: ${{ secrets.SAP_SERVICE_PASSWORD }}
          output-file: dashboard/data/transport-status.json
          days-back: 30
          
      - name: Commit and push if changed
        run: |
          git config --global user.name 'Transport Status Bot'
          git config --global user.email 'bot@example.com'
          git add dashboard/data/transport-status.json
          git diff --quiet && git diff --staged --quiet || git commit -m "Update transport status data [skip ci]"
          git push
```

2. Create a GitHub Pages site that displays the transport dashboard

## SAP Code Deployment Best Practices

### Safe Transport Release Strategy

1. **Phased Release Approach**:
   - Release related transports together
   - Schedule releases during low-usage windows
   - Implement rollback mechanisms

2. **Validation Before Import**:
   - Check transport content for critical objects
   - Validate object dependencies
   - Verify target system readiness

3. **Monitoring Post-Import**:
   - Check system logs after import
   - Run automated tests post-import
   - Monitor system performance metrics

### Transport Approval Workflow

Implement an approval-based transport workflow:

```yaml
name: Transport Approval Process

on:
  workflow_dispatch:
    inputs:
      transport_numbers:
        description: 'Transport request numbers'
        required: true

jobs:
  prepare-approval:
    runs-on: ubuntu-latest
    outputs:
      transport_info: ${{ steps.get-info.outputs.transport_info }}
    steps:
      - name: Get transport info
        id: get-info
        uses: your-org/sap-transport-info@v1
        with:
          sap-host: ${{ secrets.SAP_HOST }}
          sap-client: ${{ secrets.SAP_CLIENT }}
          sap-user: ${{ secrets.SAP_SERVICE_USER }}
          sap-password: ${{ secrets.SAP_SERVICE_PASSWORD }}
          transport-numbers: ${{ github.event.inputs.transport_numbers }}
          
  approve-qa:
    needs: prepare-approval
    environment: QA-Approval
    runs-on: ubuntu-latest
    steps:
      - name: Display transport info
        run: echo "Approving transport(s) for QA: ${{ github.event.inputs.transport_numbers }}"
        
      - name: Release to QA
        uses: your-org/sap-transport-releaser@v1
        with:
          sap-host: ${{ secrets.SAP_HOST }}
          sap-client: ${{ secrets.SAP_CLIENT }}
          sap-user: ${{ secrets.SAP_SERVICE_USER }}
          sap-password: ${{ secrets.SAP_SERVICE_PASSWORD }}
          transport-numbers: ${{ github.event.inputs.transport_numbers }}
          target-system: QAS
          
  approve-prod:
    needs: [prepare-approval, approve-qa]
    environment: Production-Approval
    runs-on: ubuntu-latest
    steps:
      - name: Display transport info
        run: echo "Approving transport(s) for Production: ${{ github.event.inputs.transport_numbers }}"
        
      - name: Release to Production
        uses: your-org/sap-transport-releaser@v1
        with:
          sap-host: ${{ secrets.SAP_HOST }}
          sap-client: ${{ secrets.SAP_CLIENT }}
          sap-user: ${{ secrets.SAP_SERVICE_USER }}
          sap-password: ${{ secrets.SAP_SERVICE_PASSWORD }}
          transport-numbers: ${{ github.event.inputs.transport_numbers }}
          target-system: PRD
```

## Troubleshooting Transport Automation

### Common Issues

| Issue | Resolution |
|-------|------------|
| Transport not found | Verify transport exists and is not already released |
| Import sequence errors | Check for transport dependencies and correct order |
| Transport locks | Check object locks in SAP and release if needed |
| Authorization errors | Verify service user has proper transport management authorizations |
| Transport content validation errors | Review transport content for invalid or incomplete objects |

### Debugging Transport Workflows

1. Enable verbose logging in transport actions:
   ```yaml
   steps:
     - name: Release transport with debugging
       uses: your-org/sap-transport-releaser@v1
       with:
         debug: true
         # Other parameters...
   ```

2. Implement logging to SAP application logs:
   - Log all transport operations to SAP application logs
   - Use transaction SLG1 to review detailed logs
   - Correlate GitHub workflow run IDs with SAP log entries

## Automating Transport Documentation

### Transport Manifest Generation

Create a workflow that generates transport documentation:

```yaml
name: Generate Transport Manifest

on:
  pull_request:
    types: [closed]
    branches: [main]

jobs:
  generate-manifest:
    if: github.event.pull_request.merged == true
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      
      - name: Extract transport numbers
        id: extract
        run: |
          TRANSPORTS=$(grep -o '\[TRANSPORT:[A-Z0-9]*\]' "${{ github.event.pull_request.body }}" | cut -d':' -f2 | cut -d']' -f1)
          echo "transports=$TRANSPORTS" >> $GITHUB_OUTPUT
          
      - name: Generate manifest
        if: steps.extract.outputs.transports != ''
        uses: your-org/sap-transport-manifest-generator@v1
        with:
          sap-host: ${{ secrets.SAP_HOST }}
          sap-client: ${{ secrets.SAP_CLIENT }}
          sap-user: ${{ secrets.SAP_SERVICE_USER }}
          sap-password: ${{ secrets.SAP_SERVICE_PASSWORD }}
          transport-numbers: ${{ steps.extract.outputs.transports }}
          output-file: docs/transport-manifests/${{ github.event.pull_request.number }}.md
          pr-url: ${{ github.event.pull_request.html_url }}
          pr-title: ${{ github.event.pull_request.title }}
          
      - name: Commit and push manifest
        run: |
          git config --global user.name 'Transport Bot'
          git config --global user.email 'transport-bot@example.com'
          git add docs/transport-manifests/
          git diff --quiet && git diff --staged --quiet || git commit -m "Add transport manifest for PR #${{ github.event.pull_request.number }} [skip ci]"
          git push
```

## Best Practices

### Transport Naming and Organization

1. **Naming Conventions**:
   - Use descriptive names for transports
   - Include feature/bug reference numbers
   - Consider prefix/suffix indicating GitHub integration

2. **Organization**:
   - Group related changes in single transports
   - Create separate transports for independent features
   - Use task transports for small changes, workbench for larger changes

### Compliance and Audit Support

1. **Transport Audit Trail**:
   - Log all transport operations
   - Document approvals and releases
   - Maintain history of transport status changes

2. **Regulatory Compliance**:
   - Implement segregation of duties
   - Enforce approval workflows for regulated systems
   - Generate compliance reports for audits

## ➡️ Next Steps

Once your transport automation is configured, proceed to:

- [Monitoring and Alerting Setup](../../operations/monitoring.md)
- [Advanced Workflow Configuration](advanced-workflows.md)
- [Developer Training on Transport Management](../../training/transport-management.md)

## Additional Resources

- [SAP Transport Management Documentation](https://help.sap.com/docs/basis)
- [GitHub Actions Documentation](https://docs.github.com/en/actions)
- [abapGit Transport Integration](https://docs.abapgit.org/ref-transport.html)

---

*Last Updated: [YYYY-MM-DD] for [GitHub Enterprise X.Y.Z, SAP NetWeaver X.Y.Z]* 
---


