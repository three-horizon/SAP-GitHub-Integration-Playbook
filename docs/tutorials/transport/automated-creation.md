# Automating Transport Creation with GitHub Actions

This tutorial provides detailed guidance on how to automate SAP transport creation and management using GitHub Actions, making your development process more efficient and reliable.

## Overview

SAP transport requests are a critical component of SAP development, managing the movement of changes through your system landscape. Automating transport creation and management provides several benefits:

- Ensures consistent transport naming and categorization
- Creates tight integration between GitHub commits and SAP transports
- Reduces manual effort and potential for human error
- Provides better traceability between requirements, code, and transports
- Enables automated testing and release processes

This tutorial walks you through implementing transport automation using GitHub Actions and custom integration components.

## Prerequisites

Before implementing transport automation, ensure you have:

- [ ] A GitHub repository set up for SAP development (see [Setting Up Your First SAP-GitHub Repository](../basic/first-repository.md))
- [ ] GitHub Actions workflows established (see [Setting Up GitHub Actions Workflows](../basic/github-actions-setup.md))
- [ ] SAP transport layer properly configured (see [SAP Transport Layer Configuration](../../implementation/sap-setup/transport-layer.md))
- [ ] API integration between GitHub and SAP (typically via Microsoft APIM)
- [ ] Service users with appropriate authorizations in SAP
- [ ] Custom RFC-enabled function modules or REST APIs in SAP for transport management

## Step 1: Understanding the Automated Transport Workflow

Before implementation, familiarize yourself with the typical workflow:

1. Developer creates a feature branch in GitHub
2. Automation creates a corresponding SAP transport request
3. Developer links the transport to their feature branch
4. Developer implements changes in SAP using this transport
5. Changes are pushed to GitHub with transport reference
6. When PR is merged, the transport is automatically released to QAS
7. After testing in QAS, the transport is released to PRD

This integration creates a seamless connection between GitHub development processes and SAP transport management.

## Step 2: Creating Required SAP Components

Implement the necessary custom components in SAP:

1. **Create a Z-class for transport management**:

   ```abap
   CLASS zcl_github_transport_mgmt DEFINITION
     PUBLIC
     FINAL
     CREATE PUBLIC .
   
     PUBLIC SECTION.
       INTERFACES if_http_service_extension.
       TYPES:
         BEGIN OF ty_transport_request,
           trkorr      TYPE trkorr,
           description TYPE as4text,
           type        TYPE trfunction,
           target_system TYPE tr_target,
           owner       TYPE tr_owner,
           status      TYPE trstatus,
         END OF ty_transport_request.
   
       METHODS:
         create_transport
           IMPORTING
             iv_description TYPE as4text
             iv_type        TYPE trfunction DEFAULT 'K'  " Workbench (K) or Customizing (W)
             iv_target      TYPE tr_target
           EXPORTING
             ev_trkorr      TYPE trkorr
             ev_error       TYPE string,
         
         get_transport_info
           IMPORTING
             iv_trkorr      TYPE trkorr
           EXPORTING
             es_transport   TYPE ty_transport_request
             ev_error       TYPE string,
         
         release_transport
           IMPORTING
             iv_trkorr      TYPE trkorr
             iv_target      TYPE tr_target
           EXPORTING
             ev_success     TYPE abap_bool
             ev_error       TYPE string.
   
     PRIVATE SECTION.
       " Implementation details...
   ENDCLASS.
   ```

2. **Create an RFC-enabled function module**:

   ```abap
   FUNCTION z_github_create_transport.
   *"----------------------------------------------------------------------
   *"*"Local Interface:
   *"  IMPORTING
   *"     VALUE(IV_DESCRIPTION) TYPE  AS4TEXT
   *"     VALUE(IV_TYPE) TYPE  TRFUNCTION DEFAULT 'K'
   *"     VALUE(IV_TARGET) TYPE  TR_TARGET
   *"     VALUE(IV_GITHUB_REFERENCE) TYPE  STRING
   *"  EXPORTING
   *"     VALUE(EV_TRKORR) TYPE  TRKORR
   *"     VALUE(EV_ERROR) TYPE  STRING
   *"----------------------------------------------------------------------
   
     DATA: ls_request TYPE trwbo_request,
           lt_e071    TYPE STANDARD TABLE OF e071,
           lt_e071k   TYPE STANDARD TABLE OF e071k,
           lv_trkorr  TYPE trkorr.
   
     " Create transport request
     ls_request-h-trkorr    = ''.        " Will be assigned by SAP
     ls_request-h-trfunction = iv_type.
     ls_request-h-trstatus  = 'D'.       " Modifiable
     ls_request-h-as4text   = iv_description.
     ls_request-h-as4user   = sy-uname.
     ls_request-h-tarsystem = iv_target.
   
     " Add GitHub reference as client-specific description
     ls_request-client_specific = abap_true.
     ls_request-h-client    = sy-mandt.
     ls_request-h-as4text   = iv_description && ' [GitHub:' && iv_github_reference && ']'.
   
     " Create the transport
     CALL FUNCTION 'TR_INSERT_REQUEST_WITH_TASKS'
       EXPORTING
         iv_type           = iv_type
         it_e071           = lt_e071
         it_e071k          = lt_e071k
         is_request_header = ls_request-h
       IMPORTING
         es_request        = ls_request
         ev_trkorr         = lv_trkorr
       EXCEPTIONS
         insert_failed     = 1
         OTHERS            = 2.
   
     IF sy-subrc <> 0.
       MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
         WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO ev_error.
     ELSE.
       ev_trkorr = lv_trkorr.
     ENDIF.
   
   ENDFUNCTION.
   ```

3. **Create a REST API or OData service** to expose these functions (alternative to RFC)

4. **Configure proper authorizations** for the service user that will execute these functions

## Step 3: Setting Up Microsoft API Management (APIM)

Configure APIM to securely connect GitHub Actions to SAP:

1. Create an API for transport management with these operations:
   - `POST /transports` - Create new transport request
   - `GET /transports/{id}` - Get transport status
   - `POST /transports/{id}/release` - Release transport
   - `GET /transports/{id}/objects` - List objects in transport

2. Configure authentication and security:
   - API key authentication for GitHub Actions
   - Backend service account for SAP connectivity
   - Rate limiting to prevent abuse

3. Implement request/response transformation:
   - Map GitHub Action parameters to SAP function module parameters
   - Handle error responses appropriately
   - Implement logging for audit purposes

## Step 4: Creating Custom GitHub Actions

Create custom GitHub Actions to interface with your SAP transport API:

1. **Create a transport-creator action** (`sap-transport-creator`):

   Create a repository with this structure:
   ```
   sap-transport-creator/
   ├── action.yml
   ├── Dockerfile
   ├── package.json
   └── src/
       └── index.js
   ```

   `action.yml`:
   ```yaml
   name: 'SAP Transport Creator'
   description: 'Creates SAP transport requests linked to GitHub features'
   inputs:
     api-url:
       description: 'API Management endpoint URL'
       required: true
     api-key:
       description: 'API Management API key'
       required: true
     description:
       description: 'Transport request description'
       required: true
     type:
       description: 'Transport type (K for Workbench, W for Customizing)'
       required: false
       default: 'K'
     target-system:
       description: 'Target system for the transport'
       required: true
     github-reference:
       description: 'GitHub reference (PR or branch)'
       required: true
   outputs:
     transport-number:
       description: 'Created SAP transport request number'
   runs:
     using: 'node16'
     main: 'dist/index.js'
   ```

   `index.js` (simplified):
   ```javascript
   const core = require('@actions/core');
   const github = require('@actions/github');
   const axios = require('axios');
   
   async function run() {
     try {
       // Get inputs
       const apiUrl = core.getInput('api-url', { required: true });
       const apiKey = core.getInput('api-key', { required: true });
       const description = core.getInput('description', { required: true });
       const type = core.getInput('type') || 'K';
       const targetSystem = core.getInput('target-system', { required: true });
       const githubRef = core.getInput('github-reference', { required: true });
       
       // Call API to create transport
       const response = await axios.post(
         `${apiUrl}/transports`,
         {
           description: description,
           type: type,
           target: targetSystem,
           github_reference: githubRef
         },
         {
           headers: {
             'Content-Type': 'application/json',
             'Ocp-Apim-Subscription-Key': apiKey
           }
         }
       );
       
       // Process response
       if (response.data && response.data.transport_number) {
         const transportNumber = response.data.transport_number;
         console.log(`Created transport request: ${transportNumber}`);
         core.setOutput('transport-number', transportNumber);
       } else {
         throw new Error('Invalid response from API');
       }
     } catch (error) {
       core.setFailed(`Action failed: ${error.message}`);
     }
   }
   
   run();
   ```

2. Build and publish the action to your GitHub organization

3. Create similar actions for:
   - Transport status checking
   - Transport release
   - Transport object listing

## Step 5: Setting Up Transport Creation Workflow

Create a GitHub workflow to automatically create transports:

1. Create a file named `.github/workflows/create-transport.yml`:

   ```yaml
   name: Create SAP Transport Request
   
   on:
     pull_request:
       types: [opened]
       branches:
         - develop
         - main
   
   jobs:
     create-transport:
       runs-on: ubuntu-latest
       steps:
         - name: Extract branch name
           shell: bash
           run: echo "BRANCH_NAME=${GITHUB_HEAD_REF}" >> $GITHUB_ENV
         
         - name: Create transport request
           id: create-transport
           uses: your-org/sap-transport-creator@v1
           with:
             api-url: ${{ secrets.SAP_API_URL }}
             api-key: ${{ secrets.SAP_API_KEY }}
             description: 'PR #${{ github.event.pull_request.number }}: ${{ github.event.pull_request.title }}'
             type: 'K'
             target-system: 'QAS'
             github-reference: 'PR-${{ github.event.pull_request.number }}'
         
         - name: Update PR description with transport number
           uses: actions/github-script@v6
           with:
             github-token: ${{ secrets.GITHUB_TOKEN }}
             script: |
               const transportNumber = '${{ steps.create-transport.outputs.transport-number }}';
               
               // Get current PR body
               const { data: pullRequest } = await github.rest.pulls.get({
                 owner: context.repo.owner,
                 repo: context.repo.repo,
                 pull_number: context.issue.number
               });
               
               // Add transport info
               const updatedBody = pullRequest.body + 
                 '\n\n## SAP Transport\n\n' +
                 `[TRANSPORT:${transportNumber}]`;
               
               // Update PR description
               await github.rest.pulls.update({
                 owner: context.repo.owner,
                 repo: context.repo.repo,
                 pull_number: context.issue.number,
                 body: updatedBody
               });
               
               // Add comment to PR
               await github.rest.issues.createComment({
                 owner: context.repo.owner,
                 repo: context.repo.repo,
                 issue_number: context.issue.number,
                 body: `🚀 Created SAP transport request: ${transportNumber}`
               });
   ```

2. Configure repository secrets:
   - `SAP_API_URL`: The API Management endpoint
   - `SAP_API_KEY`: The API key for authentication

## Step 6: Setting Up Transport Management Workflows

Create additional workflows for transport management:

1. **Transport status check workflow** `.github/workflows/check-transport.yml`:

   ```yaml
   name: Check SAP Transport Status
   
   on:
     workflow_dispatch:
       inputs:
         transport_number:
           description: 'Transport request number'
           required: true
     pull_request:
       types: [synchronize]
   
   jobs:
     check-transport:
       runs-on: ubuntu-latest
       steps:
         - name: Extract transport number
           id: extract
           if: github.event_name == 'pull_request'
           run: |
             PR_BODY=$(curl -s -H "Authorization: token ${{ secrets.GITHUB_TOKEN }}" \
                       "https://api.github.com/repos/${{ github.repository }}/pulls/${{ github.event.pull_request.number }}" \
                       | jq -r .body)
             TRANSPORT=$(echo "$PR_BODY" | grep -o '\[TRANSPORT:[A-Z0-9]*\]' | cut -d':' -f2 | cut -d']' -f1)
             echo "transport=$TRANSPORT" >> $GITHUB_OUTPUT
         
         - name: Use provided transport
           if: github.event_name == 'workflow_dispatch'
           run: echo "transport=${{ github.event.inputs.transport_number }}" >> $GITHUB_ENV
         
         - name: Use extracted transport
           if: github.event_name == 'pull_request'
           run: echo "transport=${{ steps.extract.outputs.transport }}" >> $GITHUB_ENV
         
         - name: Check transport status
           id: check-status
           uses: your-org/sap-transport-checker@v1
           with:
             api-url: ${{ secrets.SAP_API_URL }}
             api-key: ${{ secrets.SAP_API_KEY }}
             transport-number: ${{ env.transport }}
         
         - name: Comment with transport status
           if: github.event_name == 'pull_request'
           uses: actions/github-script@v6
           with:
             github-token: ${{ secrets.GITHUB_TOKEN }}
             script: |
               const status = `${{ steps.check-status.outputs.status }}`;
               const description = `${{ steps.check-status.outputs.description }}`;
               const objects = `${{ steps.check-status.outputs.object_count }}`;
               
               await github.rest.issues.createComment({
                 owner: context.repo.owner,
                 repo: context.repo.repo,
                 issue_number: context.issue.number,
                 body: `📦 **Transport Status Update**\n\nTransport: \`${{ env.transport }}\`\nStatus: ${status}\nDescription: ${description}\nObjects: ${objects}`
               });
   ```

2. **Transport release workflow** `.github/workflows/release-transport.yml`:

   ```yaml
   name: Release SAP Transport
   
   on:
     pull_request:
       types: [closed]
       branches:
         - develop
         - main
   
   jobs:
     release-transport:
       if: github.event.pull_request.merged == true
       runs-on: ubuntu-latest
       steps:
         - name: Extract transport number
           id: extract
           run: |
             PR_BODY=$(curl -s -H "Authorization: token ${{ secrets.GITHUB_TOKEN }}" \
                       "https://api.github.com/repos/${{ github.repository }}/pulls/${{ github.event.pull_request.number }}" \
                       | jq -r .body)
             TRANSPORT=$(echo "$PR_BODY" | grep -o '\[TRANSPORT:[A-Z0-9]*\]' | cut -d':' -f2 | cut -d']' -f1)
             echo "transport=$TRANSPORT" >> $GITHUB_OUTPUT
         
         - name: Release transport
           if: steps.extract.outputs.transport != ''
           id: release
           uses: your-org/sap-transport-releaser@v1
           with:
             api-url: ${{ secrets.SAP_API_URL }}
             api-key: ${{ secrets.SAP_API_KEY }}
             transport-number: ${{ steps.extract.outputs.transport }}
             target-system: 'QAS'
         
         - name: Comment with release status
           if: steps.extract.outputs.transport != ''
           uses: actions/github-script@v6
           with:
             github-token: ${{ secrets.GITHUB_TOKEN }}
             script: |
               const success = '${{ steps.release.outputs.success }}' === 'true';
               const message = success 
                 ? `✅ Transport \`${{ steps.extract.outputs.transport }}\` successfully released to QAS`
                 : `❌ Failed to release transport \`${{ steps.extract.outputs.transport }}\`: ${{ steps.release.outputs.error }}`;
               
               await github.rest.issues.createComment({
                 owner: context.repo.owner,
                 repo: context.repo.repo,
                 issue_number: context.issue.number,
                 body: message
               });
   ```

## Step 7: Implementing the Developer Workflow

Train your developers on working with the automated transport system:

1. **Create a pull request**:
   - GitHub automatically creates a transport request
   - Transport number is added to the PR description
   - Developer receives notification with transport number

2. **Development process**:
   - Developer uses the assigned transport in SAP
   - Each commit references the transport number
   - Status checks verify transport integrity

3. **Code review and approval**:
   - Reviewers can see the associated transport and objects
   - Approval triggers automated QA processes
   - After merge, the transport is automatically released to QAS

4. **Production deployment**:
   - After testing in QAS, create a production release PR
   - Or use the quality gate workflow for transport promotion

## Step 8: Creating a Transport Dashboard

For better visibility into transport status:

1. Create a scheduled workflow to collect transport status:

   ```yaml
   name: Update Transport Dashboard
   
   on:
     schedule:
       - cron: '0 */3 * * *'  # Every 3 hours
     workflow_dispatch:
   
   jobs:
     update-dashboard:
       runs-on: ubuntu-latest
       steps:
         - uses: actions/checkout@v3
         
         - name: Get recent PRs with transports
           id: get-prs
           uses: actions/github-script@v6
           with:
             github-token: ${{ secrets.GITHUB_TOKEN }}
             script: |
               const { data: prs } = await github.rest.pulls.list({
                 owner: context.repo.owner,
                 repo: context.repo.repo,
                 state: 'all',
                 sort: 'updated',
                 direction: 'desc',
                 per_page: 50
               });
               
               // Extract PRs with transport info
               const prsWithTransports = [];
               for (const pr of prs) {
                 const match = pr.body && pr.body.match(/\[TRANSPORT:([A-Z0-9]+)\]/);
                 if (match) {
                   prsWithTransports.push({
                     pr_number: pr.number,
                     title: pr.title,
                     state: pr.state,
                     merged: pr.merged,
                     transport: match[1],
                     created_at: pr.created_at,
                     updated_at: pr.updated_at
                   });
                 }
               }
               
               require('fs').writeFileSync('transport-prs.json', JSON.stringify(prsWithTransports, null, 2));
         
         - name: Collect transport status
           uses: your-org/sap-transport-collector@v1
           with:
             api-url: ${{ secrets.SAP_API_URL }}
             api-key: ${{ secrets.SAP_API_KEY }}
             input-file: 'transport-prs.json'
             output-file: 'public/data/transports.json'
         
         - name: Commit and push if changed
           run: |
             git config --global user.name 'Transport Bot'
             git config --global user.email 'transport-bot@example.com'
             git add public/data/transports.json
             git diff --quiet && git diff --staged --quiet || git commit -m "Update transport status [skip ci]"
             git push
   ```

2. Create a simple dashboard page using GitHub Pages to display transport status

## Validation

Your transport automation setup is working correctly when:

- Pull request creation automatically creates a transport request
- Transport numbers are properly tracked in PR descriptions
- Status checks validate transport content and status
- Merged PRs trigger transport release to QAS
- Production releases follow your defined workflow
- Transport dashboard shows accurate status information

## Troubleshooting

### Common Issues

| Issue | Solution |
|-------|----------|
| Transport creation fails | Check service user authorizations and RFC settings |
| Transport number not appearing in PR | Verify the GitHub script parsing logic for extracting transport numbers |
| Transport release fails | Check transport status in SAP and verify it's releasable |
| API connection issues | Check network connectivity and API Management configuration |
| Missing objects in transport | Ensure developers are using the correct transport and proper object activation |

### Debugging Tips

1. Enable verbose logging in GitHub Actions:
   ```yaml
   - name: Enable debugging
     run: echo "ACTIONS_RUNNER_DEBUG=true" >> $GITHUB_ENV
   ```

2. Add explicit error handling in your custom actions:
   ```javascript
   try {
     // Action code...
   } catch (error) {
     console.error('Detailed error:', error);
     core.setFailed(`Action failed: ${error.message}`);
   }
   ```

3. Use the SAP transaction AL11 to check RFC gateway logs

## Best Practices

1. **Transport Naming Convention**:
   - Include PR number in transport description
   - Use consistent prefixes for different types of changes
   - Consider adding feature/fix category indicators

2. **Transport Security**:
   - Regular rotation of API keys
   - Least-privilege service accounts
   - Audit logging for all transport operations
   - IP restrictions for API access

3. **Transport Management**:
   - Document dependencies between transports
   - Include validation of transport sequencing
   - Consider implementing change management integration
   - Regularly clean up old/obsolete transports

## Next Steps

After implementing transport automation, consider:

- [Controlled Transport Release](./controlled-release.md) for more complex release processes
- [Transport Sequencing and Dependencies](./sequencing.md) for managing complex changes
- [Advanced GitHub Actions](../advanced/custom-actions.md) for enhanced automation
- [Integration with SAP Solution Manager](../advanced/solman-integration.md) for enterprise-scale management

## Additional Resources

- [SAP Transport Management Documentation](https://help.sap.com/viewer/4a368c63b617424e8a2f473e7447525a/latest/en-US/4b632231a33c4e5fb09f2f90f243f17c.html)
- [GitHub Actions Documentation](https://docs.github.com/en/actions)
- [Microsoft API Management Documentation](https://docs.microsoft.com/en-us/azure/api-management/)
- [RFC SDK Documentation](https://support.sap.com/en/product/connectors/msnet.html)

---

*Last Updated: [YYYY-MM-DD] for [GitHub Enterprise X.Y.Z, SAP NetWeaver X.Y.Z]* 