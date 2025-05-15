# 📄 MCP Implementation Guide

## 📋 Table of Contents

- [📋 Overview](#overview)
- [✅ Prerequisites](#prerequisites)
- [Architecture Overview](#architecture-overview)
- [🔧 Implementation Steps](#implementation-steps)
- [Testing the Integration](#testing-the-integration)
- [Troubleshooting](#troubleshooting)
- [Related Documentation](#related-documentation)
- [External References](#external-references)


## 📋 Overview

This guide provides step-by-step instructions for implementing the Model Context Protocol (MCP) integration layer and Context Store for your SAP-GitHub integration environment. Follow these steps to enhance your development workflow with persistent context.

## ✅ Prerequisites

Before beginning the MCP implementation, ensure you have:

- Existing SAP-GitHub integration with API Management
- Azure subscription with appropriate permissions
- SAP systems with RFC and HTTP connectivity
- GitHub Enterprise account with API access
- Administrative access to all systems

## Architecture Overview

The implementation will follow this architectural pattern:

```
                               ┌─────────────────┐
                               │  GitHub Copilot │
                               └────────┬────────┘
                                        │
┌──────────────┐   ┌──────────────┐   ┌─┴────────┐   ┌─────────────────┐
│  SAP Systems ├───┤ MCP Integration├──┤ Context  ├───┤ GitHub          │
│  DEV/QAS/PRD │   │ Layer         │  │ Store    │   │ Enterprise      │
└──────────────┘   └──────┬───────┬┘  └──────────┘   └─────────────────┘
                          │       │
                    ┌─────┴───────┴─────┐
                    │ Microsoft Security │
                    │ Services           │
                    └───────────────────┘
```

## 🔧 Implementation Steps

### Phase 1: Azure Infrastructure Setup

1. **Create Resource Group**

   ```bash
   az group create --name sap-github-mcp-rg --location eastus
   ```

2. **Deploy Context Store (Cosmos DB)**

   ```bash
   az deployment group create \
     --resource-group sap-github-mcp-rg \
     --template-file templates/cosmos-db-template.json \
     --parameters accountName=sap-github-context-store \
                  databaseName=ContextDB \
                  containerName=ContextContainer
   ```

3. **Deploy Storage Account for Large Artifacts**

   ```bash
   az storage account create \
     --name sapgithubcontextarts \
     --resource-group sap-github-mcp-rg \
     --location eastus \
     --sku Standard_LRS \
     --kind StorageV2
   ```

4. **Create API Management Instance (or use existing)**

   ```bash
   az apim create \
     --name sap-github-mcp-apim \
     --resource-group sap-github-mcp-rg \
     --publisher-name "Your Company" \
     --publisher-email "admin@yourcompany.com" \
     --sku-name Developer
   ```

5. **Deploy Azure Functions for Context Processing**

   ```bash
   az functionapp create \
     --name sap-github-mcp-functions \
     --resource-group sap-github-mcp-rg \
     --storage-account sapgithubcontextarts \
     --consumption-plan-location eastus \
     --runtime dotnet
   ```

### Phase 2: Context Store Setup

1. **Create Cosmos DB Collections**

   Create the following collections:
   - `contexts`: Main context documents
   - `links`: Context relationship links
   - `metadata`: Context metadata and indexing
   - `audit`: Audit logs for context access

   ```bash
   # Example for creating contexts collection
   az cosmosdb sql container create \
     --account-name sap-github-context-store \
     --database-name ContextDB \
     --name contexts \
     --partition-key-path "/id" \
     --resource-group sap-github-mcp-rg
   ```

2. **Configure Indexing**

   ```json
   {
     "indexingMode": "consistent",
     "automatic": true,
     "includedPaths": [
       {
         "path": "/*"
       }
     ],
     "excludedPaths": [
       {
         "path": "/content/*"
       }
     ]
   }
   ```

3. **Deploy Context API Functions**

   Deploy the following functions:
   - `CreateContext`: Create new context documents
   - `GetContext`: Retrieve context by ID
   - `UpdateContext`: Update existing context
   - `LinkContexts`: Create relationships between contexts
   - `SearchContext`: Search contexts by criteria

   ```bash
   # Deploy from source control
   az functionapp deployment source config \
     --name sap-github-mcp-functions \
     --resource-group sap-github-mcp-rg \
     --repo-url "https://github.com/your-org/mcp-functions" \
     --branch main \
     --manual-integration
   ```

4. **Configure API Management**

   a. Import Context API Swagger definition
   
   ```bash
   az apim api import \
     --resource-group sap-github-mcp-rg \
     --service-name sap-github-mcp-apim \
     --path /context \
     --display-name "Context API" \
     --api-id contextapi \
     --specification-format OpenApi \
     --specification-path apis/context-api-swagger.json
   ```
   
   b. Configure policies for authentication and rate limiting
   
   ```xml
   <policies>
     <inbound>
       <base />
       <validate-jwt header-name="Authorization" failed-validation-httpcode="401" failed-validation-error-message="Unauthorized. Access token is missing or invalid.">
         <openid-config url="https://login.microsoftonline.com/common/.well-known/openid-configuration" />
         <audiences>
           <audience>api://sap-github-mcp-api</audience>
         </audiences>
       </validate-jwt>
       <rate-limit calls="5" renewal-period="10" />
     </inbound>
   </policies>
   ```

### Phase 3: SAP Integration

1. **Create SAP RFC Function Module**

   Create a custom RFC module `Z_MCP_CREATE_CONTEXT` to capture context from SAP:
   
   ```abap
   FUNCTION Z_MCP_CREATE_CONTEXT.
   *"----------------------------------------------------------------------
   *"*"Local Interface:
   *"  IMPORTING
   *"     VALUE(IV_TRANSPORT_ID) TYPE  TRKORR
   *"     VALUE(IV_CONTEXT_TYPE) TYPE  STRING DEFAULT 'SAP_TRANSPORT'
   *"  EXPORTING
   *"     VALUE(EV_CONTEXT_ID) TYPE  STRING
   *"     VALUE(EV_CONTEXT_URL) TYPE  STRING
   *"  EXCEPTIONS
   *"      ERROR_CREATING_CONTEXT
   *"----------------------------------------------------------------------
   
     DATA:
       lv_payload TYPE string,
       lv_response TYPE string,
       lo_http_client TYPE REF TO if_http_client,
       lv_subrc TYPE sysubrc.
     
     " Create JSON payload for context API
     CONCATENATE '{"contextType":"' iv_context_type '","sourceSystem":"SAP","transportId":"'
                iv_transport_id '"}' INTO lv_payload.
     
     " Call Context API
     cl_http_client=>create_by_url(
       EXPORTING
         url    = 'https://sap-github-mcp-apim.azure-api.net/context/api/v1/contexts'
       IMPORTING
         client = lo_http_client ).
     
     lo_http_client->request->set_method( 'POST' ).
     lo_http_client->request->set_content_type( 'application/json' ).
     lo_http_client->request->set_cdata( lv_payload ).
     
     " Add authentication header (replace with your actual auth mechanism)
     lo_http_client->request->set_header_field(
       name  = 'Authorization'
       value = 'Bearer YOUR_TOKEN' ).
     
     lo_http_client->send( ).
     lo_http_client->receive( ).
     lv_subrc = lo_http_client->response->get_status( ).
     
     IF lv_subrc <> 201.
       RAISE error_creating_context.
     ENDIF.
     
     " Parse response to get context ID
     lv_response = lo_http_client->response->get_cdata( ).
     " Simple string parsing (in real implementation, use proper JSON parser)
     FIND REGEX '"id"\s*:\s*"([^"]+)"' IN lv_response SUBMATCHES ev_context_id.
     
     " Build context URL
     CONCATENATE 'https://sap-github-mcp-apim.azure-api.net/context/api/v1/contexts/'
                ev_context_id INTO ev_context_url.
     
     lo_http_client->close( ).
   
   ENDFUNCTION.
   ```

2. **Create SAP Custom Exit for Transport Events**

   ```abap
   METHOD handle_transport_event.
     DATA:
       lv_context_id TYPE string,
       lv_context_url TYPE string.
     
     CASE iv_event_type.
       WHEN 'RELEASE'.
         " Create context for transport release
         CALL FUNCTION 'Z_MCP_CREATE_CONTEXT'
           EXPORTING
             iv_transport_id = iv_transport_id
             iv_context_type = 'SAP_TRANSPORT_RELEASE'
           IMPORTING
             ev_context_id   = lv_context_id
             ev_context_url  = lv_context_url
           EXCEPTIONS
             error_creating_context = 1
             OTHERS                 = 2.
         
         IF sy-subrc <> 0.
           " Handle error
         ENDIF.
     ENDCASE.
   ENDMETHOD.
   ```

### Phase 4: GitHub Integration

1. **Create GitHub Actions Workflow for Context Integration**

   Create a file `.github/workflows/context-integration.yml`:

   ```yaml
   name: Context Integration

   on:
     pull_request:
       types: [opened, synchronize, reopened, closed]
     issues:
       types: [opened, edited, closed, reopened]

   jobs:
     create-context:
       runs-on: ubuntu-latest
       steps:
         - name: Create or Update Context
           uses: actions/github-script@v6
           with:
             script: |
               const eventType = context.eventName;
               const payload = {
                 contextType: `GITHUB_${eventType.toUpperCase()}`,
                 sourceSystem: 'GITHUB',
                 metadata: {
                   repo: context.repo.repo,
                   owner: context.repo.owner,
                   eventType: eventType
                 },
                 content: context.payload
               };
               
               // Call the Context API
               const response = await fetch('https://sap-github-mcp-apim.azure-api.net/context/api/v1/contexts', {
                 method: 'POST',
                 headers: {
                   'Content-Type': 'application/json',
                   'Authorization': `Bearer ${process.env.CONTEXT_API_TOKEN}`
                 },
                 body: JSON.stringify(payload)
               });
               
               if (!response.ok) {
                 console.error(`Error creating context: ${response.statusText}`);
                 core.setFailed('Failed to create context');
                 return;
               }
               
               const data = await response.json();
               console.log(`Created context with ID: ${data.id}`);
               
               // Set output for other jobs
               core.setOutput('contextId', data.id);
               core.setOutput('contextUrl', `https://sap-github-mcp-apim.azure-api.net/context/api/v1/contexts/${data.id}`);
           env:
             CONTEXT_API_TOKEN: ${{ secrets.CONTEXT_API_TOKEN }}
   ```

2. **Configure GitHub Secrets**

   In your GitHub repository or organization, add these secrets:
   
   - `CONTEXT_API_TOKEN`: Authentication token for Context API

### Phase 5: Security Integration

1. **Configure Azure Sentinel for Context Monitoring**

   Create a custom log with context events and configure detection rules.

2. **Deploy Context-Aware KQL Query**

   Create a file `sentinel/context-anomaly.kql`:

   ```kusto
   // SAP-GitHub Context Anomaly Detection
   // Detects unusual context patterns that may indicate unauthorized or malicious activity
   
   let ContextBaseline = 
       SAPGitHubContext_CL
       | where TimeGenerated > ago(30d) and TimeGenerated < ago(1d)
       | summarize 
           AvgContextSize = avg(ContextSize_d),
           MaxContextSize = max(ContextSize_d),
           CommonContextTypes = make_set(ContextType_s),
           UsualUsers = make_set(UserName_s)
       by TransportID_s, RepositoryName_s;
   
   // Detect anomalous context patterns
   SAPGitHubContext_CL
   | where TimeGenerated > ago(24h)
   | join kind=leftouter ContextBaseline on TransportID_s, RepositoryName_s
   | extend
       ContextSizeRatio = ContextSize_d / AvgContextSize,
       IsUnusualUser = not(array_contains(UsualUsers, UserName_s)),
       IsUnusualContextType = not(array_contains(CommonContextTypes, ContextType_s))
   | where 
       ContextSizeRatio > 3 or
       IsUnusualUser or
       IsUnusualContextType
   | project
       TimeGenerated,
       TransportID_s,
       RepositoryName_s,
       UserName_s,
       ContextType_s,
       ContextSizeRatio,
       IsUnusualUser,
       IsUnusualContextType,
       ClientIP_s
   ```

### Phase 6: GitHub Copilot Integration

1. **Configure GitHub Copilot for Enterprise**

   Follow the steps in [GitHub Copilot Setup Guide](../implementation/github-copilot-setup.md).

2. **Create Context-Aware Prompts**

   Develop context-aware prompts for ABAP development as documented in the [ABAP Prompting Patterns](../../examples/copilot-prompts/abap-patterns.md).

## Testing the Integration

1. **Create Test Transport in SAP**

   Create a test transport with the relevant objects.

2. **Verify Context Creation**

   Verify the context creation in the Context Store using:

   ```bash
   curl -X GET "https://sap-github-mcp-apim.azure-api.net/context/api/v1/contexts?type=SAP_TRANSPORT&transportId=YOUR_TRANSPORT_ID" \
     -H "Authorization: Bearer YOUR_TOKEN"
   ```

3. **Create GitHub PR and Link to Context**

   Create a PR in GitHub and verify that the context is created and linked to the SAP transport.

## Troubleshooting

### Common Issues

1. **Context API Authentication Failures**
   
   Check the token expiration and permissions.

2. **Missing Context Data**
   
   Verify the SAP function module is correctly capturing all required data.

3. **Integration Errors**
   
   Check the Azure Function logs for details on processing errors.

## Related Documentation

- [MCP Integration Architecture](../architecture/mcp-integration.md)
- [Context Store Design](../architecture/context-store.md)
- [GitHub Copilot Setup](./github-copilot-setup.md)

## External References

1. [Azure Functions Documentation](https://learn.microsoft.com/en-us/azure/azure-functions/)
2. [Azure Cosmos DB Documentation](https://learn.microsoft.com/en-us/azure/cosmos-db/)
3. [GitHub Actions Documentation](https://docs.github.com/en/actions)
4. [SAP RFC SDK Documentation](https://support.sap.com/en/product/connectors/msnet.html) 
---


