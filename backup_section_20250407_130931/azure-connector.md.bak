# Azure Connector Setup for Microsoft Sentinel

## Overview
This document provides detailed instructions for configuring Azure service connectors in Microsoft Sentinel to monitor your Azure resources that support the SAP-GitHub integration. This setup enables comprehensive security monitoring, threat detection, and incident response across your Azure infrastructure.

## Prerequisites
- Microsoft Sentinel workspace configured (refer to [Sentinel Workspace Setup](./sentinel-workspace.md))
- Azure subscription with appropriate permissions
- Basic understanding of Azure services and Microsoft Sentinel
- Resource providers registered for Azure services to be monitored

## Detailed Content

### Understanding Azure Connectors

Microsoft Sentinel can ingest security data from multiple Azure services:

1. **Core Azure Services**
   - Azure Activity logs
   - Azure Active Directory logs
   - Azure Resource logs
   - Azure Security Center alerts
   - Azure Defender alerts

2. **Integration-Specific Services**
   - API Management logs
   - Azure Functions logs
   - Azure Logic Apps logs
   - Azure Key Vault logs
   - Virtual Machine logs

3. **Networking Components**
   - Network Security Group flow logs
   - Azure Firewall logs
   - Azure Application Gateway WAF logs
   - Private Link and Private Endpoints logs

### Configuring Core Azure Connectors

1. **Azure Activity Logs**
   - Navigate to Microsoft Sentinel > Data connectors
   - Search for "Azure Activity" and select it
   - Click "Open connector page"
   - Select the Azure subscriptions to connect
   - Click "Connect"
   - Verify data collection starts (may take up to 15 minutes)

2. **Azure Active Directory Logs**
   - Navigate to Microsoft Sentinel > Data connectors
   - Search for "Azure Active Directory" and select it
   - Click "Open connector page"
   - Choose which logs to collect:
     - Sign-in logs
     - Audit logs
     - Provisioning logs
   - Click "Apply"
   - Validate data is flowing correctly

3. **Azure Defender (Microsoft Defender for Cloud)**
   - Navigate to Microsoft Sentinel > Data connectors
   - Search for "Microsoft Defender for Cloud" and select it
   - Click "Open connector page"
   - Select the subscriptions to connect
   - Click "Connect"
   - Verify detection of new alerts

### Configuring API Management Logs

For SAP-GitHub integration monitoring, API Management is critical:

1. **Enable Diagnostic Settings**
   - Navigate to your API Management instance in Azure Portal
   - Select "Diagnostic settings" from the left menu
   - Click "Add diagnostic setting"
   - Configure:
     - **Name**: "API Management to Sentinel"
     - **Categories**: Select all log categories
       - Gateway Logs
       - WebSocket Logs
       - Management Logs
     - **Destination details**: Select "Send to Log Analytics workspace"
     - **Subscription/Workspace**: Select your Sentinel workspace
   - Click "Save"

2. **Create Custom Logs for API Payload Monitoring**
   - In API Management, configure logging policy to capture payloads:
     ```xml
     <policies>
         <inbound>
             <base />
             <set-variable name="requestTime" value="@(DateTime.UtcNow)" />
             <log-to-eventhub logger-id="sentinel-logger">
                 @{
                     return new JObject(
                         new JProperty("EventTime", DateTime.UtcNow.ToString()),
                         new JProperty("ServiceName", context.Api.Name),
                         new JProperty("OperationName", context.Operation.Name),
                         new JProperty("RequestMethod", context.Request.Method),
                         new JProperty("RequestPath", context.Request.Url.Path),
                         new JProperty("RequestHeaders", context.Request.Headers.AllKeys.Select(h => new JProperty(h, context.Request.Headers.GetValueOrDefault(h))).ToArray()),
                         new JProperty("RequestBody", context.Request.Body?.As<string>(preserveContent: true))
                     ).ToString();
                 }
             </log-to-eventhub>
         </inbound>
         <!-- Other policy sections -->
     </policies>
     ```
   - Configure Event Hub to forward logs to Log Analytics

3. **Configure API Management Analytics**
   - Navigate to Microsoft Sentinel > Workbooks
   - Create new workbook for API Management monitoring
   - Add KQL queries for tracking API usage patterns
   - Configure alerts for anomalous API behavior

### Configuring Virtual Machine Monitoring

For SAP and integration systems running on Azure VMs:

1. **Install Log Analytics Agent**
   - Navigate to the VM in Azure Portal
   - Select "Logs" from the left menu
   - Click "Enable" to install the Log Analytics agent
   - Select your Sentinel workspace
   - Wait for agent installation to complete

2. **Configure Data Collection**
   - In Log Analytics workspace, go to "Agents configuration"
   - Configure Windows Event logs collection:
     - System: Error, Warning, Information
     - Application: Error, Warning, Information
     - Security: All events
   - Configure Linux Syslog collection:
     - facility: '*'
     - log levels: emergency, alert, critical, error, warning
   - Configure performance counters collection

3. **Enable Change Tracking**
   - Navigate to VM > Change tracking
   - Enable change tracking for:
     - Files (especially configuration files)
     - Registry (Windows VMs)
     - Software
     - Services
   - Configure to send data to your Sentinel workspace

### Configuring Network Security Monitoring

For comprehensive visibility into network traffic:

1. **Enable NSG Flow Logs**
   - Navigate to the Network Security Group in Azure Portal
   - Select "Flow logs" from the left menu
   - Click "Create"
   - Configure:
     - **Flow logs version**: Version 2
     - **Storage account**: Select or create a storage account
     - **Retention**: Set appropriate retention period (e.g., 30 days)
     - **Traffic Analytics**: Enable
     - **Processing interval**: 10 minutes
     - **Log Analytics workspace**: Select your Sentinel workspace
   - Click "Save"

2. **Enable Azure Firewall Logs**
   - Navigate to your Azure Firewall in Azure Portal
   - Select "Diagnostic settings" from the left menu
   - Click "Add diagnostic setting"
   - Configure:
     - **Name**: "Firewall to Sentinel"
     - **Categories**: Select all log categories
       - AzureFirewallApplicationRule
       - AzureFirewallNetworkRule
       - AzureFirewallDNSProxy
     - **Destination details**: Select "Send to Log Analytics workspace"
     - **Subscription/Workspace**: Select your Sentinel workspace
   - Click "Save"

3. **Configure Network Watcher**
   - Enable Network Watcher in your regions
   - Configure packet captures for specific scenarios
   - Set up connection monitors for critical connections

### Configuring Integration Service Monitoring

For SAP-GitHub integration middleware:

1. **Azure Functions Logs**
   - Navigate to your Function App in Azure Portal
   - Select "Diagnostic settings" from the left menu
   - Click "Add diagnostic setting"
   - Configure to send FunctionAppLogs to your Sentinel workspace
   - Enable Application Insights integration

2. **Logic Apps Logs**
   - Navigate to your Logic App in Azure Portal
   - Select "Diagnostic settings" from the left menu
   - Enable workflow runtime logs
   - Configure to send to your Sentinel workspace

3. **Key Vault Logs**
   - Navigate to your Key Vault in Azure Portal
   - Select "Diagnostic settings" from the left menu
   - Enable all log categories
   - Configure to send to your Sentinel workspace

### Creating Integration-Specific Queries

Implement these queries for monitoring integration components:

1. **API Management SAP-GitHub Traffic**
   ```kql
   ApiManagementGatewayLogs
   | where OperationId contains "SAP" or OperationId contains "GitHub"
   | where TimeGenerated > ago(24h)
   | extend ResponseCode = tostring(ResponseCode)
   | summarize count() by OperationId, ResponseCode, bin(TimeGenerated, 1h)
   | render timechart
   ```

2. **Integration Function Failures**
   ```kql
   FunctionAppLogs
   | where TimeGenerated > ago(24h)
   | where FunctionName contains "SAP" or FunctionName contains "GitHub"
   | where Level == "Error"
   | project TimeGenerated, FunctionName, Message, _ResourceId
   | order by TimeGenerated desc
   ```

3. **Logic App Integration Workflow Status**
   ```kql
   AzureDiagnostics
   | where ResourceProvider == "MICROSOFT.LOGIC"
   | where Category == "WorkflowRuntime"
   | extend workflow = tostring(resource_workflowName_s)
   | where workflow contains "SAP" or workflow contains "GitHub"
   | extend status = tostring(status_s)
   | summarize count() by workflow, status, bin(TimeGenerated, 1d)
   | pivot status
   ```

4. **Key Vault Access Monitoring**
   ```kql
   AzureDiagnostics
   | where ResourceProvider == "MICROSOFT.KEYVAULT"
   | where Category == "AuditEvent"
   | extend identity = tostring(identity_claim_upn_s)
   | extend operation = tostring(OperationName)
   | extend vault = tostring(Resource)
   | extend secret = tostring(id_s)
   | project TimeGenerated, identity, operation, vault, secret, ResultType, ResultSignature
   ```

### Creating Custom Azure Connectors

For specialized monitoring needs:

1. **Custom HTTP Data Collector API**
   - Create and configure the HTTP Data Collector API:
     ```
     Endpoint: https://{workspaceId}.ods.opinsights.azure.com/api/logs?api-version=2016-04-01
     Method: POST
     Headers:
       - Content-Type: application/json
       - Authorization: SharedKey {workspaceId}:{signature}
     ```
   - Generate signature using your workspace key
   - Format payload according to Log Analytics requirements

2. **Custom Azure Function Collector**
   - Create an Azure Function to:
     - Poll specific services or endpoints
     - Transform data into Sentinel-compatible format
     - Send to Log Analytics workspace
   - Schedule the function to run at appropriate intervals

3. **Azure Event Hub Integration**
   - Configure Event Hubs to collect custom application logs
   - Set up Event Hub capture for archiving
   - Create Logic App to process events and send to Sentinel

## Best Practices

1. **Data Collection Strategy**
   - Start with core Azure services
   - Prioritize integration-specific services
   - Implement custom collection only when necessary
   - Consider data volume and costs

2. **Performance Optimization**
   - Use filtering at source when possible
   - Implement appropriate sampling for high-volume data
   - Configure optimal retention periods
   - Monitor ingestion volume regularly

3. **Security Considerations**
   - Use managed identities for authentication
   - Implement least privilege for collectors
   - Encrypt data in transit and at rest
   - Regularly rotate shared keys and credentials

4. **Governance**
   - Document all data collection configurations
   - Implement tagging for Azure resources
   - Create naming conventions for diagnostic settings
   - Define ownership for each connector

## Troubleshooting

### Common Issues and Solutions

- **No Data Flowing**:
  - Verify diagnostic settings are properly configured
  - Check resource provider registration
  - Validate workspace connectivity
  - Examine permission and network restrictions

- **Incomplete Data**:
  - Review collection configuration
  - Check for filtering that might exclude data
  - Verify service is generating expected logs
  - Test query with different time ranges

- **Data Ingestion Delays**:
  - Check service health for Log Analytics
  - Verify collector components are functioning
  - Adjust collection frequency if needed
  - Monitor latency metrics

## Next Steps

After configuring Azure connectors, proceed to:

- [Standard Rules Deployment](./standard-rules.md) - Deploy standard analytics rules
- [Custom SAP Rules](./custom-sap-rules.md) - Implement SAP-specific detection rules
- [Integration Dashboard](./integration-dashboard.md) - Set up comprehensive monitoring dashboard

## References

- [Microsoft Sentinel Data Connectors Documentation](https://docs.microsoft.com/en-us/azure/sentinel/connect-data-sources)
- [Azure Monitor Documentation](https://docs.microsoft.com/en-us/azure/azure-monitor/)
- [Log Analytics Data Ingestion API](https://docs.microsoft.com/en-us/azure/azure-monitor/logs/data-collector-api)
- [Azure Diagnostic Settings Documentation](https://docs.microsoft.com/en-us/azure/azure-monitor/essentials/diagnostic-settings)
- [Microsoft Sentinel Best Practices](https://docs.microsoft.com/en-us/azure/sentinel/best-practices)

---

**Document Metadata:**
- Last Updated: 2023-07-22
- Contributors: Cloud Security Team, Integration Team
- Version: 1.0.0 