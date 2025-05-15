# 📄 Setting Up Microsoft Sentinel for SAP Monitoring

## 📋 Table of Contents

- [📋 Overview](#overview)
- [✅ Prerequisites](#prerequisites)
- [Step-by-Step Implementation](#step-by-step-implementation)
- [Recommended Security Monitoring Scenarios](#recommended-security-monitoring-scenarios)
- [Troubleshooting](#troubleshooting)
- [➡
️ ️ Next Steps](#next-steps)


## 📋 Overview

This tutorial guides you through the process of configuring Microsoft Sentinel to monitor your SAP systems and GitHub repositories. By implementing Sentinel monitoring, you'll gain comprehensive security insights across your entire SAP-GitHub integration, enabling early detection of security threats and compliance violations.

## ✅ Prerequisites

Before starting this tutorial, ensure you have:

- An Azure subscription with sufficient permissions to create and configure Sentinel
- Access to Microsoft Sentinel workspace
- SAP system administrator credentials
- GitHub Enterprise administrator access
- Network connectivity between Azure, SAP systems, and GitHub Enterprise

## Step-by-Step Implementation

### 1. Create a Microsoft Sentinel Workspace

First, set up your Sentinel workspace in Azure:

1. Log in to the Azure portal (https://portal.azure.com)
2. Navigate to "Microsoft Sentinel"
3. Click "Create" or "Add"
4. Select "Create a new workspace" and configure:
   - **Subscription**: Select your Azure subscription
   - **Resource group**: Create or select an existing resource group
   - **Name**: Enter a distinctive name (e.g., "sap-github-sentinel")
   - **Region**: Select a region close to your SAP and GitHub instances
5. Click "Review + create" and then "Create"

### 2. Connect SAP Data Sources

Next, configure Sentinel to collect data from your SAP systems:

1. In your Sentinel workspace, navigate to "Data connectors"
2. Search for and select "SAP" in the connectors gallery
3. Click "Open connector page"
4. Follow the installation instructions, which include:
   - Installing the SAP data connector on your SAP systems
   - Configuring the necessary authorizations
   - Setting up log forwarding

For detailed SAP configuration, use the following settings in your SAP system:

```abap
"RSAU/CONFIG_CHANGE
  SOURCE_SYSTEM = 'Sentinel'
  TARGET_PATTERN = '/usr/sap/trans/log/security_audit*.log'
  TRACE_LEVEL = '3'
  ACTIVE = 'X'.
```

### 3. Connect GitHub Data Sources

Configure Sentinel to ingest data from GitHub Enterprise:

1. In your Sentinel workspace, navigate to "Data connectors"
2. Search for and select "GitHub Enterprise" in the connectors gallery
3. Click "Open connector page"
4. Complete the configuration steps:
   - Create a GitHub app for Sentinel integration
   - Generate and register authentication keys
   - Configure webhook settings for real-time event monitoring

GitHub App configuration example:

```json
{
  "name": "Microsoft Sentinel Connector",
  "url": "https://github.com/apps/microsoft-sentinel",
  "hook_attributes": {
    "url": "https://<your-function-app>.azurewebsites.net/api/GitHubWebhook",
    "content_type": "json",
    "secret": "<your-webhook-secret>"
  },
  "permissions": {
    "contents": "read",
    "metadata": "read",
    "security_events": "read",
    "statuses": "read",
    "vulnerability_alerts": "read"
  },
  "events": [
    "code_scanning_alert",
    "repository",
    "push",
    "repository_vulnerability_alert"
  ]
}
```

### 4. Configure API Management Data Collection

Set up data collection from your Microsoft API Management instance:

1. In your Sentinel workspace, navigate to "Data connectors"
2. Search for and select "Azure API Management" in the connectors gallery
3. Click "Open connector page"
4. Enable diagnostic settings on your API Management instance:
   - Navigate to your API Management resource in Azure
   - Select "Diagnostic settings" under Monitoring
   - Create a new setting pointing to your Log Analytics workspace

Enable these diagnostic logs:

- GatewayLogs
- WebSocketConnectionLogs
- DeveloperPortalLogs
- AdminAPILogs

### 5. Import SAP-GitHub Specific Workbooks

Import specialized workbooks for monitoring the SAP-GitHub integration:

1. In your Sentinel workspace, navigate to "Workbooks"
2. Click "Add workbook"
3. Click the "Edit" button
4. Click the "Advanced Editor" button (</> icon)
5. Replace the content with the SAP-GitHub integration workbook template:

```json
{
  "version": "Notebook/1.0",
  "items": [
    {
      "type": 1,
      "content": {
        "json": "# SAP-GitHub Integration Security Monitoring\r\n---\r\n\r\nThis workbook provides security insights across your SAP-GitHub integration pipeline."
      },
      "name": "text - 0"
    },
    {
      "type": 9,
      "content": {
        "version": "KqlParameterItem/1.0",
        "parameters": [
          {
            "id": "3f5f344a-d4a4-4d4f-90b6-e0a04c9a9fe2",
            "version": "KqlParameterItem/1.0",
            "name": "TimeRange",
            "type": 4,
            "isRequired": true,
            "value": {
              "durationMs": 86400000
            },
            "typeSettings": {
              "selectableValues": [
                {
                  "durationMs": 3600000
                },
                {
                  "durationMs": 14400000
                },
                {
                  "durationMs": 43200000
                },
                {
                  "durationMs": 86400000
                },
                {
                  "durationMs": 172800000
                },
                {
                  "durationMs": 259200000
                },
                {
                  "durationMs": 604800000
                },
                {
                  "durationMs": 1209600000
                }
              ],
              "allowCustom": true
            }
          }
        ],
        "style": "pills",
        "queryType": 0,
        "resourceType": "microsoft.operationalinsights/workspaces"
      },
      "name": "parameters - 1"
    },
    {
      "type": 3,
      "content": {
        "version": "KqlItem/1.0",
        "query": "SecurityEvent\r\n| where TimeGenerated {TimeRange}\r\n| where EventID == 4624 or EventID == 4625\r\n| summarize LoginAttempts = count(), SuccessfulLogins = countif(EventID == 4624), FailedLogins = countif(EventID == 4625) by bin(TimeGenerated, 1h), Computer\r\n| render timechart",
        "size": 0,
        "title": "SAP Login Attempts",
        "queryType": 0,
        "resourceType": "microsoft.operationalinsights/workspaces",
        "gridSettings": {
          "sortBy": [
            {
              "itemKey": "TimeGenerated",
              "sortOrder": 1
            }
          ]
        },
        "sortBy": [
          {
            "itemKey": "TimeGenerated",
            "sortOrder": 1
          }
        ]
      },
      "name": "query - 2"
    }
  ],
  "styleSettings": {},
  "fromTemplateId": "sentinel-UserWorkbook",
  "$schema": "https://github.com/Microsoft/Application-Insights-Workbooks/blob/master/schema/workbook.json"
}
```

6. Click "Apply" and then "Save"
7. Provide a name (e.g., "SAP-GitHub Security Monitoring")
8. Select appropriate location and save

### 6. Create Custom Analytics Rules

Create detection rules specific to the SAP-GitHub integration:

1. In your Sentinel workspace, navigate to "Analytics"
2. Click "Create" and select "Scheduled query rule"
3. Configure the rule:
   - **Name**: "Suspicious SAP-GitHub Transport Activity"
   - **Description**: "Detects unusual transport patterns between SAP and GitHub"
   - **Tactics**: Select appropriate MITRE ATT&CK tactics
   - **Severity**: Medium
   - **Status**: Enabled

4. In the "Set rule logic" section, enter this KQL query:

```kql
let timeframe = 1d;
let threshold = 3;
union
  AzureDiagnostics
  | where TimeGenerated >= ago(timeframe)
  | where Category == "GatewayLogs"
  | where httpMethod_s == "POST" 
  | where apiId_s contains "sap-transport" or operationId_s contains "transport",
  GitHubAuditData
  | where TimeGenerated >= ago(timeframe)
  | where OperationName contains "transport" or OperationName contains "workflow"
| summarize StartTime = min(TimeGenerated), EndTime = max(TimeGenerated), 
  count() by ClientIP = coalesce(clientIP_s, tostring(ClientIP), "unknown"), 
  User = coalesce(callerIPAddress_s, tostring(ActorIpAddress), "unknown"),
  Operation = coalesce(operationId_s, OperationName, "unknown")
| where count_ > threshold
| extend timestamp = StartTime
```

5. Configure the query scheduling and alert settings, then save the rule

### 7. Configure Incident Response Playbooks

Set up automated response playbooks for SAP-GitHub security incidents:

1. In your Sentinel workspace, navigate to "Automation"
2. Click "Create" and select "Playbook with incident trigger"
3. Configure a new Logic App with the following steps:
   - Trigger: When a Sentinel incident is created
   - Condition: Check if the incident involves SAP-GitHub integration
   - Actions:
     - Create a ServiceNow ticket or GitHub issue
     - Send email notification to security team
     - Add comment to the incident
     - Optional: Trigger a security scan on the affected repository

4. Save and enable the playbook

### 8. Enable User Entity Behavior Analytics (UEBA)

Configure UEBA to detect anomalous user behavior:

1. In your Sentinel workspace, navigate to "Entity behavior"
2. Click "Settings"
3. Enable data sources for both SAP and GitHub
4. Configure entity mapping for SAP and GitHub users
5. Set the analysis time window (recommended: 30 days)
6. Enable built-in anomaly detection rules

### 9. Validate the Configuration

Test your Sentinel configuration to ensure proper data collection:

1. Generate test events in your SAP system:
   - Attempt a few failed logins
   - Create and release a transport
   - Modify sensitive configuration

2. Generate test events in GitHub:
   - Create a pull request
   - Push a commit with a test security vulnerability
   - Trigger a CI/CD workflow

3. In Sentinel, navigate to "Logs" and run the following query to verify data collection:

```kql
search "SAP" or "GitHub"
| where TimeGenerated > ago(1h)
| take 100
```

## Recommended Security Monitoring Scenarios

Configure the following key monitoring scenarios for your SAP-GitHub integration:

### 1. Unauthorized Transport Requests

Detection of unauthorized or suspicious transport requests between systems:

```kql
AzureDiagnostics
| where Category == "GatewayLogs"
| where apiId_s contains "sap-transport" 
| join kind=leftouter (
    SapAuditLog 
    | where Operation contains "transport"
) on $left.user_s == $right.User
| where isempty(User)
| project TimeGenerated, ClientIP = clientIP_s, User = user_s, Operation = operationId_s
```

### 2. GitHub Credential Misuse

Detection of potential GitHub credential misuse:

```kql
GitHubAuditData
| where OperationName contains "oauth_authorization" 
| summarize AuthCount = count() by bin(TimeGenerated, 1h), ActorIpAddress, ActorLogin
| join kind=inner (
    GitHubAuditData
    | where OperationName contains "oauth_authorization"
    | summarize DistinctIPs = dcount(ActorIpAddress) by ActorLogin
    | where DistinctIPs > 1
) on ActorLogin
| project TimeGenerated, ActorLogin, ActorIpAddress, AuthCount
```

### 3. Sensitive ABAP Code Modifications

Detection of changes to sensitive ABAP objects:

```kql
union
  GitHubAuditData,
  SapAuditLog
| where TimeGenerated > ago(1d)
| where OperationName contains "push" or Operation contains "MODI" 
| where ObjectName contains "AUTH" or ObjectName contains "SECURITY" or ObjectName contains "PAYMENT"
| project TimeGenerated, User = coalesce(ActorLogin, User), Operation = coalesce(OperationName, Operation), Object = coalesce(Repo, ObjectName)
```

## Troubleshooting

| Issue | Solution |
|-------|----------|
| No SAP data in Sentinel | Verify the SAP connector configuration and check if audit logging is enabled in SAP |
| Missing GitHub events | Check the GitHub App permissions and webhook configuration |
| Workbook doesn't show data | Verify that the time range selected contains data and that all data connectors are properly configured |
| False positive alerts | Tune the detection rules by adjusting thresholds or adding exclusions |

## ➡️ Next Steps

After completing this tutorial, consider these advanced configurations:

- [Implementing Custom Security Scanning](./custom-scanning.md) for ABAP-specific vulnerability detection
- [Securing API Management](./secure-apim.md) for comprehensive API security
- Creating executive security dashboards for compliance reporting
- Implementing automated remediation playbooks for common security issues

---




