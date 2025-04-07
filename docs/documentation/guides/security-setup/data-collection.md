# 📄 Data Collection for Microsoft Sentinel

## 📋 Table of Contents

- [📋 Overview](#overview)
- [✅ Prerequisites](#prerequisites)
- [Detailed Content](#detailed-content)
- [Best Practices](#best-practices)
- [Troubleshooting](#troubleshooting)
- [➡
️ ️ Next Steps](#next-steps)
- [References](#references)


## 📋 Overview
This document provides comprehensive instructions for configuring data collection in Microsoft Sentinel for your SAP-GitHub integration. Proper data collection is the foundation of effective security monitoring, enabling threat detection, incident response, and security analytics across your environment.

## ✅ Prerequisites
- Microsoft Sentinel workspace configured (refer to [Sentinel Workspace Setup](./sentinel-workspace.md))
- Appropriate permissions in Azure and source systems
- Network connectivity between source systems and Azure
- Familiarity with Log Analytics and data connectors

## Detailed Content

### Data Collection Strategy

When planning your data collection strategy, consider the following aspects:

1. **Data Sources Prioritization**
   - Focus on critical systems and infrastructure first
   - Prioritize security-relevant logs and events
   - Include both SAP and GitHub data sources

2. **Data Volume Management**
   - Estimate data ingestion volumes
   - Define appropriate retention periods
   - Implement data filtering where appropriate

3. **Collection Methods**
   - Agent-based collection for system logs
   - API-based collection for application logs
   - Syslog/CEF/Fluentd for network devices

### Configuring Core Data Connectors

1. **Azure Activity Logs**
   - Navigate to Microsoft Sentinel > Data connectors
   - Search for "Azure Activity" and open the connector page
   - Click "Connect" to enable Azure Activity logs collection
   - Configure subscription coverage as needed

2. **Azure AD Logs**
   - Navigate to Microsoft Sentinel > Data connectors
   - Search for "Azure Active Directory" and open the connector page
   - Enable the logs you want to collect (Sign-ins, Audit logs, etc.)
   - Click "Apply" to save the configuration

3. **Office 365 Logs** (if applicable)
   - Navigate to Microsoft Sentinel > Data connectors
   - Search for "Office 365" and open the connector page
   - Select workloads to monitor (Exchange, SharePoint, Teams)
   - Click "Connect" to enable data collection

### Configuring SAP Data Collection

SAP data collection requires specific setup. For detailed instructions, refer to [SAP Connector Setup](./sap-connector.md). Key steps include:

1. **Deploy SAP Connector Components**
   - Set up connector VM infrastructure
   - Configure network connectivity
   - Install required SAP libraries

2. **Configure SAP Log Sources**
   - Enable Security Audit Log (SM19)
   - Configure System Log (SM21)
   - Set up change document logging
   - Enable table access logging

3. **Verify SAP Data Collection**
   - Check for data in Log Analytics
   - Validate log formats and fields
   - Monitor ingestion latency

### Configuring GitHub Data Collection

GitHub data collection requires specific setup. For detailed instructions, refer to [GitHub Connector Setup](./github-connector.md). Key steps include:

1. **Configure GitHub Enterprise Connection**
   - Create authentication tokens
   - Configure API access
   - Set up webhook endpoints

2. **Define GitHub Event Collection**
   - Repository events (commits, branches)
   - Issue and PR events
   - Security events (code scanning, secret scanning)
   - Workflow run events

3. **Verify GitHub Data Collection**
   - Check for data in Log Analytics
   - Validate event formats and fields
   - Monitor API rate limits

### Custom Data Collection

For specialized data sources not covered by built-in connectors:

1. **Custom Logs via Log Analytics Agent**
   - Install Log Analytics agent on source systems
   - Configure agent to collect custom logs
   - Define custom log formats in Log Analytics

2. **Custom API-based Collection**
   - Create Azure Functions to call APIs
   - Process and format data
   - Send to Log Analytics using HTTP Data Collector API

3. **Data Transformation with Logic Apps**
   - Create Logic Apps workflows to process data
   - Transform data to standardized format
   - Send to Log Analytics workspace

### Common Event Format (CEF) Collection

For network and security appliances:

1. **Deploy CEF Collector**
   - Set up a Linux VM as CEF collector
   - Install the Log Analytics agent
   - Configure syslog daemon

2. **Configure Devices to Send CEF**
   - Update firewall/appliance configurations
   - Point syslog output to CEF collector
   - Validate message formatting

### Data Collection Verification

After setting up data connectors:

1. **Check Data Ingestion**
   - In Log Analytics, run queries to verify data:
     ```kql
     // Check for Azure Activity logs
     AzureActivity
     | summarize count() by OperationName
     | sort by count_ desc
     | limit 10
     
     // Check for Azure AD logs
     SigninLogs
     | summarize count() by UserPrincipalName
     | sort by count_ desc
     | limit 10
     ```

2. **Monitor Collection Health**
   - Set up data collection health monitoring
   - Create alerts for collection failures
   - Monitor data ingestion rates

## Best Practices

1. **Data Collection Scope**
   - Start with essential security logs
   - Gradually expand coverage
   - Regularly review collection strategy

2. **Performance Optimization**
   - Use filtering to reduce volume
   - Implement batching for API calls
   - Configure appropriate time intervals

3. **Cost Management**
   - Monitor daily ingestion
   - Set up daily caps
   - Implement data filtering at source

4. **Security Considerations**
   - Secure communication channels
   - Use encrypted connections
   - Implement least privilege for collectors

## Troubleshooting

### Common Issues and Solutions

- **No Data Appearing**:
  - Verify connectivity between sources and Azure
  - Check agent installation status
  - Validate permissions and authentication
  - Confirm data is being generated at source

- **Incomplete Data Collection**:
  - Review configuration for all event types
  - Check filtering settings
  - Verify format specifications

- **High Latency**:
  - Check network connectivity
  - Optimize batch sizes
  - Increase resources for collection components

## ➡️ Next Steps

After configuring data collection, proceed to:

- [Azure Connector Setup](./azure-connector.md) - Configure additional Azure service connectors
- [Standard Rules Deployment](./standard-rules.md) - Deploy standard detection rules
- [Custom SAP Rules](./custom-sap-rules.md) - Implement SAP-specific detection rules
- [GitHub Activity Workbook](./github-workbook.md) - Set up visualization dashboards

## References

- [Microsoft Sentinel Data Connectors Documentation](https://docs.microsoft.com/en-us/azure/sentinel/connect-data-sources)
- [Log Analytics Data Collection API](https://docs.microsoft.com/en-us/azure/azure-monitor/logs/data-collector-api)
- [CEF and Syslog Collection](https://docs.microsoft.com/en-us/azure/sentinel/connect-common-event-format)
- [Optimizing Log Analytics Data Ingestion](https://docs.microsoft.com/en-us/azure/azure-monitor/logs/manage-cost-storage)

---




