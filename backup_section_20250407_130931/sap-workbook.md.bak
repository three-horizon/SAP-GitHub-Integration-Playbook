# SAP Security Workbook for Microsoft Sentinel

## Overview
This document provides comprehensive instructions for implementing and customizing the SAP Security Workbook in Microsoft Sentinel. This interactive dashboard enables security teams to visualize, analyze, and investigate SAP security events, helping identify threats, compliance issues, and suspicious activities within your SAP landscape that's integrated with GitHub.

## Prerequisites
- Microsoft Sentinel workspace configured (refer to [Sentinel Workspace Setup](./sentinel-workspace.md))
- SAP data collection configured (refer to [SAP Connector Setup](./sap-connector.md))
- Data flowing successfully from SAP to Sentinel
- Appropriate permissions to create and edit workbooks

## Detailed Content

### Understanding SAP Security Monitoring

SAP security monitoring with Sentinel workbooks offers:

1. **Comprehensive Visibility**
   - Monitor critical SAP security logs
   - Track authorization changes and sensitive transactions
   - Identify suspicious user activities
   - Correlate security events across systems

2. **Compliance Monitoring**
   - Track SOD (Segregation of Duties) violations
   - Monitor regulatory compliance status
   - Document access to sensitive data
   - Support audit requirements

3. **Threat Detection**
   - Identify potential security breaches
   - Detect anomalous user behaviors
   - Monitor for known attack patterns
   - Correlate with other security events

### Deploying the SAP Security Workbook

1. **Using the Template Gallery**
   - Navigate to Microsoft Sentinel > Workbooks
   - Click "Templates" to open the gallery
   - Search for "SAP" in the template gallery
   - Locate the "SAP Security Monitoring" workbook template
   - Click "Save" to save a copy to your workspace
   - Click "View saved workbook" to open it

2. **Manual Import (Alternative Method)**
   - Download the SAP Security workbook ARM template from:
     ```
     https://raw.githubusercontent.com/Azure/Azure-Sentinel/master/Workbooks/SapMonitor/SAPSecurityWorkbook.json
     ```
   - In Microsoft Sentinel, navigate to Workbooks
   - Click "Add workbook"
   - Click the "Edit" button in the toolbar
   - Click the "Advanced Editor" button (</> icon)
   - Replace the existing content with the downloaded template
   - Click "Apply" and then "Save"

### Workbook Contents and Features

The SAP Security workbook includes these key sections:

1. **Security Overview**
   - Security audit log volume
   - Critical security events
   - Failed login attempts
   - Events by SAP system

2. **User Activity Analysis**
   - Privileged user activities
   - Transaction usage patterns
   - User login patterns
   - Authorization changes

3. **Transaction Monitoring**
   - Critical transaction usage
   - Transaction usage by system
   - Unauthorized transaction attempts
   - Transaction trends over time

4. **System Table Access**
   - Sensitive table access events
   - Table modifications
   - Direct table access (SE16/SE16N)
   - Table access by non-standard users

5. **Transport Analysis**
   - Transport activities
   - Critical object changes
   - Transport pathway visualization
   - Integration with GitHub commits

### Customizing the SAP Security Workbook

Modify the workbook to match your SAP environment:

1. **Updating System Identifiers**
   - Edit the workbook parameters
   - Update the SAP system list parameter with your SIDs
   - Add client numbers relevant to your environment
   - Adjust time range parameters as needed

2. **Editing Critical Transaction Lists**
   - Locate the critical transaction parameter
   - Add or remove transactions based on your security policies
   - Update transaction descriptions if needed
   - Organize transactions by category

3. **Sensitive Table Customization**
   - Update the sensitive tables parameter
   - Add your organization-specific sensitive tables
   - Group tables by business function
   - Add table descriptions for context

4. **Adding Custom Visualizations**
   - In edit mode, click "Add" and select "Add query"
   - Write a KQL query for the data you want to visualize
   - Select the visualization type (chart, grid, etc.)
   - Configure visualization settings
   - Click "Done Editing"

### Essential KQL Queries for SAP Monitoring

Here are key queries to include in your workbook:

1. **Security Audit Log Overview**
   ```kql
   SAPSecurity_CL
   | where TimeGenerated > ago(7d)
   | summarize count() by EventType_s, bin(TimeGenerated, 1d)
   | render timechart
   ```

2. **Failed Login Analysis**
   ```kql
   SAPSecurity_CL
   | where TimeGenerated > ago(24h)
   | where EventType_s == "AUDIT_FAILURE" and EventSubType_s == "LOGIN_FAILED"
   | summarize FailedCount=count() by User_s, Terminal_s, Client_s
   | where FailedCount > 3
   | sort by FailedCount desc
   ```

3. **Privileged Transaction Usage**
   ```kql
   let criticalTCodes = dynamic(["SU01", "SU10", "SU24", "SM19", "SE16", "SE16N", "SM59", "SM49", "SA38", "SE38"]);
   SAPSecurity_CL
   | where TimeGenerated > ago(7d)
   | where TransactionCode_s in (criticalTCodes)
   | summarize count() by User_s, TransactionCode_s, SAPSystem_s, Client_s
   | sort by count_ desc
   ```

4. **Authorization Changes**
   ```kql
   SAPSecurity_CL
   | where TimeGenerated > ago(30d)
   | where EventType_s == "AUDIT_AUTHORIZATION" 
   | extend AuthObject = extract("([A-Z_0-9]+)", 1, EventText_s)
   | summarize count() by User_s, AuthObject, SAPSystem_s, bin(TimeGenerated, 1d)
   | render columnchart
   ```

5. **Table Access Monitoring**
   ```kql
   let sensitiveTables = dynamic(["USR02", "USRAC", "USR40", "USH02", "CDHDR", "CDPOS", "TSTC", "USOBT"]);
   SAPSecurity_CL
   | where TimeGenerated > ago(14d)
   | where (TransactionCode_s in ("SE16", "SE16N", "SE17", "SM30", "SM31")) or (EventType_s == "AUDIT_TABLE")
   | extend TableName = extract("([A-Z_0-9]+)", 1, EventText_s)
   | where TableName in (sensitiveTables)
   | project TimeGenerated, User_s, TableName, TransactionCode_s, Terminal_s, SAPSystem_s
   ```

### SAP-GitHub Integration Visualizations

Add these specialized queries to monitor the integration between SAP and GitHub:

1. **Transport to GitHub Commit Correlation**
   ```kql
   let github_commits = GitHubRepoEvents_CL
   | where event_type_s == "push";
   let sap_transports = SAPTransportEvents_CL;
   github_commits
   | join kind=inner sap_transports on $left.commit_message_s contains $right.TransportID_s
   | project TimeGenerated, Repository=repository_name_s, TransportID=TransportID_s, 
     TransportType=TransportType_s, Committer=actor_s, SAPSystem=SystemID_s
   ```

2. **Transport Security Analysis**
   ```kql
   SAPTransportEvents_CL
   | join kind=leftouter (
       SAPSecurity_CL
       | where EventType_s == "AUDIT_AUTHORIZATION"
       | where TransactionCode_s in ("SE01", "SE09", "SE10", "STMS")
   ) on User_s
   | project TransportID_s, TransportType_s, User_s, TimeGenerated, EventText_s, SystemID_s
   ```

3. **System Change Overview**
   ```kql
   let transports = SAPTransportEvents_CL
   | where TimeGenerated > ago(30d);
   let security_events = SAPSecurity_CL
   | where TimeGenerated > ago(30d)
   | where EventType_s in ("AUDIT_CHANGE", "AUDIT_AUTHORIZATION");
   let github_events = GitHubRepoEvents_CL
   | where TimeGenerated > ago(30d)
   | where event_type_s == "push";
   union transports, security_events, github_events
   | summarize count() by bin(TimeGenerated, 1d), Source=iff(TableName == "SAPTransportEvents_CL", "Transport", 
       iff(TableName == "SAPSecurity_CL", "Security", "GitHub"))
   | render timechart
   ```

### Custom Reports for SAP Security

Create these specialized reports for your SAP security teams:

1. **SOD Violations Report**
   - Create a grid showing users with conflicting authorizations
   - Include links to detailed user authorization reports
   - Highlight critical violations
   - Show trends over time

2. **Emergency Access Management**
   - Track Firefighter/Emergency user access
   - Monitor SAP_ALL usage
   - Report on critical authorization assignments
   - Track temporary access removals

3. **Custom Code Security Report**
   - Monitor custom program execution
   - Track changes to custom code
   - Correlate with GitHub code changes
   - Identify security-relevant modifications

### Scheduling and Sharing

Configure workbook distribution:

1. **Scheduling Snapshots**
   - Open the workbook
   - Click "Save" to ensure all changes are saved
   - Click the "Schedule" button (clock icon)
   - Configure schedule settings:
     - **Frequency**: Daily, weekly, or monthly
     - **Format**: PDF or PNG
     - **Recipients**: Email addresses
   - Click "Apply" to create the schedule

2. **Sharing with Stakeholders**
   - Open the workbook
   - Click "Share" button
   - Choose sharing method:
     - **Link**: Generate a direct link
     - **Dashboard**: Pin to Azure Dashboard
     - **Export**: Save as PDF or PNG
   - Configure permissions as needed
   - Click "Apply" to share

## Best Practices

1. **SAP System Coverage**
   - Include all production SAP systems
   - Prioritize business-critical systems
   - Add development systems for complete visibility
   - Group systems by landscape or business function

2. **Performance Considerations**
   - Limit queries to necessary time ranges
   - Use efficient KQL patterns
   - Consider data volume when designing visualizations
   - Implement materialized views for complex queries

3. **Security Context**
   - Add baseline information for reference
   - Include regulatory compliance context
   - Document your security policies within workbooks
   - Provide guidance for security responders

4. **Integration Strategy**
   - Correlate SAP events with other security data
   - Create links to related workbooks
   - Enable drill-down capabilities
   - Include business context for technical events

## Troubleshooting

### Common Issues and Solutions

- **Missing SAP Data**:
  - Verify SAP connector is functioning properly
  - Check log collection configuration
  - Confirm audit logging is enabled in SAP
  - Validate data retention settings

- **Visualization Issues**:
  - Check for null or empty fields in queries
  - Verify data types match visualization requirements
  - Adjust time ranges to ensure data availability
  - Test queries independently before adding to workbook

- **Performance Problems**:
  - Optimize KQL queries
  - Reduce the number of visualizations per tab
  - Use parameters to limit data scope
  - Consider splitting into multiple workbooks

## Next Steps

After configuring the SAP Security workbook, proceed to:

- [Transport Anomaly Rules](./transport-rules.md) - Configure rules for transport anomalies
- [Integration Dashboard](./integration-dashboard.md) - Set up comprehensive integration monitoring
- [Custom SAP Rules](./custom-sap-rules.md) - Implement SAP-specific detection rules

## References

- [Microsoft Sentinel for SAP Documentation](https://docs.microsoft.com/en-us/azure/sentinel/sap-solution)
- [SAP Security Audit Log Documentation](https://help.sap.com/viewer/2d84b32f84ed4c7ea14ad33dcfa2ee9d/7.5.9/en-US/c769bebf70804069b80acc0b76618c5d.html)
- [Kusto Query Language (KQL) Documentation](https://docs.microsoft.com/en-us/azure/data-explorer/kusto/query/)
- [SAP Security Monitoring Guide](https://www.sap.com/documents/2017/02/f049ccc6-a97c-0010-82c7-eda71af511fa.html)

---

**Document Metadata:**
- Last Updated: 2023-08-03
- Contributors: SAP Security Team, Microsoft Sentinel Team
- Version: 1.0.0 