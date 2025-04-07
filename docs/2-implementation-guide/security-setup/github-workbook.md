# 📄 GitHub Activity Workbook for Microsoft Sentinel

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
This document provides detailed instructions for implementing and customizing the GitHub Activity Workbook in Microsoft Sentinel. This interactive dashboard enables security teams to visualize, analyze, and investigate GitHub activities related to your SAP development and deployment processes, helping identify security issues and suspicious behaviors.

## ✅ Prerequisites
- Microsoft Sentinel workspace configured (refer to [Sentinel Workspace Setup](./sentinel-workspace.md))
- GitHub data collection configured (refer to [GitHub Connector Setup](./github-connector.md))
- Data flowing successfully from GitHub to Sentinel
- Appropriate permissions to create and edit workbooks

## Detailed Content

### Workbook Fundamentals

Microsoft Sentinel workbooks are:

1. **Interactive Dashboards**
   - Visualize data with charts, graphs, and tables
   - Apply filters to focus on specific data points
   - Drill down from summary to detailed information

2. **Investigation Tools**
   - Perform ad-hoc queries during investigations
   - Pivot between different data types
   - Correlate events across multiple sources

3. **Reporting Solutions**
   - Create scheduled reports
   - Export data for compliance purposes
   - Share insights with stakeholders

### Deploying the GitHub Activity Workbook

1. **Using the Template Gallery**
   - Navigate to Microsoft Sentinel > Workbooks
   - Click "Templates" to open the gallery
   - Search for "GitHub" in the template gallery
   - Locate the "GitHub Activity" workbook template
   - Click "Save" to save a copy to your workspace
   - Click "View saved workbook" to open it

2. **Manual Import (Alternative Method)**
   - Download the GitHub Activity workbook ARM template from:
     ```
     https://raw.githubusercontent.com/Azure/Azure-Sentinel/master/Workbooks/GitHub/GitHubActivity.json
     ```
   - In Microsoft Sentinel, navigate to Workbooks
   - Click "Add workbook"
   - Click the "Edit" button in the toolbar
   - Click the "Advanced Editor" button (</> icon)
   - Replace the existing content with the downloaded template
   - Click "Apply" and then "Save"

### Workbook Contents and Features

The GitHub Activity workbook includes the following key sections:

1. **Activity Overview**
   - Total repositories monitored
   - Active users over time
   - Event distribution by type
   - Activity trends over time

2. **User Activity Analysis**
   - Most active users
   - User activity patterns
   - New and inactive users
   - User permission changes

3. **Repository Activity**
   - Most active repositories
   - Branch creation and deletion
   - Pull request statistics
   - Commit volumes and patterns

4. **Security Event Analysis**
   - Failed authentication attempts
   - Secret scanning alerts
   - Code scanning results
   - Permission changes

5. **CI/CD Pipeline Activity**
   - Workflow run statistics
   - Failed workflow analysis
   - Deployment frequencies
   - Integration with SAP transports

### Customizing the GitHub Workbook

Modify the workbook to match your organization's needs:

1. **Editing Existing Visualizations**
   - Navigate to the workbook and click "Edit"
   - Locate the visualization to modify
   - Click the "Edit" button on that visualization
   - Modify the KQL query, chart type, or layout
   - Click "Done Editing" to save changes

2. **Adding New Visualizations**
   - In edit mode, click "Add" and select "Add query"
   - Write a KQL query for the data you want to visualize
   - Select the visualization type (chart, grid, etc.)
   - Configure visualization settings
   - Click "Done Editing"

3. **Creating Custom Sections**
   - In edit mode, click "Add" and select "Add text"
   - Add section headers using markdown
   - Click "Done Editing"
   - Add relevant visualizations under the header

4. **Adding Parameters**
   - Click "Add" and select "Add parameter"
   - Configure parameter type (dropdown, time range, text)
   - Define parameter values
   - Reference parameters in queries using {ParameterName}

### Essential KQL Queries for GitHub Monitoring

Here are key queries to include in your workbook:

1. **Repository Activity Overview**
   ```kql
   GitHubRepoEvents_CL
   | summarize count() by Repository=repository_name_s, EventType=event_type_s
   | extend RepoEventKey=strcat(Repository, ":", EventType)
   | order by Repository asc, count_ desc
   | render barchart
   ```

2. **User Activity Heatmap**
   ```kql
   GitHubAuditLogEvents_CL
   | where TimeGenerated > ago(7d)
   | summarize EventCount=count() by User=actor_s, bin(TimeGenerated, 1h)
   | render heatmap
   ```

3. **Pull Request Analysis**
   ```kql
   GitHubPREvents_CL
   | where TimeGenerated > ago(30d)
   | summarize 
       PRsOpened=countif(action_s == "opened"), 
       PRsMerged=countif(action_s == "closed" and state_s == "closed"),
       PRsRejected=countif(action_s == "closed" and state_s == "open")
       by Repository=repository_name_s, bin(TimeGenerated, 1d)
   | render timechart
   ```

4. **Workflow Reliability Tracking**
   ```kql
   GitHubWorkflowEvents_CL
   | where TimeGenerated > ago(14d)
   | summarize 
       SuccessCount=countif(conclusion_s == "success"),
       FailureCount=countif(conclusion_s == "failure")
       by Workflow=workflow_name_s
   | extend SuccessRate=(SuccessCount * 100.0 / (SuccessCount + FailureCount))
   | order by SuccessRate asc
   | project Workflow, SuccessCount, FailureCount, SuccessRate
   ```

5. **Security Event Tracker**
   ```kql
   GitHubSecurityEvents_CL
   | where TimeGenerated > ago(30d)
   | summarize count() by AlertType=alert_type_s, Repository=repository_name_s
   | order by count_ desc
   ```

### SAP-GitHub Integration Visualizations

Add these specialized queries to monitor the integration between SAP and GitHub:

1. **Transport-to-Commit Correlation**
   ```kql
   let github_commits = GitHubRepoEvents_CL
   | where event_type_s == "push";
   let sap_transports = SAPTransportEvents_CL;
   github_commits
   | join kind=inner sap_transports on $left.commit_message_s contains $right.TransportID_s
   | project TimeGenerated, Repository=repository_name_s, TransportID=TransportID_s, 
     CommitUser=actor_s, TransportUser=UserName_s, CommitMessage=commit_message_s
   ```

2. **Pull Request to Transport Timeline**
   ```kql
   let pr_events = GitHubPREvents_CL;
   let transport_events = SAPTransportEvents_CL;
   pr_events
   | join kind=inner transport_events on $left.title_s contains $right.TransportID_s
   | project PRTime=pr_events.TimeGenerated, TransportTime=transport_events.TimeGenerated, 
     Repository=repository_name_s, TransportID=TransportID_s, PR=number_d
   | extend TimeDifference=datetime_diff('minute', TransportTime, PRTime)
   | render timechart
   ```

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

2. **Sharing the Workbook**
   - Open the workbook
   - Click "Share" button
   - Choose sharing method:
     - **Link**: Generate a direct link
     - **Dashboard**: Pin to Azure Dashboard
     - **Export**: Save as PDF or PNG
   - Configure permissions as needed
   - Click "Apply" to share

## Best Practices

1. **Performance Optimization**
   - Limit time ranges to necessary periods
   - Use efficient KQL patterns
   - Apply filters early in queries
   - Create separate workbooks for different use cases

2. **Visual Design**
   - Use consistent color schemes
   - Organize related visualizations together
   - Include clear titles and descriptions
   - Use appropriate visualization types for data

3. **Usability**
   - Add instructions for interpreting data
   - Include links to related workbooks
   - Provide context with text sections
   - Use parameters for flexible filtering

4. **Governance**
   - Document customizations
   - Implement version control
   - Test queries before adding to workbook
   - Review and update regularly

## Troubleshooting

### Common Issues and Solutions

- **No Data Displayed**:
  - Verify GitHub data is flowing to Log Analytics
  - Check time range settings
  - Validate KQL queries independently
  - Ensure table names match your environment

- **Performance Issues**:
  - Simplify complex queries
  - Reduce the time range
  - Use summarize and project operators efficiently
  - Split into multiple workbooks if needed

- **Rendering Problems**:
  - Try different chart types
  - Check for null or incomplete data
  - Verify column names in queries
  - Use render operators explicitly

## ➡️ Next Steps

After configuring the GitHub Activity workbook, proceed to:

- [SAP Security Workbook](./sap-workbook.md) - Implement SAP security monitoring dashboard
- [Integration Dashboard](./integration-dashboard.md) - Set up comprehensive integration monitoring
- [Executive Reporting](./executive-dashboard.md) - Configure executive-level security reporting

## References

- [Microsoft Sentinel Workbooks Documentation](https://docs.microsoft.com/en-us/azure/sentinel/monitor-your-data)
- [Kusto Query Language (KQL) Documentation](https://docs.microsoft.com/en-us/azure/data-explorer/kusto/query/)
- [GitHub Security Best Practices](https://docs.github.com/en/enterprise-cloud@latest/admin/best-practices/best-practices-for-enterprise-security)
- [Data Visualization Best Practices](https://docs.microsoft.com/en-us/azure/architecture/data-guide/big-data/real-time-processing)

---

**Document Metadata:**
- Last Updated: 2023-07-28
- Contributors: Security Engineering Team, GitHub Integration Team
- Version: 1.0.0 
---

<details>
<summary><strong>📊 Document Metadata</strong></summary>

- **Last Updated:** 2025-04-07
- **Version:** 1.0.0
- **Status:** Published
</details>
