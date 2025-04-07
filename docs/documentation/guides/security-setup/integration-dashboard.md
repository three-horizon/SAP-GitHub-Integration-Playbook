# 📄 Integration Dashboard for Microsoft Sentinel

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
This document provides detailed instructions for creating and configuring a comprehensive integration dashboard in Microsoft Sentinel to monitor the security posture of your SAP-GitHub integration. This unified dashboard combines data from SAP systems, GitHub repositories, and Azure resources to provide a holistic view of your integration's health and security status.

## ✅ Prerequisites
- Microsoft Sentinel workspace configured (refer to [Sentinel Workspace Setup](./sentinel-workspace.md))
- Data connectors configured for all components:
  - SAP connector (refer to [SAP Connector Setup](./sap-connector.md))
  - GitHub connector (refer to [GitHub Connector Setup](./github-connector.md))
  - Azure connector (refer to [Azure Connector Setup](./azure-connector.md))
- Component workbooks deployed:
  - SAP Security Workbook (refer to [SAP Workbook](./sap-workbook.md))
  - GitHub Activity Workbook (refer to [GitHub Workbook](./github-workbook.md))
- Appropriate permissions to create and edit workbooks

## Detailed Content

### Understanding Integration Monitoring

A comprehensive integration dashboard addresses these key areas:

1. **Cross-System Security Visibility**
   - Unified view of security events
   - Correlation of related activities
   - End-to-end process monitoring
   - Security control effectiveness

2. **Integration Health Monitoring**
   - Connection status
   - API performance
   - Authentication health
   - Data flow integrity

3. **Operational Metrics**
   - Transaction volumes
   - Processing times
   - Success/failure rates
   - Exception handling

4. **Business Process Context**
   - SAP to GitHub development flow
   - Transport management
   - Release processes
   - Change impact

### Creating the Integration Dashboard

Follow these steps to create a comprehensive integration dashboard:

1. **Create a New Workbook**
   - Navigate to Microsoft Sentinel > Workbooks
   - Click "Add workbook"
   - Click "Edit" to enter edit mode
   - Set an appropriate title: "SAP-GitHub Integration Dashboard"
   - Save the workbook

2. **Set Up Dashboard Structure**
   - Add sections using "Add text"
     - Overview
     - Integration Health
     - Security Events
     - Transport Activity
     - GitHub Activity
     - Cross-System Correlation
   - Add parameters using "Add parameter"
     - Time Range (7d default)
     - SAP System (multiselect)
     - GitHub Repository (multiselect)
     - Severity Filter (multiselect)

3. **Add Component Integration**
   - Link to other specialized workbooks:
     - Add link to SAP Security Workbook
     - Add link to GitHub Activity Workbook
     - Add link to Azure Security Workbook

### Building the Overview Section

Create a high-level summary of the integration:

1. **Add Key Metrics**
   ```kql
   // SAP Integration Stats
   let sapEvents = SAPSecurity_CL
   | where TimeGenerated > ago({TimeRange})
   | where SAPSystem_s in ({SAPSystem}) or '*' in ({SAPSystem});
   // GitHub Stats
   let githubEvents = GitHubAuditLogEvents_CL
   | where TimeGenerated > ago({TimeRange})
   | where repository_s in ({GitHubRepo}) or '*' in ({GitHubRepo});
   // API Stats
   let apiEvents = ApiManagementGatewayLogs
   | where TimeGenerated > ago({TimeRange})
   | where ApiId contains "SAP" or ApiId contains "GitHub";
   // Return summary metrics
   union
   (sapEvents | summarize SAP_Event_Count=count() | extend Metric = "SAP Events"),
   (githubEvents | summarize GitHub_Event_Count=count() | extend Metric = "GitHub Events"),
   (apiEvents | summarize API_Call_Count=count() | extend Metric = "API Calls")
   ```

2. **Add Security Summary**
   ```kql
   SecurityAlert
   | where TimeGenerated > ago({TimeRange})
   | where ProductName in ("Microsoft Sentinel", "Azure Active Directory", "Microsoft Defender for Cloud")
   | where AlertSeverity in ({Severity}) or "*" in ({Severity})
   | summarize Count=count() by AlertSeverity
   | order by Count desc
   | render piechart
   ```

3. **Add System Status Indicators**
   ```kql
   // Define expected systems and connectivity
   let expectedSystems = datatable(SystemType:string, SystemName:string)
   [
       "SAP", "PRD",
       "SAP", "QAS",
       "SAP", "DEV",
       "GitHub", "enterprise.github.com",
       "API", "api-management",
       "Azure", "key-vault"
   ];
   // Check latest heartbeats/activity
   let sapStatus = SAPConnectorHeartbeat_CL
   | where TimeGenerated > ago(1h)
   | summarize LastSeen=max(TimeGenerated) by SystemName=SAPSystem_s
   | extend SystemType = "SAP", Status=iff(LastSeen > ago(15m), "Healthy", "Unhealthy");
   let githubStatus = GitHubAuditLogEvents_CL
   | where TimeGenerated > ago(2h)
   | summarize LastSeen=max(TimeGenerated)
   | extend SystemType="GitHub", SystemName="enterprise.github.com", Status=iff(LastSeen > ago(30m), "Healthy", "Unhealthy");
   let apiStatus = ApiManagementGatewayLogs
   | where TimeGenerated > ago(2h)
   | summarize LastSeen=max(TimeGenerated)
   | extend SystemType="API", SystemName="api-management", Status=iff(LastSeen > ago(30m), "Healthy", "Unhealthy");
   // Combine and return status
   union sapStatus, githubStatus, apiStatus
   | join kind=fullouter expectedSystems on SystemType, SystemName
   | extend Status = coalesce(Status, "No Data")
   | project SystemType, SystemName, Status, LastSeen
   ```

### Building the Integration Health Section

Monitor the performance and reliability of integration components:

1. **API Management Performance**
   ```kql
   ApiManagementGatewayLogs
   | where TimeGenerated > ago({TimeRange})
   | where ApiId contains "SAP" or ApiId contains "GitHub"
   | summarize 
       Success = countif(IsRequestSuccess == true), 
       Failure = countif(IsRequestSuccess == false), 
       Latency = avg(BackendTime)
       by ApiId, bin(TimeGenerated, 1h)
   | render timechart
   ```

2. **Integration Point Failures**
   ```kql
   union
   (ApiManagementGatewayLogs
   | where TimeGenerated > ago({TimeRange})
   | where ApiId contains "SAP" or ApiId contains "GitHub"
   | where IsRequestSuccess == false
   | extend ErrorSource = "API Management", ErrorDetails = tostring(ErrorMessage)),
   (FunctionAppLogs
   | where TimeGenerated > ago({TimeRange})
   | where FunctionName contains "SAP" or FunctionName contains "GitHub"
   | where Level == "Error"
   | extend ErrorSource = "Azure Function", ErrorDetails = Message)
   | order by TimeGenerated desc
   | project TimeGenerated, ErrorSource, ErrorDetails
   ```

3. **Authentication Health**
   ```kql
   let sapAuth = SAPSecurity_CL
   | where TimeGenerated > ago({TimeRange})
   | where EventType_s in ("AUDIT_SUCCESS", "AUDIT_FAILURE") 
   | where EventSubType_s in ("LOGIN_SUCCESSFUL", "LOGIN_FAILED")
   | summarize 
       SuccessCount = countif(EventSubType_s == "LOGIN_SUCCESSFUL"),
       FailCount = countif(EventSubType_s == "LOGIN_FAILED")
       by bin(TimeGenerated, 1h)
   | extend System = "SAP";
   let githubAuth = GitHubAuditLogEvents_CL
   | where TimeGenerated > ago({TimeRange})
   | where Action_s in ("user.login", "user.failed_login")
   | summarize 
       SuccessCount = countif(Action_s == "user.login"),
       FailCount = countif(Action_s == "user.failed_login")
       by bin(TimeGenerated, 1h)
   | extend System = "GitHub";
   let azureAuth = SigninLogs
   | where TimeGenerated > ago({TimeRange})
   | summarize 
       SuccessCount = countif(ResultType == 0),
       FailCount = countif(ResultType != 0)
       by bin(TimeGenerated, 1h)
   | extend System = "Azure AD";
   union sapAuth, githubAuth, azureAuth
   | order by TimeGenerated asc
   | render timechart
   ```

### Building the Transport Activity Section

Monitor SAP transport activities and their integration with GitHub:

1. **Transport Volume Dashboard**
   ```kql
   SAPTransportEvents_CL
   | where TimeGenerated > ago({TimeRange})
   | where SAPSystem_s in ({SAPSystem}) or '*' in ({SAPSystem})
   | summarize count() by EventType_s, bin(TimeGenerated, 1d)
   | render columnchart
   ```

2. **Transport to GitHub Traceability**
   ```kql
   // Get all transports
   let transports = SAPTransportEvents_CL
   | where TimeGenerated > ago({TimeRange})
   | where SAPSystem_s in ({SAPSystem}) or '*' in ({SAPSystem})
   | where EventType_s in ("TRANSPORT_CREATED", "TRANSPORT_RELEASED", "TRANSPORT_IMPORTED")
   | distinct TransportID_s, EventType_s;
   // Get all GitHub commits with transport references
   let github_refs = GitHubRepoEvents_CL
   | where TimeGenerated > ago({TimeRange})
   | where repository_s in ({GitHubRepo}) or '*' in ({GitHubRepo})
   | where event_type_s == "push"
   | where commit_message_s matches regex "TR[0-9]{6,}" or commit_message_s contains "TRID="
   | extend TransportID = extract("(TR[0-9]{6,})", 1, commit_message_s)
   | where isnotempty(TransportID)
   | distinct TransportID, repository_s;
   // Combine and analyze
   transports
   | summarize TransportTypes=make_set(EventType_s) by TransportID=TransportID_s
   | join kind=leftouter github_refs on TransportID
   | extend HasGitHubReference = isnotempty(repository_s)
   | summarize 
       Total=count(), 
       WithGitHubRef=countif(HasGitHubReference), 
       WithoutGitHubRef=countif(HasGitHubReference == false),
       RepositoryCoverage=make_set_if(repository_s, isnotempty(repository_s))
   ```

3. **Transport Error Detection**
   ```kql
   SAPSecurity_CL
   | where TimeGenerated > ago({TimeRange})
   | where SAPSystem_s in ({SAPSystem}) or '*' in ({SAPSystem})
   | where EventType_s == "AUDIT_FAILURE" 
   | where EventText_s contains "TRANSPORT" or TransactionCode_s in ("STMS", "SE01", "SE09", "SE10")
   | project TimeGenerated, User_s, SAPSystem_s, Client_s, EventText_s
   ```

### Building the GitHub Activity Section

Monitor GitHub events related to SAP development:

1. **Repository Activity Overview**
   ```kql
   GitHubRepoEvents_CL
   | where TimeGenerated > ago({TimeRange})
   | where repository_s in ({GitHubRepo}) or '*' in ({GitHubRepo})
   | summarize count() by repository_s, event_type_s
   | order by repository_s, count_ desc
   ```

2. **SAP-Related Commits**
   ```kql
   GitHubRepoEvents_CL
   | where TimeGenerated > ago({TimeRange})
   | where repository_s in ({GitHubRepo}) or '*' in ({GitHubRepo})
   | where event_type_s == "push"
   | where commit_message_s contains "SAP" 
      or commit_message_s matches regex "TR[0-9]{6,}" 
      or commit_message_s contains "TRID="
      or file_path_s endswith ".abap"
   | extend isSAPRelated = true
   | project TimeGenerated, repository_s, actor_s, commit_id_s, commit_message_s, isSAPRelated
   ```

3. **GitHub Workflow Status**
   ```kql
   GitHubWorkflowEvents_CL
   | where TimeGenerated > ago({TimeRange})
   | where repository_s in ({GitHubRepo}) or '*' in ({GitHubRepo})
   | summarize 
       SuccessRuns = countif(conclusion_s == "success"),
       FailedRuns = countif(conclusion_s == "failure"), 
       TotalRuns = count()
       by workflow_name_s, repository_s
   | extend SuccessRate = (SuccessRuns * 100.0 / TotalRuns)
   | order by SuccessRate asc
   ```

### Building the Cross-System Correlation Section

Create visualizations that connect activities across systems:

1. **End-to-End Change Flow**
   ```kql
   // Get latest change events across systems
   union
   (SAPTransportEvents_CL
   | where TimeGenerated > ago({TimeRange})
   | where SAPSystem_s in ({SAPSystem}) or '*' in ({SAPSystem})
   | where EventType_s in ("TRANSPORT_CREATED", "TRANSPORT_RELEASED", "TRANSPORT_IMPORTED")
   | extend TransportID = TransportID_s
   | extend ChangeID = TransportID
   | extend SystemType = "SAP"
   | extend Action = EventType_s),
   (GitHubRepoEvents_CL
   | where TimeGenerated > ago({TimeRange})
   | where repository_s in ({GitHubRepo}) or '*' in ({GitHubRepo})
   | where event_type_s in ("push", "pull_request", "pull_request_review")
   | extend TransportID = extract("(TR[0-9]{6,})", 1, coalesce(commit_message_s, title_s, ""))
   | where isnotempty(TransportID)
   | extend ChangeID = TransportID
   | extend SystemType = "GitHub"
   | extend Action = event_type_s)
   | project TimeGenerated, ChangeID, SystemType, Action
   | order by ChangeID asc, TimeGenerated asc
   ```

2. **User Activity Correlation**
   ```kql
   // Map users across systems
   let userMappings = externaldata(SAPUser:string, GitHubUser:string, AADUser:string)
   ["https://yourstorageaccount.blob.core.windows.net/sentinel/user_mappings_full.csv"];
   // Get SAP activities
   let sapActivity = SAPSecurity_CL
   | where TimeGenerated > ago({TimeRange})
   | where SAPSystem_s in ({SAPSystem}) or '*' in ({SAPSystem})
   | summarize SAPCount=count() by SAPUser=User_s, bin(TimeGenerated, 1h);
   // Get GitHub activities
   let githubActivity = GitHubAuditLogEvents_CL
   | where TimeGenerated > ago({TimeRange})
   | where repository_s in ({GitHubRepo}) or '*' in ({GitHubRepo})
   | summarize GitHubCount=count() by GitHubUser=actor_s, bin(TimeGenerated, 1h);
   // Correlate activities
   sapActivity
   | join kind=inner userMappings on SAPUser
   | join kind=inner githubActivity on $left.GitHubUser == $right.GitHubUser, TimeGenerated
   | project TimeGenerated, SAPUser, GitHubUser, SAPCount, GitHubCount
   | order by TimeGenerated desc
   ```

3. **Cross-System Security Events**
   ```kql
   // Get security events across systems
   union
   (SAPSecurity_CL
   | where TimeGenerated > ago({TimeRange})
   | where EventType_s == "AUDIT_FAILURE"
   | extend User = User_s, System = "SAP", EventType = EventType_s, Details = EventText_s),
   (GitHubAuditLogEvents_CL
   | where TimeGenerated > ago({TimeRange})
   | where Action_s in ("repo.destroy", "repo.remove_member", "user.failed_login")
   | extend User = actor_s, System = "GitHub", EventType = Action_s, Details = ""),
   (SecurityAlert
   | where TimeGenerated > ago({TimeRange})
   | where AlertSeverity in ({Severity}) or "*" in ({Severity})
   | where ProductName in ("Microsoft Sentinel", "Azure Active Directory")
   | extend User = tostring(Entities[0].Name), System = ProductName, EventType = AlertType, Details = Description)
   | project TimeGenerated, System, User, EventType, Details
   | order by TimeGenerated desc
   ```

### Creating Advanced Visualizations

Add sophisticated visuals to enhance understanding:

1. **Process Flow Diagram**
   - Add a flow diagram using "Add text" with Mermaid syntax:
   ```
   mermaid
   graph LR
       A[SAP Developer] -->|Create/Modify| B[ABAP Code]
       B -->|Transport| C[Transport Request]
       C -->|Sync| D[GitHub Repository]
       D -->|Pull Request| E[Code Review]
       E -->|Merge| F[Main Branch]
       F -->|CI/CD| G[Deploy]
       G -->|Release| H[Production SAP]
   ```

2. **Integration Health Heatmap**
   ```kql
   // Define expected integration points
   let integrationPoints = datatable(Source:string, Target:string, ExpectedVolume:int)
   [
       "SAP DEV", "GitHub", 100,
       "GitHub", "Azure Pipeline", 50,
       "Azure Pipeline", "SAP QAS", 30,
       "SAP QAS", "Azure Pipeline", 10,
       "Azure Pipeline", "SAP PRD", 5
   ];
   // Get actual integration metrics
   let actualData = union
   (SAPTransportEvents_CL 
   | where TimeGenerated > ago({TimeRange})
   | where EventType_s == "TRANSPORT_IMPORTED"
   | summarize count() by Source = SAPSystem_s, Target = "GitHub"),
   (GitHubWorkflowEvents_CL
   | where TimeGenerated > ago({TimeRange})
   | summarize count() by Source = "GitHub", Target = "Azure Pipeline")
   // Add more integration points as needed
   ;
   // Combine and calculate health
   actualData
   | join kind=fullouter integrationPoints on Source, Target
   | extend 
       ActualVolume = coalesce(count_, 0),
       HealthScore = iff(ActualVolume >= ExpectedVolume * 0.8, 
           iff(ActualVolume <= ExpectedVolume * 1.2, 100, 80), 
           (ActualVolume * 100.0 / (ExpectedVolume * 0.8)))
   | project Source, Target, ExpectedVolume, ActualVolume, HealthScore
   | order by HealthScore asc
   | render columnchart with (kind=heatmap)
   ```

3. **Time-Based Funnel Visualization**
   ```kql
   let timeWindow = {TimeRange};
   union
   (SAPTransportEvents_CL
   | where TimeGenerated > ago(timeWindow)
   | where EventType_s == "TRANSPORT_CREATED"
   | summarize TransportsCreated=count()),
   (SAPTransportEvents_CL
   | where TimeGenerated > ago(timeWindow)
   | where EventType_s == "TRANSPORT_RELEASED"
   | summarize TransportsReleased=count()),
   (GitHubRepoEvents_CL
   | where TimeGenerated > ago(timeWindow)
   | where event_type_s == "push"
   | where commit_message_s matches regex "TR[0-9]{6,}" or commit_message_s contains "TRID="
   | summarize CommitsMade=count()),
   (GitHubRepoEvents_CL
   | where TimeGenerated > ago(timeWindow)
   | where event_type_s == "pull_request" and state_s == "closed" and merged_s == "true"
   | summarize PRsMerged=count()),
   (SAPTransportEvents_CL
   | where TimeGenerated > ago(timeWindow)
   | where SAPSystem_s == "PRD" and EventType_s == "TRANSPORT_IMPORTED"
   | summarize ProductionImports=count())
   | evaluate bag_unpack(*)
   | project Stage=pack_array(
       "Created", 
       "Released", 
       "Committed", 
       "Merged", 
       "Deployed"),
       Value=pack_array(
       coalesce(TransportsCreated, 0), 
       coalesce(TransportsReleased, 0), 
       coalesce(CommitsMade, 0), 
       coalesce(PRsMerged, 0), 
       coalesce(ProductionImports, 0))
   | mv-expand Stage, Value
   | order by array_index_of(pack_array(
       "Created", 
       "Released", 
       "Committed", 
       "Merged", 
       "Deployed"), tostring(Stage)) asc
   | render columnchart
   ```

### Scheduling and Sharing

Configure workbook distribution:

1. **Export Options**
   - Configure PDF export settings
   - Set up scheduled reports
   - Create snapshot schedules
   - Provide direct link sharing

2. **Dashboard Integration**
   - Pin to Azure Dashboard
   - Create Operations Center view
   - Configure shared team view
   - Set up executive dashboard

3. **Alert Integration**
   - Link from alert details
   - Provide context for investigations
   - Enable quick pivot to relevant data
   - Add incident response guidance

## Best Practices

1. **Dashboard Design**
   - Focus on user-centric design
   - Organize by use case, not data source
   - Include clear explanations
   - Provide interactivity and drill-down

2. **Performance Optimization**
   - Use efficient KQL patterns
   - Limit time range parameters
   - Implement query caching
   - Break complex dashboards into multiple tabs

3. **Actionable Insights**
   - Connect data to business outcomes
   - Highlight exceptions and anomalies
   - Provide clear next steps
   - Include thresholds and expected values

4. **Governance**
   - Document all queries
   - Maintain change log
   - Schedule regular reviews
   - Document dashboard update process

## Troubleshooting

### Common Issues and Solutions

- **Missing Data**:
  - Verify data connectors are properly configured
  - Check time ranges in queries match parameters
  - Ensure table and field names are correct
  - Validate data collection is functioning

- **Performance Issues**:
  - Optimize KQL queries
  - Reduce number of visualizations per tab
  - Implement time-based filtering
  - Consider materialized views for complex queries

- **Inconsistent Data**:
  - Check for timezone differences
  - Validate user mapping files
  - Ensure consistent field mapping
  - Verify synchronization between systems

## ➡️ Next Steps

After creating the integration dashboard, proceed to:

- [Executive Reporting](./executive-dashboard.md) - Create executive summary views
- [Security Governance](./security-governance.md) - Establish governance frameworks
- [Notification Configuration](./notifications.md) - Configure alerts based on dashboard metrics
- [Audit Configuration](./audit-setup.md) - Set up comprehensive auditing

## References

- [Microsoft Sentinel Workbooks Documentation](https://docs.microsoft.com/en-us/azure/sentinel/monitor-your-data)
- [Kusto Query Language Documentation](https://docs.microsoft.com/en-us/azure/data-explorer/kusto/query/)
- [Azure Monitor Workbooks](https://docs.microsoft.com/en-us/azure/azure-monitor/visualize/workbooks-overview)
- [Data Visualization Best Practices](https://docs.microsoft.com/en-us/power-bi/fundamentals/desktop-accessibility-creating-reports)
- [Dashboard Design Guidelines](https://docs.microsoft.com/en-us/azure/architecture/best-practices/monitoring)

---

**Document Metadata:**
- Last Updated: 2023-08-25
- Contributors: Integration Team, Monitoring Team
- Version: 1.2.0 
---

<details>
<summary><strong>📊 Document Metadata</strong></summary>

- **Last Updated:** 2025-04-07
- **Version:** 1.0.0
- **Status:** Published
</details>
