# 📄 Transport Anomaly Rules for Microsoft Sentinel

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
This document provides detailed instructions for configuring analytics rules in Microsoft Sentinel to detect anomalies and security issues in SAP transport processes. These rules are especially important for the SAP-GitHub integration, where transports are triggered by or correlated with GitHub activities.

## ✅ Prerequisites
- Microsoft Sentinel workspace configured (refer to [Sentinel Workspace Setup](./sentinel-workspace.md))
- SAP connector deployed and configured (refer to [SAP Connector Setup](./sap-connector.md))
- GitHub connector deployed (refer to [GitHub Connector Setup](./github-connector.md))
- Standard rules deployed (refer to [Standard Rules Deployment](./standard-rules.md))
- Appropriate permissions to create and manage analytics rules

## Detailed Content

### Understanding SAP Transport Security Risks

SAP transports present several security concerns:

1. **Unauthorized Changes**
   - Unauthorized modifications to critical SAP objects
   - Transport of malicious or vulnerable code
   - Unauthorized access to production systems

2. **Segregation of Duties (SoD) Violations**
   - Same user developing and releasing transports
   - Same user creating and importing transports
   - Lack of proper review and approval process

3. **DevSecOps Process Violations**
   - Bypassing established development processes
   - Direct changes to production outside transport routes
   - Missing security reviews for critical transports

4. **Integration Anomalies**
   - Mismatch between GitHub commits and SAP transports
   - Inconsistent transport paths
   - Timing anomalies in transport release and import

### Configuring Transport Data Collection

Before creating analytics rules, ensure proper data collection:

1. **SAP Transport Logs**
   - Configure SAP Transport Collection in Sentinel connector
   - Enable logging for the following SAP tables:
     - E070 (Transport Header)
     - E071 (Transport Objects)
     - TMSQLCAT (Transport Status)
     - TMSQLAUDIT (Transport Audit)
   - Set appropriate data collection frequency

2. **GitHub Integration Events**
   - Ensure GitHub connector is collecting workflow events
   - Configure webhook notifications for transport-related actions
   - Validate data is flowing to Sentinel

3. **SAP Change Documents**
   - Enable SAP change document logging (table CDHDR, CDPOS)
   - Configure collection of change document data
   - Correlate with transport events

### Core Transport Anomaly Rules

Implement these essential rules to monitor transport anomalies:

1. **Unauthorized Transport Creation**
   ```kql
   SAPTransportEvents_CL
   | where TimeGenerated > ago(24h)
   | where EventType_s == "TRANSPORT_CREATED"
   | where User_s !in ((
       SecurityAllowList_CL
       | where AllowListType_s == "TransportAuthorized"
       | project User_s
   ))
   | project TimeGenerated, TransportID_s, User_s, SystemID_s, ObjectType_s, ObjectName_s
   ```

2. **Transport Release Anomalies**
   ```kql
   SAPTransportEvents_CL
   | where TimeGenerated > ago(7d)
   | where EventType_s == "TRANSPORT_RELEASED"
   | extend ReleaseTime = TimeGenerated
   | join kind=inner (
       SAPTransportEvents_CL
       | where EventType_s == "TRANSPORT_CREATED"
       | extend CreateTime = TimeGenerated
   ) on TransportID_s
   | extend TimeDiff = datetime_diff('minute', ReleaseTime, CreateTime)
   | where TimeDiff < 10
   | project TransportID_s, User_s, SystemID_s, TimeDiff, CreateTime, ReleaseTime
   ```

3. **Critical Object Transport Detection**
   ```kql
   let criticalObjects = dynamic([
     "SOBJ:PAYR", // Payroll objects
     "PROG:RFUMSV00", // Payment program
     "TABL:USR*", // User tables
     "TABL:T000", // Client table
     "FUGR:SU*" // User management function groups
   ]);
   SAPTransportEvents_CL
   | where TimeGenerated > ago(7d)
   | where EventType_s == "TRANSPORT_CREATED" or EventType_s == "TRANSPORT_RELEASED"
   | where ObjectType_s startswith_in (criticalObjects) or ObjectName_s startswith_in (criticalObjects)
   | project TimeGenerated, TransportID_s, User_s, SystemID_s, ObjectType_s, ObjectName_s, EventType_s
   ```

4. **Off-Hours Transport Activity**
   ```kql
   SAPTransportEvents_CL
   | where TimeGenerated > ago(7d)
   | extend hour = datetime_part("hour", TimeGenerated)
   | where hour < 6 or hour > 20 // Adjust based on your business hours
   | where EventType_s == "TRANSPORT_IMPORTED" or EventType_s == "TRANSPORT_RELEASED"
   | where User_s !in ((
       SecurityAllowList_CL
       | where AllowListType_s == "OffHoursAuthorized"
       | project User_s
   ))
   | project TimeGenerated, TransportID_s, User_s, SystemID_s, EventType_s
   ```

### GitHub Integration Monitoring Rules

Create these rules to detect anomalies in the GitHub-SAP transport integration:

1. **Missing GitHub Validation**
   ```kql
   SAPTransportEvents_CL
   | where TimeGenerated > ago(24h)
   | where EventType_s == "TRANSPORT_RELEASED"
   | join kind=leftouter (
       GitHubWorkflowEvents_CL
       | where event_type_s == "workflow_run" and conclusion_s == "success" 
       | where workflow_name_s == "transport-validation"
       | project TransportID = extract("TRID=([A-Z0-9]+)", 1, run_url_s)
   ) on $left.TransportID_s == $right.TransportID
   | where isempty(TransportID)
   | project TimeGenerated, TransportID_s, User_s, SystemID_s
   ```

2. **Commit-Transport Mismatch**
   ```kql
   let github_transports = GitHubRepoEvents_CL
   | where TimeGenerated > ago(3d)
   | where event_type_s == "push"
   | where commit_message_s contains "TRID=" 
   | extend TransportID = extract("TRID=([A-Z0-9]+)", 1, commit_message_s);
   SAPTransportEvents_CL
   | where TimeGenerated > ago(3d)
   | join kind=leftouter (github_transports) on $left.TransportID_s == $right.TransportID
   | where EventType_s == "TRANSPORT_RELEASED" 
   | where isempty(TransportID)
   | project TimeGenerated, TransportID_s, User_s, SystemID_s
   ```

3. **Transport Velocity Anomalies**
   ```kql
   // Get historical baselines for each user's transport velocity
   let userBaselines = SAPTransportEvents_CL
   | where TimeGenerated between(ago(90d)..ago(1d))
   | where EventType_s == "TRANSPORT_CREATED"
   | summarize TransportsPerDay = count() by User_s, bin(TimeGenerated, 1d)
   | summarize 
       avg_count = avg(TransportsPerDay),
       max_count = max(TransportsPerDay),
       stdev_count = stdev(TransportsPerDay)
       by User_s;
   // Calculate current transport velocity
   let current = SAPTransportEvents_CL
   | where TimeGenerated > ago(24h)
   | where EventType_s == "TRANSPORT_CREATED"
   | summarize CurrentCount = count() by User_s;
   // Join and detect anomalies
   current
   | join kind=inner userBaselines on User_s
   | extend threshold = avg_count + (2 * stdev_count) // 2 standard deviations
   | where CurrentCount > threshold and CurrentCount > 5 // At least 5 transports
   | project User_s, CurrentCount, avg_count, threshold
   ```

### Specialized Transport Rules for SAP-GitHub Integration

Implement these advanced rules for your integrated environment:

1. **Transport Approval Bypass Detection**
   ```kql
   GitHubPREvents_CL
   | where TimeGenerated > ago(7d)
   | where action_s == "closed" and state_s == "closed" and merged_s == "true"
   | extend TransportID = extract("TRID=([A-Z0-9]+)", 1, title_s)
   | where isnotempty(TransportID)
   | join kind=leftouter (
       GitHubReviewEvents_CL
       | where TimeGenerated > ago(7d)
       | where state_s == "approved"
       | project PR = number_d, Approved = "true"
   ) on $left.number_d == $right.PR
   | where isempty(Approved)
   | join kind=inner (
       SAPTransportEvents_CL
       | where EventType_s == "TRANSPORT_RELEASED"
   ) on $left.TransportID == $right.TransportID_s
   | project TimeGenerated, PR = number_d, TransportID, User_s
   ```

2. **Cross-System Transport Path Analysis**
   ```kql
   // Track the normal transport path
   let normalPath = dynamic(["DEV", "QAS", "PRD"]);
   // Get transport path for each transport
   let transportPaths = SAPTransportEvents_CL
   | where TimeGenerated > ago(30d)
   | where EventType_s == "TRANSPORT_IMPORTED"
   | summarize SystemList = make_list(SystemID_s), TimeList = make_list(TimeGenerated) by TransportID_s
   | extend PathString = array_strcat(SystemList, "->")
   | extend ExpectedPath = array_strcat(normalPath, "->");
   // Detect anomalies
   transportPaths
   | where PathString != ExpectedPath
   | extend IsAuthorized = PathString in ((
       SecurityAllowList_CL
       | where AllowListType_s == "AuthorizedPaths"
       | project PathString
   ))
   | where IsAuthorized == false
   | project TransportID_s, PathString, ExpectedPath, SystemList, TimeList
   ```

3. **Transport-User-GitHub Relationship Analysis**
   ```kql
   let github_transports = GitHubRepoEvents_CL
   | where TimeGenerated > ago(7d)
   | where event_type_s == "push"
   | extend TransportID = extract("TRID=([A-Z0-9]+)", 1, commit_message_s)
   | where isnotempty(TransportID)
   | extend GitHubUser = actor_s;
   let user_mappings = externaldata(SAPUser: string, GitHubUser: string)
   ["https://yourstorageaccount.blob.core.windows.net/sentinel/user_mappings.csv"];
   SAPTransportEvents_CL
   | where TimeGenerated > ago(7d)
   | join kind=inner github_transports on $left.TransportID_s == $right.TransportID
   | join kind=leftouter user_mappings on $left.User_s == $right.SAPUser and $left.GitHubUser == $right.GitHubUser
   | where isempty(SAPUser) // No matching user mapping found
   | project TimeGenerated, TransportID_s, SAPUser=User_s, GitHubUser, EventType_s
   ```

### Implementing and Tuning Rules

Follow these steps to implement the rules:

1. **Start with Basic Rules**
   - Begin with core transport rules
   - Monitor initial results and tune
   - Gradually add more complex rules

2. **Rule Customization**
   - Adjust thresholds based on your environment
   - Update user allowlists regularly
   - Customize critical object definitions

3. **Correlation and Enrichment**
   - Link transports to user identities
   - Correlate with change document information
   - Add business system context

4. **Incident Orchestration**
   - Configure entity mapping
   - Implement incident grouping
   - Set appropriate severities

## Best Practices

1. **Rule Tuning Guidelines**
   - Initialize rules in "audit mode" before alerting
   - Establish baseline metrics for normal behavior
   - Adjust thresholds based on baseline + acceptable deviation
   - Review and update rules quarterly

2. **False Positive Reduction**
   - Maintain accurate allowlists
   - Document expected deviations and exceptions
   - Implement automated exclusion feedback loops
   - Enrich alerts with context

3. **Investigation Workflow**
   - Document standard investigation procedures
   - Create playbooks for common transport issues
   - Establish escalation paths for critical findings
   - Include remediation steps

4. **Change Management Integration**
   - Link detection to change management process
   - Validate transports against approved changes
   - Document emergency transport procedures
   - Integrate with CAB (Change Advisory Board) processes

## Troubleshooting

### Common Issues and Solutions

- **Rule Not Triggering**:
  - Verify data collection from SAP and GitHub
  - Check KQL syntax for errors
  - Ensure time ranges are appropriate
  - Validate that thresholds match your environment

- **Too Many Alerts**:
  - Adjust thresholds based on observed patterns
  - Implement better user allowlists
  - Add additional filtering conditions
  - Consider using incident grouping

- **Missing GitHub Correlation**:
  - Check commit message formats
  - Verify GitHub webhook configuration
  - Ensure transport IDs are properly referenced
  - Validate user mapping data

## ➡️ Next Steps

After configuring transport anomaly rules, proceed to:

- [Incident Response](./incident-response.md) - Configure automated response to transport anomalies
- [Auth Rules](./auth-rules.md) - Set up authentication and authorization rules
- [Integration Dashboard](./integration-dashboard.md) - Monitor transport activities in a unified dashboard

## References

- [Microsoft Sentinel Analytics Rules Documentation](https://docs.microsoft.com/en-us/azure/sentinel/detect-threats-built-in)
- [SAP Transport Management System Documentation](https://help.sap.com/docs/basis/transport-management-system)
- [GitHub Security Best Practices](https://docs.github.com/en/enterprise-cloud@latest/admin/best-practices/best-practices-for-enterprise-security)
- [KQL Query Language Reference](https://docs.microsoft.com/en-us/azure/data-explorer/kusto/query/)

---




