# 📄 Custom SAP Rules for Microsoft Sentinel

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
This document provides detailed instructions for developing and implementing custom analytics rules in Microsoft Sentinel to detect SAP-specific security threats in your environment. These specialized rules complement standard detection capabilities and address unique security requirements for SAP systems integrated with GitHub.

## ✅ Prerequisites
- Microsoft Sentinel workspace configured (refer to [Sentinel Workspace Setup](./sentinel-workspace.md))
- SAP connector deployed and configured (refer to [SAP Connector Setup](./sap-connector.md))
- Standard rules deployed (refer to [Standard Rules Deployment](./standard-rules.md))
- SAP system expertise and understanding of common security risks
- Familiarity with Kusto Query Language (KQL)

## Detailed Content

### Understanding SAP Security Risks

SAP systems present several unique security concerns:

1. **Excessive Authorizations**
   - SAP_ALL and SAP_NEW profiles
   - Sensitive authorization objects
   - Inappropriate role assignments
   - Emergency access management

2. **Critical Configuration Changes**
   - System parameter modifications
   - Security settings changes
   - Client settings modifications
   - RFC destination alterations

3. **Sensitive Transactions**
   - Direct table access
   - System administration
   - User management
   - Development tools
   - Transport management

4. **Custom ABAP Code**
   - Unauthorized code changes
   - Vulnerable coding patterns
   - Backdoor implementation
   - Unauthorized client code

### SAP Log Sources for Detection

Effective detection relies on these key SAP log sources:

1. **Security Audit Log (SM19/SM20)**
   - User authentication events
   - Critical transaction usage
   - Authorization checks
   - System configuration changes

2. **System Log (SM21)**
   - System errors
   - Background jobs
   - Interface errors
   - Gateway activity

3. **Change Documents (CDHDR/CDPOS)**
   - Configuration changes
   - Master data modifications
   - Transport activities
   - User management changes

4. **Table Access Logging**
   - Sensitive data access
   - Direct table modifications
   - Client access

### Creating Custom SAP Detection Rules

Implement these specialized rules for comprehensive SAP security monitoring:

1. **Excessive Authorization Assignment**
   ```kql
   SAPSecurity_CL
   | where TimeGenerated > ago(7d)
   | where EventType_s == "AUDIT_AUTHORIZATION" 
   | where EventSubType_s == "ROLE_CHANGE"
   | extend AuthObj = extract("([A-Z_0-9]+)", 1, EventText_s)
   | where AuthObj in ("S_ADMI_FCD", "S_DEVELOP", "S_TABU_DIS", "S_USER_GRP")
   | summarize count() by User_s, SAPSystem_s, Client_s, bin(TimeGenerated, 1d)
   | join kind=leftouter (
       SecurityAllowList_CL
       | where AllowListType_s == "AuthorizedRoleAdmins"
       | project User_s
   ) on User_s
   | where isempty(User_s1)
   | project TimeGenerated, User_s, SAPSystem_s, Client_s, count_
   ```

2. **Critical Table Access Detection**
   ```kql
   let criticalTables = dynamic(["USR02", "USH02", "USGRP", "AGR_DEFINE", "AGR_1016", 
     "SAPUSER", "USR40", "DEVACCESS", "TMSQDCLS", "RSECTAB", "UST12"]);
   SAPSecurity_CL
   | where TimeGenerated > ago(7d)
   | where (TransactionCode_s in ("SE16", "SE16N", "SE17", "SM30", "SM31")) 
   | extend TableName = extract("([A-Z_0-9]+)", 1, EventText_s)
   | where TableName in (criticalTables)
   | summarize count() by User_s, TableName, SAPSystem_s, Client_s, bin(TimeGenerated, 1d)
   ```

3. **ABAP Debugging in Production**
   ```kql
   SAPSecurity_CL
   | where TimeGenerated > ago(7d)
   | where TransactionCode_s in ("SE24", "SE37", "SE38", "SE80") 
        or EventText_s contains "DEBUGGER" 
        or EventText_s contains "BREAKPOINT"
   | where SAPSystem_s in ("PRD", "PROD") // Production systems
   | join kind=leftouter (
       SecurityAllowList_CL
       | where AllowListType_s == "AuthorizedDebuggers"
       | project User_s
   ) on User_s
   | where isempty(User_s1)
   | project TimeGenerated, User_s, TransactionCode_s, SAPSystem_s, Client_s, EventText_s
   ```

4. **RFC Destination Changes**
   ```kql
   SAPSecurity_CL
   | where TimeGenerated > ago(7d)
   | where TransactionCode_s in ("SM59", "STRUST") 
       or EventText_s contains "RFC DESTINATION" 
       or EventText_s contains "TRUSTED SYSTEM"
   | project TimeGenerated, User_s, TransactionCode_s, SAPSystem_s, Client_s, EventText_s
   ```

5. **Off-Hours Development Activities**
   ```kql
   SAPSecurity_CL
   | where TimeGenerated > ago(7d)
   | where TransactionCode_s in ("SE01", "SE09", "SE10", "SE11", "SE37", "SE80", "SE38", "STMS")
   | extend hour = datetime_part("hour", TimeGenerated)
   | where hour < 6 or hour > 22 // Adjust to your business hours
   | join kind=leftouter (
       SecurityAllowList_CL
       | where AllowListType_s == "AuthorizedOffHoursDevelopers"
       | project User_s
   ) on User_s
   | where isempty(User_s1)
   | project TimeGenerated, hour, User_s, TransactionCode_s, SAPSystem_s, Client_s
   ```

### Advanced SAP Custom Rules

Implement these rules for deeper security visibility:

1. **SAP Profile Parameter Changes**
   ```kql
   SAPChangeLog_CL
   | where TimeGenerated > ago(30d)
   | where TableName_s == "TPFKT" or ObjectName_s == "PROFILE_PARAMETER"
   | extend Parameter = extract("PARAMETER ([A-Za-z0-9_]+)", 1, ChangeText_s)
   | project TimeGenerated, User_s, SAPSystem_s, Parameter, ChangeText_s
   ```

2. **SAP User Attribute Anomalies**
   ```kql
   // Get baseline of normal user attributes
   let userBaseline = SAPUserAttributes_CL
   | where TimeGenerated > ago(90d) and TimeGenerated < ago(1d)
   | summarize
       AvgValidityDays = avg(ValidityDays_d),
       StdDevValidityDays = stdev(ValidityDays_d)
       by UserType_s;
   // Detect anomalies
   SAPUserAttributes_CL
   | where TimeGenerated > ago(1d)
   | join kind=inner userBaseline on UserType_s
   | extend ValidityDaysThreshold = AvgValidityDays + (3 * StdDevValidityDays)
   | where ValidityDays_d > ValidityDaysThreshold or ValidityDays_d < 1
   | project TimeGenerated, User_s, UserType_s, ValidityDays_d, ValidityDaysThreshold
   ```

3. **Suspicious SAP Client Creation or Modification**
   ```kql
   let clientChanges = SAPChangeLog_CL
   | where TimeGenerated > ago(7d)
   | where TableName_s == "T000" or TableName_s == "T001"
   | project TimeGenerated, User_s, SAPSystem_s, ChangeType_s, ChangeText_s;
   let clientConfig = SAPChangeLog_CL
   | where TimeGenerated > ago(7d)
   | where TableName_s == "T000C" or TableName_s == "T001C"
   | project TimeGenerated, User_s, SAPSystem_s, ChangeType_s, ChangeText_s;
   union clientChanges, clientConfig
   | project TimeGenerated, User_s, SAPSystem_s, ChangeType_s, ChangeText_s
   ```

4. **SAP Authorizations vs. GitHub Activity Correlation**
   ```kql
   // First get users with sensitive authorizations in SAP
   let sapAdmins = SAPSecurity_CL
   | where TimeGenerated > ago(30d)
   | where EventType_s == "AUDIT_AUTHORIZATION"
   | where EventText_s contains "S_ADMI" or EventText_s contains "S_USER" or EventText_s contains "S_DEVELOP"
   | summarize by SAPUser=User_s;
   // Map SAP users to GitHub users
   let userMapping = externaldata(SAPUser:string, GitHubUser:string)
   ["https://yourstorageaccount.blob.core.windows.net/sentinel/user_mappings.csv"];
   // Find suspicious GitHub activities by SAP admins
   sapAdmins
   | join kind=inner userMapping on SAPUser
   | join kind=inner (
       GitHubAuditLogEvents_CL
       | where TimeGenerated > ago(7d)
       | where Action_s in ("repo.add_member", "team.add_member", "org.add_member", "org.update_member", "repo.create")
   ) on $left.GitHubUser == $right.actor_s
   | project TimeGenerated, SAPUser, GitHubUser=actor_s, Action=Action_s, ActionTarget=target_s
   ```

### SAP Baseline Monitoring Rules

Create rules to detect deviations from established baselines:

1. **User Activity Baseline Deviation**
   ```kql
   // Establish normal activity patterns per user
   let userBaselines = SAPSecurity_CL
   | where TimeGenerated between(ago(90d)..ago(1d))
   | summarize 
       ActionsByDay = count() / 90,  
       StdDev = stdev(count_),
       AvgLoginTime = avg(datetime_part("hour", TimeGenerated)),
       Systems = make_set(SAPSystem_s), 
       Clients = make_set(Client_s),
       Transactions = make_set(TransactionCode_s)
       by User_s;
   // Detect current deviations
   SAPSecurity_CL
   | where TimeGenerated > ago(1d)
   | summarize 
       CurrentActions = count(), 
       CurrentLoginTime = avg(datetime_part("hour", TimeGenerated)),
       CurrentSystems = make_set(SAPSystem_s),
       CurrentClients = make_set(Client_s),
       CurrentTransactions = make_set(TransactionCode_s)
       by User_s
   | join kind=inner userBaselines on User_s
   | extend 
       ActionDiff = CurrentActions - ActionsByDay,
       NewSystem = set_difference(CurrentSystems, Systems),
       NewClient = set_difference(CurrentClients, Clients),
       NewTransaction = set_difference(CurrentTransactions, Transactions),
       TimeDifference = abs(CurrentLoginTime - AvgLoginTime)
   | where ActionDiff > (3 * StdDev) or array_length(NewSystem) > 0 or array_length(NewClient) > 0 or TimeDifference > 3
   | project User_s, ActionDiff, NewSystem, NewClient, NewTransaction, TimeDifference
   ```

2. **System Config Change Velocity**
   ```kql
   // Get baseline of config changes
   let configBaseline = SAPChangeLog_CL
   | where TimeGenerated between(ago(90d)..ago(1d))
   | where TableName_s startswith "T" and not(TableName_s startswith "TST")
   | summarize
       ChangesPerDay = count() / 90,
       StdDev = stdev(count_)
       by SAPSystem_s;
   // Detect abnormal change rates
   SAPChangeLog_CL
   | where TimeGenerated > ago(1d)
   | where TableName_s startswith "T" and not(TableName_s startswith "TST")
   | summarize CurrentChanges = count() by SAPSystem_s
   | join kind=inner configBaseline on SAPSystem_s
   | extend Threshold = ChangesPerDay + (2 * StdDev)
   | where CurrentChanges > Threshold
   | project SAPSystem_s, CurrentChanges, Threshold, Difference = CurrentChanges - Threshold
   ```

### SAP-GitHub Integration Detection Rules

Create these rules to monitor the integration points between SAP and GitHub:

1. **Transport to GitHub Commit Mismatch**
   ```kql
   // Get all transports created/released in SAP
   let sap_transports = SAPTransportEvents_CL
   | where TimeGenerated > ago(7d)
   | where EventType_s == "TRANSPORT_CREATED" or EventType_s == "TRANSPORT_RELEASED"
   | distinct TransportID_s;
   // Get all GitHub commits with transport references
   let github_transports = GitHubRepoEvents_CL
   | where TimeGenerated > ago(7d)
   | where event_type_s == "push"
   | where commit_message_s matches regex "TR[0-9]{6,}" or commit_message_s contains "TRID="
   | extend TransportID = extract("(TR[0-9]{6,})", 1, commit_message_s)
   | where isnotempty(TransportID)
   | distinct TransportID;
   // Find transports with no matching GitHub commit
   sap_transports 
   | join kind=leftouter (github_transports) on $left.TransportID_s == $right.TransportID
   | where isempty(TransportID)
   | project MissingTransport = TransportID_s
   ```

2. **Development Path Validation**
   ```kql
   // Detect objects created/modified directly in production
   let prod_objects = SAPObjectEvents_CL
   | where TimeGenerated > ago(7d)
   | where SAPSystem_s in ("PRD", "PROD") 
   | where EventType_s in ("OBJECT_CREATED", "OBJECT_MODIFIED")
   | distinct ObjectName_s, ObjectType_s;
   // Find objects that weren't in GitHub
   prod_objects
   | join kind=leftouter (
       GitHubRepoEvents_CL
       | where TimeGenerated > ago(30d)
       | where event_type_s == "push"
       | extend ObjectName = extract("([A-Z0-9_]+\\.[A-Z0-9_]+)", 1, file_path_s)
       | where isnotempty(ObjectName)
       | distinct ObjectName
   ) on $left.ObjectName_s == $right.ObjectName
   | where isempty(ObjectName)
   | project ObjectName_s, ObjectType_s
   ```

### Optimizing Custom Rules

To improve the efficiency and effectiveness of your custom rules:

1. **Performance Optimization**
   - Use appropriate time ranges
   - Filter events early in the query
   - Limit unnecessary joins
   - Utilize summarize and other aggregation functions efficiently

2. **Reducing False Positives**
   - Implement and maintain allowlists
   - Use context-aware thresholds
   - Incorporate baseline patterns
   - Consider business cycles and release schedules

3. **Enrichment Strategies**
   - Add user role information
   - Include organizational details
   - Provide SAP business context
   - Link to GitHub activities

4. **Rule Management**
   - Document detection logic and thresholds
   - Track rule versions
   - Establish rule review cadence
   - Maintain knowledgebase of triggered alerts

## Best Practices

1. **Development Methodology**
   - Start with high-priority risks
   - Test in development environment first
   - Begin with low severity alerts
   - Gradually increase scope and complexity

2. **Alert Tuning Strategy**
   - Gather baseline data for at least 30 days
   - Analyze alert distribution
   - Adjust thresholds systematically
   - Document tuning decisions

3. **Collaboration Framework**
   - Engage SAP Basis teams
   - Involve security team in reviews
   - Seek input from ABAP developers
   - Share findings with compliance teams

4. **Continuous Improvement**
   - Review alert effectiveness regularly
   - Update detection patterns
   - Incorporate threat intelligence
   - Implement feedback from incident responders

## Troubleshooting

### Common Issues and Solutions

- **Missing SAP Events**:
  - Verify SAP Security Audit Log configuration
  - Check data connector status
  - Validate log collection settings
  - Ensure proper table filtering is configured

- **Rule Performance Problems**:
  - Review query complexity
  - Optimize join operations
  - Implement appropriate time filters
  - Consider partitioning large datasets

- **False Positive Management**:
  - Update allowlists regularly
  - Implement dynamic thresholds
  - Add context-specific exclusions
  - Configure incident suppression rules

## ➡️ Next Steps

After implementing custom SAP rules, proceed to:

- [Transport Anomaly Rules](./transport-rules.md) - Configure specialized transport monitoring
- [Authentication Rules](./auth-rules.md) - Set up authentication monitoring rules
- [Incident Response](./incident-response.md) - Configure automated response to SAP alerts
- [Integration Dashboard](./integration-dashboard.md) - Create a unified monitoring dashboard

## References

- [SAP Security Audit Log Configuration Guide](https://help.sap.com/viewer/9a1db9c4a981440fba03169e136d8009/7.5.19/en-US)
- [SAP Security Notes](https://support.sap.com/security)
- [Kusto Query Language Reference](https://docs.microsoft.com/en-us/azure/data-explorer/kusto/query/)
- [OWASP SAP Security Guidelines](https://owasp.org/www-project-sap-security/)
- [Microsoft Sentinel for SAP Documentation](https://docs.microsoft.com/en-us/azure/sentinel/sap-solution)

---




