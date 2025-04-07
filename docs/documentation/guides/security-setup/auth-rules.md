# 📄 Authentication Rules for Microsoft Sentinel

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
This document provides detailed instructions for implementing authentication monitoring rules in Microsoft Sentinel for your SAP-GitHub integration. These rules detect suspicious authentication activities, potential credential compromises, and improper access across your integrated environment.

## ✅ Prerequisites
- Microsoft Sentinel workspace configured (refer to [Sentinel Workspace Setup](./sentinel-workspace.md))
- Data connectors configured (refer to [Data Collection](./data-collection.md))
- SAP connector deployed (refer to [SAP Connector Setup](./sap-connector.md))
- GitHub connector deployed (refer to [GitHub Connector Setup](./github-connector.md))
- Azure connector configured (refer to [Azure Connector Setup](./azure-connector.md))
- Standard rules deployed (refer to [Standard Rules Deployment](./standard-rules.md))

## Detailed Content

### Understanding Authentication Security Risks

Authentication monitoring addresses these key risk areas:

1. **Credential Compromise**
   - Password brute force attacks
   - Password spraying
   - Stolen credentials
   - Pass-the-hash attacks

2. **Privilege Escalation**
   - Elevation to privileged roles
   - Unauthorized privilege changes
   - Temporary privilege abuse
   - Role/group membership changes

3. **Authentication Anomalies**
   - Off-hours access
   - Unusual locations
   - Multiple concurrent sessions
   - Unusual system access patterns

4. **Cross-System Authentication**
   - Inconsistent identities
   - Service account misuse
   - Machine-to-machine authentication issues
   - Improper API authentication

### Core Authentication Rules for SAP

Implement these rules to detect SAP authentication issues:

1. **SAP Failed Login Attempts**
   ```kql
   SAPSecurity_CL
   | where TimeGenerated > ago(1h)
   | where EventType_s == "AUDIT_FAILURE" and EventSubType_s == "LOGIN_FAILED"
   | summarize 
       FailedCount = count(),
       TimesSeen = dcount(TimeGenerated)
       by User_s, Terminal_s, Client_s, SAPSystem_s
   | where FailedCount > 5
   | project TimeGenerated, User_s, Terminal_s, Client_s, SAPSystem_s, FailedCount
   ```

2. **SAP User Lock/Unlock Events**
   ```kql
   SAPSecurity_CL
   | where TimeGenerated > ago(24h)
   | where (EventText_s contains "USER LOCKED" or EventText_s contains "USER UNLOCKED")
   | extend Action = iff(EventText_s contains "LOCKED", "LOCKED", "UNLOCKED")
   | extend TargetUser = extract("USER ([A-Z0-9_]+)", 1, EventText_s)
   | project TimeGenerated, Actor=User_s, TargetUser, Action, SAPSystem_s, Client_s
   ```

3. **SAP Emergency User Access**
   ```kql
   SAPSecurity_CL
   | where TimeGenerated > ago(7d)
   | where User_s in ("SAP*", "DDIC", "FIREFIGHTER*", "EMERGENCY*")
   | where EventType_s == "AUDIT_SUCCESS" and EventSubType_s == "LOGIN_SUCCESSFUL"
   | project TimeGenerated, User_s, Terminal_s, SAPSystem_s, Client_s
   ```

4. **SAP Password Changes**
   ```kql
   SAPSecurity_CL
   | where TimeGenerated > ago(7d)
   | where TransactionCode_s == "SU01" and EventText_s contains "PASSWORD"
   | extend TargetUser = extract("USER ([A-Z0-9_]+)", 1, EventText_s)
   | project TimeGenerated, Actor=User_s, TargetUser, SAPSystem_s, Client_s
   ```

### Core Authentication Rules for GitHub

Implement these rules to detect GitHub authentication issues:

1. **GitHub Failed Authentication**
   ```kql
   GitHubAuditLogEvents_CL
   | where TimeGenerated > ago(24h)
   | where Action_s == "user.failed_login" 
   | summarize 
       FailedCount = count(),
       TimesSeen = dcount(TimeGenerated)
       by User=actor_s, IPAddress=actor_ip_s
   | where FailedCount > 5
   | project TimeGenerated, User, IPAddress, FailedCount
   ```

2. **GitHub OAuth Application Authorization**
   ```kql
   GitHubAuditLogEvents_CL
   | where TimeGenerated > ago(7d)
   | where Action_s in ("oauth_application.create", "oauth_application.token_requested")
   | project TimeGenerated, User=actor_s, IPAddress=actor_ip_s, Action=Action_s, Details=details_s
   ```

3. **GitHub Personal Access Token Creation**
   ```kql
   GitHubAuditLogEvents_CL
   | where TimeGenerated > ago(7d)
   | where Action_s == "personal_access_token.create"
   | project TimeGenerated, User=actor_s, IPAddress=actor_ip_s, TokenName=token_name_s
   ```

4. **GitHub User Permission Changes**
   ```kql
   GitHubAuditLogEvents_CL
   | where TimeGenerated > ago(7d)
   | where Action_s in ("org.add_member", "org.update_member", "team.add_member", "repo.add_member")
   | extend TargetUser = coalesce(user_s, member_s, target_s)
   | project TimeGenerated, Actor=actor_s, TargetUser, Action=Action_s, Permission=permission_s
   ```

### Core Authentication Rules for Azure

Implement these rules to detect Azure authentication issues:

1. **Azure AD Failed Authentication**
   ```kql
   SigninLogs
   | where TimeGenerated > ago(24h)
   | where ResultType != 0
   | summarize 
       FailedCount = count(),
       FailedResults = make_set(ResultType)
       by UserPrincipalName, IPAddress, AppDisplayName
   | where FailedCount > 5
   | project TimeGenerated, UserPrincipalName, IPAddress, AppDisplayName, FailedCount, FailedResults
   ```

2. **Azure AD New Sign-in Locations**
   ```kql
   // Get baseline of locations per user
   let knownLocations = SigninLogs
   | where TimeGenerated between(ago(30d)..ago(1d))
   | where ResultType == 0
   | summarize by UserPrincipalName, Location=tostring(LocationDetails.countryOrRegion);
   // Detect new locations
   SigninLogs
   | where TimeGenerated > ago(1d)
   | where ResultType == 0
   | extend Location = tostring(LocationDetails.countryOrRegion)
   | join kind=leftanti knownLocations on UserPrincipalName, Location
   | project TimeGenerated, UserPrincipalName, IPAddress, Location, AppDisplayName
   ```

3. **Azure AD Privileged Role Assignment**
   ```kql
   AuditLogs
   | where TimeGenerated > ago(7d)
   | where OperationName has "PIM"
   | where OperationName has "role" and OperationName has "assigned"
   | extend RoleName = tostring(TargetResources[0].displayName)
   | extend TargetUser = tostring(TargetResources[0].userPrincipalName)
   | project TimeGenerated, Actor=InitiatedBy.user.userPrincipalName, TargetUser, RoleName
   ```

4. **API Management Authentication Failures**
   ```kql
   ApiManagementGatewayLogs
   | where TimeGenerated > ago(24h)
   | where ResponseCode in (401, 403)
   | summarize 
       FailedCount = count()
       by IPAddress=CallerIpAddress, URL=BackendUrl, Product=ProductId
   | where FailedCount > 10
   | project TimeGenerated, IPAddress, URL, Product, FailedCount
   ```

### Cross-System Authentication Rules

Implement these rules to detect cross-system authentication issues:

1. **SAP and GitHub Account Correlation**
   ```kql
   // Map SAP users to GitHub accounts
   let userMapping = externaldata(SAPUser:string, GitHubUser:string)
   ["https://yourstorageaccount.blob.core.windows.net/sentinel/user_mappings.csv"];
   // Get SAP user logouts
   let sapLogouts = SAPSecurity_CL
   | where TimeGenerated > ago(4h)
   | where EventType_s == "AUDIT_SUCCESS" and EventSubType_s == "LOGOUT"
   | project LogoutTime=TimeGenerated, SAPUser=User_s;
   // Get GitHub logins occurring after SAP logouts
   let githubLogins = GitHubAuditLogEvents_CL
   | where TimeGenerated > ago(4h)
   | where Action_s == "user.login"
   | project LoginTime=TimeGenerated, GitHubUser=actor_s;
   // Find correlated events
   sapLogouts
   | join kind=inner userMapping on SAPUser
   | join kind=inner githubLogins on GitHubUser
   | where LoginTime between (LogoutTime .. (LogoutTime + 2m))
   | project SAPUser, GitHubUser, SAPLogoutTime=LogoutTime, GitHubLoginTime=LoginTime, TimeDelta=datetime_diff('second', LoginTime, LogoutTime)
   ```

2. **Service Account Abuse Detection**
   ```kql
   // Define known service accounts
   let serviceAccounts = dynamic(["SAP_SRV", "GITHUB_SVC", "API_SERVICE", "INTEGRATION_USER"]);
   // Detect suspicious interactive logins
   union
     (SAPSecurity_CL
     | where User_s in (serviceAccounts)
     | where EventType_s == "AUDIT_SUCCESS" and EventSubType_s == "LOGIN_SUCCESSFUL"
     | extend System="SAP", Account=User_s),
     (GitHubAuditLogEvents_CL
     | where actor_s in (serviceAccounts)
     | where Action_s == "user.login"
     | extend System="GitHub", Account=actor_s),
     (SigninLogs
     | where UserPrincipalName has_any (serviceAccounts)
     | where ResultType == 0
     | extend System="Azure AD", Account=UserPrincipalName)
   | project TimeGenerated, System, Account, IPAddress=coalesce(IPAddress, actor_ip_s, Terminal_s)
   ```

3. **Cross-System Logon Time Anomalies**
   ```kql
   // First define our timeframe
   let timeWindow = 1h;
   // Get SAP logins
   let sapLogins = SAPSecurity_CL
   | where TimeGenerated > ago(timeWindow)
   | where EventType_s == "AUDIT_SUCCESS" and EventSubType_s == "LOGIN_SUCCESSFUL"
   | project SAPTime=TimeGenerated, SAPUser=User_s;
   // Get GitHub logins
   let githubLogins = GitHubAuditLogEvents_CL
   | where TimeGenerated > ago(timeWindow)
   | where Action_s == "user.login"
   | project GitHubTime=TimeGenerated, GitHubUser=actor_s;
   // Get Azure AD logins
   let aadLogins = SigninLogs
   | where TimeGenerated > ago(timeWindow)
   | where ResultType == 0
   | project AADTime=TimeGenerated, AADUser=UserPrincipalName;
   // Map users across systems
   let userMappings = externaldata(SAPUser:string, GitHubUser:string, AADUser:string)
   ["https://yourstorageaccount.blob.core.windows.net/sentinel/user_mappings_full.csv"];
   // Detect impossible travel
   sapLogins
   | join kind=inner userMappings on SAPUser
   | join kind=inner githubLogins on GitHubUser
   | join kind=inner aadLogins on AADUser
   | extend 
       SAPtoGitHub = datetime_diff('second', GitHubTime, SAPTime),
       GitHubtoAAD = datetime_diff('second', AADTime, GitHubTime),
       SAPtoAAD = datetime_diff('second', AADTime, SAPTime)
   | where (abs(SAPtoGitHub) < 30 or abs(GitHubtoAAD) < 30 or abs(SAPtoAAD) < 30) // Too close together
   | project SAPUser, GitHubUser, AADUser, SAPTime, GitHubTime, AADTime, SAPtoGitHub, GitHubtoAAD, SAPtoAAD
   ```

### Machine-to-Machine Authentication Rules

Implement these rules for API and service authentication monitoring:

1. **API Management Authentication Pattern Changes**
   ```kql
   ApiManagementGatewayLogs
   | where TimeGenerated > ago(7d)
   | where ApiId contains "SAP" or ApiId contains "GitHub"
   | summarize 
       RequestCount = count(),
       StatusCodes = make_set(ResponseCode),
       IPAddresses = make_set(CallerIpAddress)
       by bin(TimeGenerated, 1h), SubscriptionId, ApiId
   | order by TimeGenerated asc
   | serialize
   | extend 
       PrevCount = prev(RequestCount, 1),
       PrevIPs = prev(IPAddresses, 1)
   | extend 
       CountDiff = iff(isempty(PrevCount), 0, RequestCount - PrevCount),
       NewIPs = set_difference(IPAddresses, PrevIPs)
   | where (abs(CountDiff) > 50 and CountDiff > PrevCount * 0.5) or array_length(NewIPs) > 3
   | project TimeGenerated, SubscriptionId, ApiId, RequestCount, CountDiff, StatusCodes, NewIPs
   ```

2. **OAuth Token Usage Anomalies**
   ```kql
   ApiManagementGatewayLogs
   | where TimeGenerated > ago(7d)
   | where IsRequestSuccess == true
   | extend Headers = parse_json(RequestHeaders)
   | where Headers.Authorization startswith "Bearer"
   | extend Token = trim_start("Bearer ", Headers.Authorization)
   | summarize 
       RequestCount = count(),
       APIs = make_set(ApiId),
       IPAddresses = make_set(CallerIpAddress)
       by Token
   | where RequestCount > 1000 or array_length(APIs) > 10 or array_length(IPAddresses) > 5
   | project Token, RequestCount, APIs, IPAddresses
   ```

3. **System Account Key Changes**
   ```kql
   AzureActivity
   | where TimeGenerated > ago(30d)
   | where OperationName has_any ("REGENERATE", "UPDATE", "REKEY", "ROTATE") 
       and OperationName has_any ("KEY", "SECRET", "PASSWORD", "CREDENTIAL")
   | where ResourceGroup has_any ("SAP", "GitHub", "Integration", "API")
   | project TimeGenerated, Caller, CallerIpAddress, OperationName, Resource = ResourceProviderValue
   ```

### Authentication Policy Monitoring

Implement these rules to detect authentication policy changes:

1. **Authentication Policy Modifications**
   ```kql
   union
     (AuditLogs
      | where TimeGenerated > ago(30d)
      | where OperationName has_any ("policy", "Policy") and OperationName has_any ("Conditional Access", "Authentication")),
     (AzureActivity
      | where TimeGenerated > ago(30d)
      | where OperationName has_any ("POLICY", "AUTHENTICATION", "AUTH") 
          and ResourceProvider has_any ("MICROSOFT.APIMANAGEMENT", "MICROSOFT.KEYVAULT"))
   | project TimeGenerated, Actor = coalesce(Caller, tostring(InitiatedBy.user.userPrincipalName)), Operation = OperationName, Resource = coalesce(ResourceProviderValue, TargetResources[0].displayName)
   ```

2. **MFA Configuration Changes**
   ```kql
   AuditLogs
   | where TimeGenerated > ago(30d)
   | where OperationName has "MFA" or OperationName has "MultiFactor"
   | extend User = tostring(InitiatedBy.user.userPrincipalName)
   | extend Target = tostring(TargetResources[0].displayName)
   | project TimeGenerated, User, OperationName, Target
   ```

3. **SAP Authorization Policy Changes**
   ```kql
   SAPSecurity_CL
   | where TimeGenerated > ago(7d)
   | where TransactionCode_s in ("PFCG", "SU01", "SU10", "SU24")
   | where EventText_s has_any ("AUTHORIZATION", "PROFILE", "ROLE")
   | project TimeGenerated, User_s, TransactionCode_s, SAPSystem_s, Client_s, EventText_s
   ```

### Authentication Alert Enrichment

Enhance authentication alerts with context:

1. **User Role Information**
   - Add user role data to alerts
   - Include group memberships
   - Identify privileged accounts
   - Add business context

2. **Authentication Pattern Analysis**
   - Provide historical authentication patterns
   - Include typical work hours
   - List common access locations
   - Identify deviations

3. **Cross-System Context**
   - Associate identities across systems
   - Add authorization context
   - Include recent activities
   - Provide access history

## Best Practices

1. **Authentication Rule Strategy**
   - Focus on high-value accounts
   - Implement graduated response thresholds
   - Tune alert timing for different scenarios
   - Consider user roles in detection logic

2. **Geolocation Context**
   - Maintain location allowlists
   - Create country/region profiles
   - Use IP reputation data
   - Account for business travel

3. **Service Account Management**
   - Create dedicated service account detection rules
   - Apply stricter thresholds
   - Monitor for interactive usage
   - Track credential rotation compliance

4. **Alert Tuning Guidelines**
   - Begin with informational alerts
   - Analyze alert pattern for 30+ days
   - Adjust thresholds based on observed data
   - Create exception lists for specific scenarios

## Troubleshooting

### Common Issues and Solutions

- **High False Positive Rate**:
  - Refine geolocation detection logic
  - Implement better user behavior profiling
  - Tune thresholds for specific user groups
  - Create targeted exception lists

- **Missing Authentication Events**:
  - Verify data collection settings
  - Check source system audit configuration
  - Validate network connectivity
  - Review API permissions

- **Alert Correlation Issues**:
  - Verify account mapping data
  - Standardize timestamp formats
  - Check time synchronization
  - Improve entity mapping

## ➡️ Next Steps

After configuring authentication rules, proceed to:

- [Incident Response](./incident-response.md) - Configure automated response to authentication incidents
- [Integration Dashboard](./integration-dashboard.md) - Monitor authentication across systems
- [Notification Configuration](./notifications.md) - Configure security notifications for authentication alerts
- [Security Governance](./security-governance.md) - Establish authentication governance framework

## References

- [SAP Security Audit Log Configuration](https://help.sap.com/docs/SAP_NETWEAVER/9a1db9c4a981440fba03169e136d8009/c769bebf70804069b80acc0b76618c5d.html)
- [GitHub Enterprise Security Best Practices](https://docs.github.com/en/enterprise-server@latest/admin/identity-and-access-management)
- [Azure AD Sign-in Log Schema](https://docs.microsoft.com/en-us/azure/active-directory/reports-monitoring/reference-azure-monitor-sign-ins-log-schema)
- [Microsoft Sentinel User Entity Behavior Analytics](https://docs.microsoft.com/en-us/azure/sentinel/identify-threats-with-entity-behavior-analytics)
- [NIST Digital Identity Guidelines](https://pages.nist.gov/800-63-3/)

---




