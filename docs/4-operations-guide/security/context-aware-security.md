# 📄 Context-Aware Security

## 📋 Table of Contents

- [📋 Overview](#overview)
- [Principles of Context-Aware Security](#principles-of-context-aware-security)
- [Context Types for Security](#context-types-for-security)
- [Architecture Components](#architecture-components)
- [🔧 Implementation Guide](#implementation-guide)
- [🛡
️ ️ Security Use Cases](#security-use-cases)
- [Best Practices](#best-practices)
- [Related Documentation](#related-documentation)
- [External References](#external-references)


## 📋 Overview

Context-aware security represents a significant advancement in protecting SAP-GitHub integrated environments. By leveraging the rich contextual information available through the Model Context Protocol (MCP), organizations can implement more intelligent, adaptive, and effective security controls. This document outlines the principles, implementation approaches, and best practices for context-aware security in the SAP-GitHub integration.

## Principles of Context-Aware Security

### 1. Contextual Identity and Access Management

Traditional security relies on static identity attributes, while context-aware security dynamically evaluates access based on a rich set of contextual factors:

- User identity and role
- Transport and development context
- System state and environment
- Temporal factors (time, day, development phase)
- Behavioral patterns and anomalies

### 2. Continuous Risk Assessment

Context-aware security continuously evaluates risk based on context, rather than making one-time authorization decisions:

- Real-time analysis of development activities
- Correlation between SAP and GitHub activities
- Behavioral baseline comparison
- Risk scoring based on context deviation

### 3. Adaptive Security Responses

Security responses scale appropriately based on the context and risk level:

- Low risk: Allow with monitoring
- Medium risk: Require additional verification
- High risk: Block and alert

## Context Types for Security

### Development Context

| Context Element | Security Relevance |
|-----------------|-------------------|
| Transport ID | Links activities across systems for correlation |
| Developer Identity | Enables behavioral analysis and unusual activity detection |
| Object Types | Different objects have different risk profiles |
| Development Phase | Different controls apply in DEV vs. QAS vs. PRD |
| Change Type | Emergency changes have different risk patterns than regular changes |

### System Context

| Context Element | Security Relevance |
|-----------------|-------------------|
| System Environment | DEV/QAS/PRD environments have different security requirements |
| Connection Details | Source IP, network segment, VPN status |
| Client | SAP client context for multi-client installations |
| System Load/State | Unusual system states might indicate security incidents |
| Integration Points | Active interfaces and connections |

### Behavioral Context

| Context Element | Security Relevance |
|-----------------|-------------------|
| Time Patterns | Usual working hours vs. unusual activity times |
| Activity Volume | Normal vs. unusual activity volume |
| Activity Sequences | Expected vs. unexpected sequences of operations |
| Cross-System Patterns | Correlation between SAP and GitHub activities |
| Content Patterns | Code pattern anomalies |

## Architecture Components

### 1. Context Collection and Enrichment

```
┌──────────────────┐     ┌───────────────────┐     ┌───────────────────┐
│  SAP Activities  │────►│  Context          │     │  Security         │
│  & Transports    │     │  Collection       │────►│  Context          │
└──────────────────┘     │  & Enrichment     │     │  Store            │
                         └───────────────────┘     └─────────┬─────────┘
┌──────────────────┐                                         │
│  GitHub          │                                         │
│  Activities      │─────────────────────────────────────────┘
└──────────────────┘
```

The Context Collection component gathers security-relevant context from all activities in both SAP and GitHub environments, enriches it with additional metadata, and stores it in the Security Context Store.

### 2. Context-Aware Security Engines

```
┌───────────────────┐    ┌───────────────────┐    ┌───────────────────┐
│  Real-time        │    │  Context-based     │    │  Adaptive         │
│  Activity         │───►│  Risk Assessment   │───►│  Security         │
│  Monitoring       │    │  Engine            │    │  Enforcement      │
└───────────────────┘    └───────────────────┘    └───────────────────┘
          ▲                        ▲                        │
          │                        │                        │
          │                        │                        ▼
┌─────────┴───────────────────────┴────────────────────────┴───────────┐
│                     Security Context Store                           │
└─────────────────────────────────────────────────────────────────────┘
```

The Security Engines use context information to perform risk assessment and apply appropriate security controls.

### 3. Integration with Microsoft Security Services

```
┌───────────────────┐    ┌───────────────────┐    ┌───────────────────┐
│  SAP-GitHub       │    │  Microsoft        │    │  Response &        │
│  Security         │───►│  Sentinel         │───►│  Remediation       │
│  Context          │    │                   │    │  Automation        │
└───────────────────┘    └───────────────────┘    └───────────────────┘
                                 │
                                 │
                         ┌───────▼───────┐
                         │  Microsoft    │
                         │  Defender     │
                         └───────────────┘
```

Integrates with Microsoft security services to provide enterprise-wide security visibility and automated response capabilities.

## 🔧 Implementation Guide

### 1. Security Context Collection

**SAP-Side Collection:**

```abap
METHOD collect_security_context.
  DATA:
    ls_context TYPE ty_security_context,
    lv_context_id TYPE string.
  
  " Collect user context
  ls_context-user_id = sy-uname.
  ls_context-client = sy-mandt.
  ls_context-terminal = sy-host.
  ls_context-timestamp = sy-datum && sy-uzeit.
  
  " Collect transport context if applicable
  IF iv_transport_id IS NOT INITIAL.
    SELECT SINGLE * FROM e070 
      INTO @DATA(ls_transport)
      WHERE trkorr = @iv_transport_id.
    
    IF sy-subrc = 0.
      ls_context-transport_id = ls_transport-trkorr.
      ls_context-transport_type = ls_transport-trfunction.
      ls_context-transport_target = ls_transport-tarsystem.
    ENDIF.
  ENDIF.
  
  " Collect activity context
  ls_context-activity_type = iv_activity_type.
  ls_context-object_type = iv_object_type.
  ls_context-object_name = iv_object_name.
  
  " Send to Context Store
  lv_json = /ui2/cl_json=>serialize( ls_context ).
  
  cl_http_client=>create_by_url(
    EXPORTING
      url    = 'https://sap-github-mcp-apim.azure-api.net/context/api/v1/security-contexts'
    IMPORTING
      client = lo_http_client ).
  
  lo_http_client->request->set_method( 'POST' ).
  lo_http_client->request->set_content_type( 'application/json' ).
  lo_http_client->request->set_cdata( lv_json ).
  
  " Add authentication
  lo_http_client->request->set_header_field(
    name  = 'Authorization'
    value = 'Bearer ' && gv_api_token ).
  
  lo_http_client->send( ).
  lo_http_client->receive( ).
  
  lv_response = lo_http_client->response->get_cdata( ).
  
  " Extract context ID from response
  FIND REGEX '"id"\s*:\s*"([^"]+)"' IN lv_response SUBMATCHES lv_context_id.
  
  " Return context ID for reference
  rv_context_id = lv_context_id.
ENDMETHOD.
```

**GitHub-Side Collection:**

```yaml
name: Security Context Collection

on:
  push:
    branches: [ main, dev*, feature/* ]
  pull_request:
    types: [opened, synchronize, closed]
  workflow_dispatch:

jobs:
  collect-security-context:
    runs-on: ubuntu-latest
    steps:
      - name: Collect Security Context
        uses: actions/github-script@v6
        with:
          script: |
            // Collect GitHub context
            const securityContext = {
              actor: context.actor,
              repo: context.repo.repo,
              owner: context.repo.owner,
              eventName: context.eventName,
              workflow: context.workflow,
              ref: context.ref,
              sha: context.sha,
              timestamp: new Date().toISOString(),
              activityType: 'GITHUB_' + context.eventName.toUpperCase(),
              ipAddress: process.env.GITHUB_ACTOR_IP || 'unknown'
            };
            
            // For pull requests, include PR information
            if (context.eventName === 'pull_request') {
              securityContext.pullRequest = {
                number: context.payload.pull_request.number,
                title: context.payload.pull_request.title,
                state: context.payload.pull_request.state,
                additions: context.payload.pull_request.additions,
                deletions: context.payload.pull_request.deletions
              };
            }
            
            // Send to Context Store
            try {
              console.log('Sending security context to Context Store...');
              const response = await fetch('https://sap-github-mcp-apim.azure-api.net/context/api/v1/security-contexts', {
                method: 'POST',
                headers: {
                  'Content-Type': 'application/json',
                  'Authorization': `Bearer ${process.env.CONTEXT_API_TOKEN}`
                },
                body: JSON.stringify(securityContext)
              });
              
              if (!response.ok) {
                throw new Error(`Error sending security context: ${response.statusText}`);
              }
              
              const data = await response.json();
              console.log(`Created security context with ID: ${data.id}`);
              
            } catch (error) {
              console.error('Error sending security context:', error);
              core.setFailed(error.message);
            }
        env:
          CONTEXT_API_TOKEN: ${{ secrets.CONTEXT_API_TOKEN }}
```

### 2. Context-Aware Risk Assessment Rules

Create context-aware security rules in Microsoft Sentinel:

```kusto
// Context-based anomaly detection for SAP-GitHub activities
let UserBaselineActivity = 
    SAPGitHubSecurityContext_CL 
    | where TimeGenerated between(ago(30d)..ago(1d))
    | where isnotempty(UserId_s)
    | summarize 
        NormalHours = make_set(datetime_part("hour", TimeGenerated)),
        NormalDays = make_set(datetime_part("dayofweek", TimeGenerated)),
        NormalActivityTypes = make_set(ActivityType_s),
        NormalWorkflows = make_set(Workflow_s),
        AvgTransportsPerDay = count_if(isnotempty(TransportId_s)) * 1.0 / (30),
        AvgCommitsPerDay = count_if(ActivityType_s == "GITHUB_PUSH") * 1.0 / (30)
    by UserId_s;

// Detect unusual timing
SAPGitHubSecurityContext_CL
| where TimeGenerated > ago(24h)
| join kind=leftouter UserBaselineActivity on UserId_s
| extend 
    CurrentHour = datetime_part("hour", TimeGenerated),
    CurrentDay = datetime_part("dayofweek", TimeGenerated),
    IsAbnormalHour = array_index_of(NormalHours, CurrentHour) == -1,
    IsAbnormalDay = array_index_of(NormalDays, CurrentDay) == -1,
    IsAbnormalActivity = array_index_of(NormalActivityTypes, ActivityType_s) == -1
| where IsAbnormalHour or IsAbnormalDay or IsAbnormalActivity
| extend RiskScore = 
    (IsAbnormalHour ? 30 : 0) + 
    (IsAbnormalDay ? 30 : 0) + 
    (IsAbnormalActivity ? 40 : 0)
| where RiskScore > 30
| project 
    TimeGenerated,
    UserId_s,
    ActivityType_s,
    TransportId_s,
    Repository_s,
    ObjectType_s,
    ObjectName_s,
    RiskScore,
    IsAbnormalHour,
    IsAbnormalDay,
    IsAbnormalActivity
```

### 3. Context-Aware Security Policies in API Management

Configure Azure API Management policies using context for adaptive security:

```xml
<policies>
  <inbound>
    <base />
    
    <!-- Extract context information -->
    <set-variable name="contextId" value="@(context.Request.Headers.GetValueOrDefault("X-Context-ID", ""))" />
    <set-variable name="userId" value="@(context.Request.Headers.GetValueOrDefault("X-User-ID", ""))" />
    <set-variable name="transportId" value="@(context.Request.Headers.GetValueOrDefault("X-Transport-ID", ""))" />
    
    <!-- Retrieve complete context information -->
    <send-request mode="new" response-variable-name="contextInfo" timeout="10" ignore-error="false">
      <set-url>@("https://sap-github-mcp-apim.azure-api.net/context/api/v1/contexts/" + context.Variables["contextId"])</set-url>
      <set-method>GET</set-method>
      <authentication-managed-identity resource="https://management.azure.com" />
    </send-request>
    
    <!-- Extract risk score from context -->
    <set-variable name="riskScore" value="@{
        try {
            var contextResponse = ((IResponse)context.Variables["contextInfo"]).Body.As<JObject>();
            return contextResponse["securityMetadata"]["riskScore"].ToString();
        }
        catch (Exception ex) {
            return "50"; // Default medium risk if context not available
        }
    }" />
    
    <!-- Apply adaptive security based on risk score -->
    <choose>
      <!-- Low risk - normal operations -->
      <when condition="@(int.Parse((string)context.Variables["riskScore"]) < 30)">
        <!-- Standard rate limiting -->
        <rate-limit calls="20" renewal-period="60" />
      </when>
      
      <!-- Medium risk - additional verification -->
      <when condition="@(int.Parse((string)context.Variables["riskScore"]) >= 30 && int.Parse((string)context.Variables["riskScore"]) < 70)">
        <!-- More restrictive rate limiting -->
        <rate-limit calls="10" renewal-period="60" />
        
        <!-- Log the medium-risk access -->
        <log-to-eventhub logger-id="security-eventhub">
          @{
              return new JObject(
                  new JProperty("severity", "medium"),
                  new JProperty("contextId", context.Variables["contextId"]),
                  new JProperty("userId", context.Variables["userId"]),
                  new JProperty("riskScore", context.Variables["riskScore"]),
                  new JProperty("action", "additional-monitoring")
              ).ToString();
          }
        </log-to-eventhub>
      </when>
      
      <!-- High risk - block and alert -->
      <when condition="@(int.Parse((string)context.Variables["riskScore"]) >= 70)">
        <!-- Block the request -->
        <return-response>
          <set-status code="403" reason="Forbidden" />
          <set-body>Access denied due to security risk assessment.</set-body>
        </return-response>
        
        <!-- Log the high-risk access attempt -->
        <log-to-eventhub logger-id="security-eventhub">
          @{
              return new JObject(
                  new JProperty("severity", "high"),
                  new JProperty("contextId", context.Variables["contextId"]),
                  new JProperty("userId", context.Variables["userId"]),
                  new JProperty("riskScore", context.Variables["riskScore"]),
                  new JProperty("action", "blocked")
              ).ToString();
          }
        </log-to-eventhub>
      </when>
    </choose>
  </inbound>
</policies>
```

## 🛡️ Security Use Cases

### 1. Cross-System Transport/PR Correlation

**Scenario:** Detect unauthorized or suspicious activities spanning SAP transports and GitHub PRs.

**Implementation:**
```kusto
// Correlate SAP transport and GitHub PR activity
let SAPTransports = 
    SAPGitHubSecurityContext_CL
    | where TimeGenerated > ago(24h)
    | where isnotempty(TransportId_s)
    | project 
        TimeGenerated,
        UserId_s,
        TransportId_s,
        TransportType_s,
        ObjectsChanged_d;

let GitHubPRs = 
    SAPGitHubSecurityContext_CL
    | where TimeGenerated > ago(24h)
    | where ActivityType_s == "GITHUB_PULL_REQUEST"
    | project 
        TimeGenerated,
        UserId_s,
        PRNumber_d,
        Repository_s,
        FilesChanged_d;

// Look for discrepancies in transport/PR activities
SAPTransports
| join kind=fullouter GitHubPRs on UserId_s
| where 
    // Different users modifying same components
    (TransportId_s != "" and PRNumber_d != "" and UserId_s != UserId_s1) or
    // Unusual timing between transport and PR
    (abs(datetime_diff('minute', TimeGenerated, TimeGenerated1)) < 5 and UserId_s != UserId_s1) or
    // Significant mismatch in volume of changes
    (TransportId_s != "" and PRNumber_d != "" and UserId_s == UserId_s1 and abs(ObjectsChanged_d - FilesChanged_d) > 10)
| project
    SAPTime=TimeGenerated,
    GitHubTime=TimeGenerated1,
    SAPUser=UserId_s,
    GitHubUser=UserId_s1,
    TransportId_s,
    PRNumber_d,
    Repository_s,
    TimeDifference=datetime_diff('minute', TimeGenerated, TimeGenerated1)
```

### 2. Unusual Development Patterns

**Scenario:** Detect unusual development patterns that may indicate security risks.

**Implementation:**
```kusto
// Detect unusual development patterns
SAPGitHubSecurityContext_CL
| where TimeGenerated > ago(24h)
| summarize 
    ActivityCount=count(),
    ActivityTypes=make_set(ActivityType_s),
    TransportCount=countif(isnotempty(TransportId_s)),
    PRCount=countif(ActivityType_s == "GITHUB_PULL_REQUEST"),
    CommitCount=countif(ActivityType_s == "GITHUB_PUSH"),
    UniqueObjects=dcount(ObjectName_s),
    DistinctSystems=dcount(SystemId_s)
  by UserId_s, bin(TimeGenerated, 1h)
| extend 
    ActivityTypesCount = array_length(ActivityTypes),
    ActivityIntensity = ActivityCount / 60.0  // Activities per minute
| join kind=leftouter (
    // Baseline user activity
    SAPGitHubSecurityContext_CL
    | where TimeGenerated between(ago(30d)..ago(1d))
    | summarize 
        BaselineActivityCount=avg(count_()),
        BaselineTransportCount=avg(countif(isnotempty(TransportId_s))),
        BaselinePRCount=avg(countif(ActivityType_s == "GITHUB_PULL_REQUEST")),
        BaselineCommitCount=avg(countif(ActivityType_s == "GITHUB_PUSH")),
        BaselineUniqueObjects=avg(dcount(ObjectName_s)),
        BaselineDistinctSystems=avg(dcount(SystemId_s))
      by UserId_s, bin(TimeGenerated, 1h)
) on UserId_s
| extend 
    ActivityCountRatio = ActivityCount / max(1, BaselineActivityCount),
    TransportCountRatio = TransportCount / max(1, BaselineTransportCount),
    PRCountRatio = PRCount / max(1, BaselinePRCount),
    CommitCountRatio = CommitCount / max(1, BaselineCommitCount)
| where 
    ActivityCountRatio > 3 or
    TransportCountRatio > 3 or
    PRCountRatio > 3 or
    CommitCountRatio > 3 or
    ActivityTypesCount > 5 or
    DistinctSystems > BaselineDistinctSystems + 2
| project
    TimeGenerated,
    UserId_s,
    ActivityCount,
    BaselineActivityCount,
    ActivityCountRatio,
    TransportCount,
    BaselineTransportCount,
    TransportCountRatio,
    PRCount,
    BaselinePRCount,
    PRCountRatio,
    ActivityTypesCount,
    DistinctSystems
```

### 3. Privilege Escalation Detection

**Scenario:** Detect attempts to escalate privileges in SAP or GitHub environments.

**Implementation:**
```kusto
// Detect privilege changes and escalation
let PrivilegeChangeEvents = 
    SAPGitHubSecurityContext_CL
    | where TimeGenerated > ago(24h)
    | where 
        // SAP privilege changes
        (ActivityType_s == "SAP_USER_CHANGE" and ObjectType_s in ("USR02", "AGR_USERS")) or
        // GitHub permission changes
        (ActivityType_s startswith "GITHUB_" and ActivityType_s endswith "_PERMISSION")
    | project
        TimeGenerated,
        UserId_s,
        TargetUser_s,
        ActivityType_s,
        ObjectType_s,
        ObjectName_s,
        PermissionLevel_s,
        SystemId_s;

// Detect suspicious privilege changes
PrivilegeChangeEvents
| join kind=leftouter (
    // Get user's baseline authorization to make permission changes
    SAPGitHubSecurityContext_CL
    | where TimeGenerated between(ago(90d)..ago(1d))
    | where 
        (ActivityType_s == "SAP_USER_CHANGE" and ObjectType_s in ("USR02", "AGR_USERS")) or
        (ActivityType_s startswith "GITHUB_" and ActivityType_s endswith "_PERMISSION")
    | summarize 
        HasAuthHistory = count() > 0,
        AuthChangeCount = count(),
        LastAuthChange = max(TimeGenerated)
      by UserId_s
) on UserId_s
| extend 
    DaysSinceLastAuthChange = datetime_diff('day', TimeGenerated, LastAuthChange),
    IsNewAuthAdmin = HasAuthHistory == false or isnull(HasAuthHistory),
    IsSelfModification = UserId_s == TargetUser_s
| where 
    IsNewAuthAdmin or                           // New admin activity
    DaysSinceLastAuthChange > 60 or             // Dormant admin activity
    (IsSelfModification and                     // Self-permission elevation
     PermissionLevel_s in ("Administrator", "S_ADMI", "SAP_ALL"))
| project
    TimeGenerated,
    UserId_s,
    TargetUser_s,
    ActivityType_s,
    PermissionLevel_s,
    IsNewAuthAdmin,
    DaysSinceLastAuthChange,
    IsSelfModification
```

## Best Practices

### 1. Context Classification

Classify context information based on sensitivity:

- **Public Context**: Non-sensitive development metadata
- **Internal Context**: Organization-specific but not highly sensitive
- **Confidential Context**: Sensitive contextual information (credentials, security settings)

### 2. Context Retention

Implement appropriate retention policies:

- **Short-term Context**: Active development (30-90 days)
- **Medium-term Context**: Project lifecycle (1-2 years)
- **Long-term Context**: Compliance and audit (3-7 years)

### 3. Continuous Monitoring

Implement continuous security monitoring of context data:

- Establish baseline context patterns
- Monitor for context anomalies
- Regularly review security alerts

### 4. Context Security Hardening

Protect the context information itself:

- Encrypt sensitive context in transit and at rest
- Implement strict access controls to context API
- Audit all context access and modifications

## Related Documentation

- [MCP Integration Architecture](../architecture/mcp-integration.md)
- [Context Store Design](../architecture/context-store.md)
- [AI Governance Framework](../security/ai-governance-framework.md)

## External References

1. [Microsoft Sentinel Documentation](https://learn.microsoft.com/en-us/azure/sentinel/)
2. [Microsoft Defender for Cloud Documentation](https://learn.microsoft.com/en-us/azure/defender-for-cloud/)
3. [SAP Security Guidelines](https://support.sap.com/en/security.html)
4. [GitHub Security Best Practices](https://docs.github.com/en/code-security) 
---

<details>
<summary><strong>📊 Document Metadata</strong></summary>

- **Last Updated:** 2025-04-07
- **Version:** 1.0.0
- **Status:** Published
</details>
