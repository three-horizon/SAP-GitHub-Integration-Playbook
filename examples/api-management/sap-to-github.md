---
layout: default
title: SAP to GitHub Integration with API Management
---

# 🔌 SAP to GitHub API Management Configuration Guide

<div align="center">
  <img src="../../assets/images/architecture/api-implementation.svg" width="700" alt="API Implementation Architecture">
  
  *API Management configuration for secure SAP-GitHub integration*
</div>

# SAP to GitHub Integration with API Management

This example demonstrates how to implement API management policies for secure and efficient integration between SAP systems and GitHub.

## Overview

API Management serves as a crucial middleware layer for SAP-GitHub integration, providing security, transformation, rate limiting, and monitoring capabilities. This example shows how to configure API Management policies to securely connect SAP systems with GitHub APIs.

## Prerequisites

- SAP Integration Suite or SAP API Management
- GitHub account and repositories
- GitHub Personal Access Token or OAuth Application
- SAP system with appropriate authorizations
- Network connectivity between systems

## Implementation Architecture

```
┌───────────────┐     ┌──────────────────────┐     ┌────────────┐
│               │     │                      │     │            │
│  SAP Systems  │────▶│  API Management      │────▶│   GitHub   │
│               │     │                      │     │            │
└───────────────┘     └──────────────────────┘     └────────────┘
                             │      ▲
                             │      │
                             ▼      │
                      ┌──────────────────┐
                      │                  │
                      │  API Monitoring  │
                      │  & Analytics     │
                      │                  │
                      └──────────────────┘
```

## API Proxy Configuration

### Base Configuration

```xml
<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<APIProxy revision="1" name="GitHub-Integration-Proxy">
    <Basepaths>/github/v1</Basepaths>
    <Description>API Proxy for SAP-GitHub Integration</Description>
    <PolicyInputs/>
    <ProxyEndpoints>
        <ProxyEndpoint>default</ProxyEndpoint>
    </ProxyEndpoints>
    <TargetEndpoints>
        <TargetEndpoint>default</TargetEndpoint>
    </TargetEndpoints>
</APIProxy>
```

### Proxy Endpoint Definition

```xml
<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<ProxyEndpoint name="default">
    <Description>SAP to GitHub API Proxy Endpoint</Description>
    <FaultRules>
        <FaultRule name="invalid_token">
            <Condition>fault.name = "invalid_token"</Condition>
            <Step>
                <Name>AM-InvalidTokenResponse</Name>
            </Step>
        </FaultRule>
        <FaultRule name="quota_violation">
            <Condition>fault.name = "quota_violation"</Condition>
            <Step>
                <Name>AM-QuotaViolationResponse</Name>
            </Step>
        </FaultRule>
    </FaultRules>
    <PreFlow name="PreFlow">
        <Request>
            <Step>
                <Name>AM-VerifyAPIKey</Name>
            </Step>
            <Step>
                <Name>AM-SpikeArrest</Name>
            </Step>
            <Step>
                <Name>AM-Quota</Name>
            </Step>
            <Step>
                <Name>AM-RequestLogging</Name>
            </Step>
        </Request>
        <Response>
            <Step>
                <Name>AM-ResponseLogging</Name>
            </Step>
            <Step>
                <Name>AM-SecurityHeaders</Name>
            </Step>
        </Response>
    </PreFlow>
    <Flows>
        <Flow name="GetRepositories">
            <Condition>proxy.pathsuffix == "/repositories" AND request.verb == "GET"</Condition>
            <Description>Get repositories from GitHub</Description>
            <Request>
                <Step>
                    <Name>AM-ExtractOrgParam</Name>
                </Step>
                <Step>
                    <Name>AM-SetGitHubHeaders</Name>
                </Step>
            </Request>
            <Response>
                <Step>
                    <Name>AM-FilterRepositoryResponse</Name>
                </Step>
            </Response>
        </Flow>
        <Flow name="CreatePullRequest">
            <Condition>proxy.pathsuffix ~~ "/*/pulls" AND request.verb == "POST"</Condition>
            <Description>Create pull request in GitHub</Description>
            <Request>
                <Step>
                    <Name>AM-ValidatePRPayload</Name>
                </Step>
                <Step>
                    <Name>AM-SetGitHubHeaders</Name>
                </Step>
            </Request>
            <Response>
                <Step>
                    <Name>AM-TransformPRResponse</Name>
                </Step>
            </Response>
        </Flow>
    </Flows>
    <PostFlow name="PostFlow">
        <Request/>
        <Response>
            <Step>
                <Name>AM-RemoveSensitiveInfo</Name>
            </Step>
        </Response>
    </PostFlow>
    <HTTPProxyConnection>
        <BasePath>/github/v1</BasePath>
    </HTTPProxyConnection>
    <RouteRule name="default">
        <TargetEndpoint>default</TargetEndpoint>
    </RouteRule>
</ProxyEndpoint>
```

### Target Endpoint Definition

```xml
<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<TargetEndpoint name="default">
    <Description>GitHub API Target</Description>
    <FaultRules/>
    <PreFlow name="PreFlow">
        <Request/>
        <Response/>
    </PreFlow>
    <Flows/>
    <PostFlow name="PostFlow">
        <Request/>
        <Response/>
    </PostFlow>
    <HTTPTargetConnection>
        <URL>https://api.github.com</URL>
        <Properties>
            <Property name="response.streaming.enabled">true</Property>
            <Property name="request.streaming.enabled">true</Property>
        </Properties>
    </HTTPTargetConnection>
</TargetEndpoint>
```

## Policy Definitions

### Authentication and Security Policies

#### 1. VerifyAPIKey (AM-VerifyAPIKey.xml)

```xml
<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<VerifyAPIKey async="false" continueOnError="false" enabled="true" name="AM-VerifyAPIKey">
    <DisplayName>Verify API Key</DisplayName>
    <Properties/>
    <APIKey ref="request.header.x-api-key"/>
</VerifyAPIKey>
```

#### 2. Set GitHub Authorization Headers (AM-SetGitHubHeaders.xml)

```xml
<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<AssignMessage async="false" continueOnError="false" enabled="true" name="AM-SetGitHubHeaders">
    <DisplayName>Set GitHub Headers</DisplayName>
    <Properties/>
    <Set>
        <Headers>
            <Header name="Authorization">Bearer {private.github.token}</Header>
            <Header name="Accept">application/vnd.github.v3+json</Header>
            <Header name="User-Agent">SAP-GitHub-Integration</Header>
        </Headers>
    </Set>
    <IgnoreUnresolvedVariables>false</IgnoreUnresolvedVariables>
</AssignMessage>
```

#### 3. Security Headers (AM-SecurityHeaders.xml)

```xml
<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<AssignMessage async="false" continueOnError="false" enabled="true" name="AM-SecurityHeaders">
    <DisplayName>Add Security Headers</DisplayName>
    <Properties/>
    <Set>
        <Headers>
            <Header name="Strict-Transport-Security">max-age=31536000; includeSubDomains</Header>
            <Header name="X-Content-Type-Options">nosniff</Header>
            <Header name="X-XSS-Protection">1; mode=block</Header>
            <Header name="X-Frame-Options">DENY</Header>
            <Header name="Content-Security-Policy">default-src 'self'</Header>
        </Headers>
    </Set>
    <IgnoreUnresolvedVariables>false</IgnoreUnresolvedVariables>
</AssignMessage>
```

### Rate Limiting and Quota Policies

#### 1. Spike Arrest (AM-SpikeArrest.xml)

```xml
<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<SpikeArrest async="false" continueOnError="false" enabled="true" name="AM-SpikeArrest">
    <DisplayName>Spike Arrest</DisplayName>
    <Properties/>
    <Rate>30ps</Rate>
    <Identifier ref="client_id"/>
</SpikeArrest>
```

#### 2. Quota (AM-Quota.xml)

```xml
<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<Quota async="false" continueOnError="false" enabled="true" name="AM-Quota">
    <DisplayName>Enforce Quota</DisplayName>
    <Properties/>
    <Identifier ref="client_id" type="ConnectionType"/>
    <Allow count="5000"/>
    <Interval ref="system.period">1</Interval>
    <TimeUnit ref="system.time.unit">hour</TimeUnit>
    <Distributed>true</Distributed>
    <Synchronous>true</Synchronous>
    <AsynchronousConfiguration>
        <SyncIntervalInSeconds>20</SyncIntervalInSeconds>
        <SyncMessageCount>5</SyncMessageCount>
    </AsynchronousConfiguration>
</Quota>
```

#### 3. Quota Violation Response (AM-QuotaViolationResponse.xml)

```xml
<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<AssignMessage async="false" continueOnError="false" enabled="true" name="AM-QuotaViolationResponse">
    <DisplayName>Quota Violation Response</DisplayName>
    <Properties/>
    <Set>
        <Headers>
            <Header name="Content-Type">application/json</Header>
        </Headers>
        <Payload contentType="application/json">
        {
            "error": {
                "code": "429",
                "message": "API quota exceeded. Please try again later.",
                "details": "Hour limit of 5000 requests exceeded"
            }
        }
        </Payload>
        <StatusCode>429</StatusCode>
    </Set>
    <IgnoreUnresolvedVariables>false</IgnoreUnresolvedVariables>
</AssignMessage>
```

### Request/Response Transformation Policies

#### 1. Extract Organization Parameter (AM-ExtractOrgParam.xml)

```xml
<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<ExtractVariables async="false" continueOnError="false" enabled="true" name="AM-ExtractOrgParam">
    <DisplayName>Extract Org Parameter</DisplayName>
    <Properties/>
    <QueryParam name="org">
        <Pattern ignoreCase="true">{org}</Pattern>
    </QueryParam>
    <VariablePrefix>github</VariablePrefix>
    <Source>request</Source>
</ExtractVariables>
```

#### 2. Filter Repository Response (AM-FilterRepositoryResponse.xml)

```xml
<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<JavaScript async="false" continueOnError="false" enabled="true" timeLimit="200" name="AM-FilterRepositoryResponse">
    <DisplayName>Filter Repository Response</DisplayName>
    <Properties/>
    <ResourceURL>jsc://filterRepositoryResponse.js</ResourceURL>
</JavaScript>
```

#### 3. Validate Pull Request Payload (AM-ValidatePRPayload.xml)

```xml
<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<Javascript async="false" continueOnError="false" enabled="true" timeLimit="200" name="AM-ValidatePRPayload">
    <DisplayName>Validate PR Payload</DisplayName>
    <Properties/>
    <ResourceURL>jsc://validatePRPayload.js</ResourceURL>
</Javascript>
```

#### 4. Transform Pull Request Response (AM-TransformPRResponse.xml)

```xml
<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<XMLToJSON async="false" continueOnError="false" enabled="true" name="AM-TransformPRResponse">
    <DisplayName>Convert XML to JSON</DisplayName>
    <Properties/>
    <Format>yahoo</Format>
    <OutputVariable>response</OutputVariable>
    <Source>response</Source>
</XMLToJSON>
```

#### 5. Remove Sensitive Information (AM-RemoveSensitiveInfo.xml)

```xml
<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<Javascript async="false" continueOnError="false" enabled="true" timeLimit="200" name="AM-RemoveSensitiveInfo">
    <DisplayName>Remove Sensitive Info</DisplayName>
    <Properties/>
    <ResourceURL>jsc://removeSensitiveInfo.js</ResourceURL>
</Javascript>
```

### Logging and Monitoring Policies

#### 1. Request Logging (AM-RequestLogging.xml)

```xml
<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<MessageLogging async="false" continueOnError="false" enabled="true" name="AM-RequestLogging">
    <DisplayName>Log Request</DisplayName>
    <Properties/>
    <Syslog>
        <Message>[REQUEST] {organization.name} {apiproxy.name} {apiproxy.revision} {system.timestamp} {messageid} {request.verb} {request.uri}</Message>
        <Host>logs.example.com</Host>
        <Port>514</Port>
        <Protocol>TCP</Protocol>
        <FormatMessage>true</FormatMessage>
    </Syslog>
</MessageLogging>
```

#### 2. Response Logging (AM-ResponseLogging.xml)

```xml
<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<MessageLogging async="false" continueOnError="false" enabled="true" name="AM-ResponseLogging">
    <DisplayName>Log Response</DisplayName>
    <Properties/>
    <Syslog>
        <Message>[RESPONSE] {organization.name} {apiproxy.name} {apiproxy.revision} {system.timestamp} {messageid} {response.status.code}</Message>
        <Host>logs.example.com</Host>
        <Port>514</Port>
        <Protocol>TCP</Protocol>
        <FormatMessage>true</FormatMessage>
    </Syslog>
</MessageLogging>
```

## JavaScript Resources

### 1. Filter Repository Response (filterRepositoryResponse.js)

```javascript
var response = JSON.parse(context.getVariable('response.content'));
var filteredResponse = [];

if (Array.isArray(response)) {
    for (var i = 0; i < response.length; i++) {
        var repo = response[i];
        filteredResponse.push({
            id: repo.id,
            name: repo.name,
            full_name: repo.full_name,
            description: repo.description,
            html_url: repo.html_url,
            visibility: repo.visibility,
            default_branch: repo.default_branch,
            created_at: repo.created_at,
            updated_at: repo.updated_at
        });
    }
}

context.setVariable('response.content', JSON.stringify(filteredResponse));
```

### 2. Validate Pull Request Payload (validatePRPayload.js)

```javascript
var payload;

try {
    payload = JSON.parse(context.getVariable('request.content'));
} catch (e) {
    context.setVariable('flow.error', 'Invalid JSON payload');
    context.setVariable('response.status.code', 400);
    context.setVariable('response.content', JSON.stringify({
        error: 'Invalid JSON payload'
    }));
    return;
}

// Validate required fields
if (!payload.title) {
    context.setVariable('flow.error', 'Missing required field: title');
    context.setVariable('response.status.code', 400);
    context.setVariable('response.content', JSON.stringify({
        error: 'Missing required field: title'
    }));
    return;
}

if (!payload.head || !payload.base) {
    context.setVariable('flow.error', 'Missing required fields: head and/or base');
    context.setVariable('response.status.code', 400);
    context.setVariable('response.content', JSON.stringify({
        error: 'Missing required fields: head and/or base'
    }));
    return;
}

// Add SAP-specific metadata if not present
if (!payload.body) {
    payload.body = '';
}

var sapMetadata = '<!-- SAP Metadata\n' +
    'Transaction: ' + context.getVariable('request.header.X-SAP-Transaction-ID') + '\n' +
    'System: ' + context.getVariable('request.header.X-SAP-System') + '\n' +
    'User: ' + context.getVariable('request.header.X-SAP-User') + '\n' +
    '-->';

payload.body = sapMetadata + '\n\n' + payload.body;

// Update the request content
context.setVariable('request.content', JSON.stringify(payload));
```

### 3. Remove Sensitive Information (removeSensitiveInfo.js)

```javascript
var response;

try {
    response = JSON.parse(context.getVariable('response.content'));
} catch (e) {
    // If not JSON, don't modify
    return;
}

// Function to recursively clean objects
function cleanObject(obj) {
    if (typeof obj !== 'object' || obj === null) {
        return obj;
    }
    
    if (Array.isArray(obj)) {
        return obj.map(cleanObject);
    }
    
    var result = {};
    for (var key in obj) {
        // Skip sensitive fields
        if (['node_id', 'private_key', 'token', 'secret', 'password'].indexOf(key) >= 0) {
            continue;
        }
        
        // Recursively clean nested objects
        result[key] = cleanObject(obj[key]);
    }
    
    return result;
}

var cleanedResponse = cleanObject(response);
context.setVariable('response.content', JSON.stringify(cleanedResponse));
```

## OAuth 2.0 Configuration

### GitHub OAuth App Setup

1. Create a GitHub OAuth App in your organization settings
2. Set the callback URL to your API Management OAuth redirect URL
3. Generate a client secret and store it securely

### API Management OAuth Configuration (oauth-config.xml)

```xml
<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<OAuthV2 async="false" continueOnError="false" enabled="true" name="AM-OAuthV2">
    <DisplayName>OAuth v2.0</DisplayName>
    <Properties/>
    <Attributes>
        <Attribute name="token.cache.enabled" ref="token.cache.enabled">true</Attribute>
        <Attribute name="token.cache.size" ref="token.cache.size">1000</Attribute>
    </Attributes>
    <ExternalAuthorization>false</ExternalAuthorization>
    <Operation>GenerateAuthorizationCode</Operation>
    <SupportedGrantTypes>
        <GrantType>authorization_code</GrantType>
    </SupportedGrantTypes>
    <GenerateResponse enabled="true"/>
    <Tokens>
        <Token type="access" expiresIn="3600" algorithm="SHA256withRSA"/>
    </Tokens>
    <GrantTypes>
        <AuthorizationCode>
            <GenerateCallbackURL>true</GenerateCallbackURL>
            <CodeLifetimeInSeconds>600</CodeLifetimeInSeconds>
            <LoginURL>https://github.com/login/oauth/authorize</LoginURL>
            <ClientAuthentication>
                <Type>basic</Type>
            </ClientAuthentication>
        </AuthorizationCode>
    </GrantTypes>
</OAuthV2>
```

## Key Value Map Configuration

Create the following Key Value Maps in your API Management instance:

### GitHub Configuration KVM

| Key | Value | Description |
|-----|-------|-------------|
| `github.api.url` | `https://api.github.com` | GitHub API base URL |
| `github.token` | `ghp_123456789abcdef` | GitHub Personal Access Token |
| `github.oauth.client_id` | `abc123def456` | GitHub OAuth App Client ID |
| `github.oauth.client_secret` | `xyz789uvw123` | GitHub OAuth App Client Secret |

### SAP Configuration KVM

| Key | Value | Description |
|-----|-------|-------------|
| `sap.api.url` | `https://example-api.sap.com` | SAP API base URL |
| `sap.system.id` | `PRD` | SAP System ID |
| `sap.client` | `100` | SAP Client |

## API Product Configuration

```xml
<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<APIProduct name="SAP-GitHub-Integration">
    <DisplayName>SAP GitHub Integration</DisplayName>
    <Description>API Product for SAP GitHub Integration</Description>
    <ApiResources>
        <ApiResource>GitHub-Integration-Proxy</ApiResource>
    </ApiResources>
    <ApprovalType>auto</ApprovalType>
    <Attributes>
        <Attribute name="access">public</Attribute>
        <Attribute name="quota">5000 per hour</Attribute>
    </Attributes>
    <Environments>
        <Environment>prod</Environment>
        <Environment>test</Environment>
    </Environments>
    <Proxies>
        <Proxy>GitHub-Integration-Proxy</Proxy>
    </Proxies>
</APIProduct>
```

## Usage Examples

### 1. Get Organization Repositories

**Request:**
```
GET /github/v1/repositories?org=MyOrganization HTTP/1.1
Host: api.example.com
x-api-key: abcd1234efgh5678
```

**Response:**
```json
[
    {
        "id": 12345678,
        "name": "sap-integration",
        "full_name": "MyOrganization/sap-integration",
        "description": "SAP integration components",
        "html_url": "https://github.com/MyOrganization/sap-integration",
        "visibility": "internal",
        "default_branch": "main",
        "created_at": "2023-01-15T10:30:15Z",
        "updated_at": "2023-10-20T14:22:33Z"
    },
    {
        "id": 23456789,
        "name": "abap-utilities",
        "full_name": "MyOrganization/abap-utilities",
        "description": "ABAP utility functions and classes",
        "html_url": "https://github.com/MyOrganization/abap-utilities",
        "visibility": "internal",
        "default_branch": "main",
        "created_at": "2023-03-05T09:15:22Z",
        "updated_at": "2023-10-18T11:45:12Z"
    }
]
```

### 2. Create Pull Request

**Request:**
```
POST /github/v1/repos/MyOrganization/sap-integration/pulls HTTP/1.1
Host: api.example.com
x-api-key: abcd1234efgh5678
Content-Type: application/json
X-SAP-Transaction-ID: 45000123
X-SAP-System: PRD
X-SAP-User: DEVELOPER1

{
    "title": "Fix customer invoice calculation",
    "head": "feature/invoice-fix",
    "base": "main",
    "body": "This pull request fixes the customer invoice calculation bug reported in SAP incident #12345."
}
```

**Response:**
```json
{
    "id": 123456789,
    "number": 42,
    "title": "Fix customer invoice calculation",
    "state": "open",
    "html_url": "https://github.com/MyOrganization/sap-integration/pull/42",
    "created_at": "2023-10-21T15:45:22Z",
    "updated_at": "2023-10-21T15:45:22Z",
    "head": {
        "ref": "feature/invoice-fix",
        "sha": "abcdef1234567890"
    },
    "base": {
        "ref": "main",
        "sha": "1234567890abcdef"
    }
}
```

## Best Practices

### Security Considerations

1. **Credential Management**
   - Store all GitHub tokens and credentials in secure Key Value Maps
   - Rotate tokens regularly
   - Use minimal permission scopes for tokens

2. **Request Validation**
   - Validate all incoming requests
   - Implement strict schema validation for payloads
   - Use OAuth 2.0 for user authentication where possible

3. **Response Filtering**
   - Filter sensitive information from responses
   - Only return necessary data fields
   - Implement proper error handling

### Performance Optimization

1. **Rate Limiting**
   - Implement client-specific rate limits
   - Respect GitHub's API rate limits
   - Use spike arrest to prevent traffic surges

2. **Caching**
   - Cache frequently accessed resources
   - Implement conditional requests with ETags
   - Set appropriate cache headers

3. **Payload Optimization**
   - Minimize payload sizes
   - Use compression for responses
   - Implement pagination for large data sets

### Monitoring and Analytics

1. **Logging**
   - Log all API requests and responses
   - Implement structured logging
   - Set up alert thresholds

2. **Error Tracking**
   - Track error rates and types
   - Implement detailed error reporting
   - Set up notifications for critical errors

3. **Performance Metrics**
   - Monitor response times
   - Track API usage by client
   - Generate regular usage reports

## Troubleshooting

### Common Issues and Solutions

1. **GitHub API Rate Limiting**
   - **Problem**: GitHub API returns 403 with "API rate limit exceeded"
   - **Solution**: Implement proper rate limiting and throttling in API Management policies

2. **Authentication Failures**
   - **Problem**: GitHub API returns 401 Unauthorized
   - **Solution**: Verify token validity and permissions; check if token is expired or revoked

3. **SSL/TLS Issues**
   - **Problem**: Failed to connect to GitHub API due to SSL errors
   - **Solution**: Update certificates and ensure proper TLS configuration

## Next Steps

- Implement [GitHub Webhook Processing](./webhook-processing.md) for event-driven integration
- Configure [Security Settings](../security/config/oauth-configuration.md) for secure connectivity
- Set up comprehensive system connectivity configurations
- Explore [ABAP Integration Examples](../abap-integration/abapgit-integration.md) for SAP-specific integration
