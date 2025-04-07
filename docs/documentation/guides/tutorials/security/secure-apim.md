# 📄 Securing API Management for SAP-GitHub Integration

## 📋 Table of Contents

- [📋 Overview](#overview)
- [✅ Prerequisites](#prerequisites)
- [Understanding the Security Challenges](#understanding-the-security-challenges)
- [Step-by-Step Implementation](#step-by-step-implementation)
- [End-to-End Security Testing Scenarios](#end-to-end-security-testing-scenarios)
- [Troubleshooting](#troubleshooting)
- [➡
️ ️ Next Steps](#next-steps)


## 📋 Overview

This tutorial guides you through securing the Microsoft API Management (APIM) layer that facilitates communication between SAP systems and GitHub. You'll learn how to implement robust security controls, configure proper authentication and authorization, and establish comprehensive monitoring to protect this critical integration point.

## ✅ Prerequisites

Before starting this tutorial, ensure you have:

- Microsoft API Management instance deployed
- Access to Azure Portal with appropriate permissions
- SAP and GitHub endpoints configured
- Basic understanding of API security concepts
- Microsoft Entra ID (Azure AD) tenant for authentication

## Understanding the Security Challenges

The API Management layer between SAP and GitHub presents several security challenges:

1. **Authentication**: Ensuring only authorized systems and users can access APIs
2. **Authorization**: Providing appropriate access levels to different consumers
3. **Data Protection**: Encrypting sensitive data and preventing data leakage
4. **Threat Protection**: Defending against API-specific threats like injection and DoS
5. **Logging and Monitoring**: Detecting and responding to suspicious activities

## Step-by-Step Implementation

### 1. Configure TLS/SSL for API Endpoints

First, ensure all API communication is encrypted:

1. In the Azure Portal, navigate to your API Management instance
2. Go to "Custom domains" under Settings
3. For each hostname, enable HTTPS by configuring a certificate:
   - You can use an Azure Key Vault certificate
   - Or upload a PFX certificate directly
4. Click "Add" to configure each hostname with its certificate
5. Set the minimum TLS version to 1.2:
   - Go to "Protocols" under Settings
   - Disable TLS 1.0 and TLS 1.1
   - Select only TLS 1.2 and TLS 1.3 (if available)

### 2. Implement OAuth 2.0 Authentication

Set up OAuth 2.0 for secure authentication:

1. Create an OAuth 2.0 authorization server in APIM:
   - Go to "OAuth 2.0 + OpenID Connect" under Security
   - Click "Add"
   - Name: "SAP-GitHub-Auth"
   - Client registration URL: Enter your registration page URL
   - Authorization endpoint URL: `https://login.microsoftonline.com/{tenant-id}/oauth2/v2.0/authorize`
   - Token endpoint URL: `https://login.microsoftonline.com/{tenant-id}/oauth2/v2.0/token`
   - Default scope: `https://api.example.com/sap-github-integration/.default`

2. Register an application in Microsoft Entra ID:
   - Navigate to "App registrations" in Entra ID
   - Click "New registration"
   - Name: "SAP-GitHub Integration"
   - Supported account types: Select appropriate option based on your requirements
   - Redirect URI: Add the callback URL from your APIM
   - Click "Register"

3. Configure client credentials:
   - Under the registered app, go to "Certificates & secrets"
   - Create a new client secret
   - Note the client ID and secret for use in APIM

4. Configure OAuth in your API:
   - In your API definition, go to "Settings"
   - For "User Authorization", select the OAuth 2.0 server created earlier
   - For "Authorization Scope", add your required scopes

### 3. Configure IP Restrictions and Network Security

Limit API access to specific network ranges:

1. Implement IP restrictions:
   - Go to "Security" > "Firewalls" in your APIM
   - Add allowed IP addresses or ranges for:
     - SAP systems (typically inside corporate network)
     - GitHub Enterprise (if using GitHub Enterprise Server)
     - Admin access IPs
   - Choose "Allow access only from specified IP addresses and address ranges"

2. Configure Virtual Network integration:
   - Go to "Deployment" > "Virtual Network" in your APIM
   - Select "Internal" or "External" mode based on your requirements
   - Configure your VNet and subnet settings
   - This isolates your API Management from the public internet

3. Set up Private Endpoints for improved security:
   - Go to "Private endpoint connections" in your APIM
   - Click "Private endpoint" and then "Create"
   - Follow the wizard to create a private endpoint
   - Configure DNS settings to resolve your API hostname to the private IP

### 4. Implement API Policies for Security

Configure policies to enhance API security:

1. Create an inbound policy at the API level with these security measures:
   - Go to your API in APIM
   - Click on "All operations"
   - Select the "Inbound processing" policy editor
   - Add the following XML policy:

```xml
<policies>
    <inbound>
        <!-- Check content type -->
        <check-header name="Content-Type" failed-check-httpcode="415" failed-check-error-message="Unsupported Content Type" ignore-case="true">
            <value>application/json</value>
            <value>application/xml</value>
        </check-header>
        
        <!-- Validate JWT token -->
        <validate-jwt header-name="Authorization" failed-validation-httpcode="401" failed-validation-error-message="Invalid or missing JWT token">
            <openid-config url="https://login.microsoftonline.com/{tenant-id}/.well-known/openid-configuration" />
            <audiences>
                <audience>{your-app-client-id}</audience>
            </audiences>
            <required-claims>
                <claim name="scp" match="any">
                    <value>SAP.Read</value>
                    <value>SAP.Write</value>
                </claim>
            </required-claims>
        </validate-jwt>
        
        <!-- Rate limiting -->
        <rate-limit-by-key calls="30" renewal-period="60" counter-key="@(context.Request.IpAddress)" />
        
        <!-- Validate request content -->
        <choose>
            <when condition="@(context.Request.Method == "POST" || context.Request.Method == "PUT")">
                <validate-content unspecified-content-type-action="prevent" max-size="1024" size-exceeded-action="prevent" />
            </when>
        </choose>
        
        <!-- Log IP address -->
        <set-variable name="clientIpAddress" value="@(context.Request.IpAddress)" />
        
        <base />
    </inbound>
    <backend>
        <base />
    </backend>
    <outbound>
        <base />
        
        <!-- Security headers -->
        <set-header name="X-Content-Type-Options" exists-action="override">
            <value>nosniff</value>
        </set-header>
        <set-header name="Strict-Transport-Security" exists-action="override">
            <value>max-age=31536000; includeSubDomains</value>
        </set-header>
        <set-header name="X-Frame-Options" exists-action="override">
            <value>deny</value>
        </set-header>
        <set-header name="Content-Security-Policy" exists-action="override">
            <value>default-src 'self'</value>
        </set-header>
        
        <!-- Remove sensitive headers -->
        <set-header name="Server" exists-action="delete" />
    </outbound>
    <on-error>
        <base />
        
        <!-- Don't expose error details -->
        <set-body>{"error": "An error occurred processing your request."}</set-body>
    </on-error>
</policies>
```

2. Create a policy for sensitive operations (like transport creation) with additional security:

```xml
<policies>
    <inbound>
        <base />
        
        <!-- Additional validation for sensitive operations -->
        <validate-jwt header-name="Authorization" failed-validation-httpcode="401" failed-validation-error-message="Invalid or missing JWT token">
            <openid-config url="https://login.microsoftonline.com/{tenant-id}/.well-known/openid-configuration" />
            <required-claims>
                <claim name="roles" match="any">
                    <value>SAP.Transport.Create</value>
                    <value>SAP.Admin</value>
                </claim>
            </required-claims>
        </validate-jwt>
        
        <!-- IP allowlist for sensitive operations -->
        <ip-filter action="allow">
            <address-range from="10.0.0.0" to="10.255.255.255" />
            <address-range from="172.16.0.0" to="172.31.255.255" />
            <address-range from="192.168.0.0" to="192.168.255.255" />
        </ip-filter>
    </inbound>
    <backend>
        <base />
    </backend>
    <outbound>
        <base />
    </outbound>
    <on-error>
        <base />
    </on-error>
</policies>
```

### 5. Implement Mutual TLS Authentication

For highly sensitive operations, set up mutual TLS authentication:

1. Configure client certificate authentication:
   - Go to "Custom domains" in your APIM
   - Select a hostname and click "Edit"
   - Enable "Request client certificate"

2. Create a policy to validate the client certificate:

```xml
<policies>
    <inbound>
        <base />
        <choose>
            <when condition="@(!context.Request.Certificate.NotBefore.HasValue || !context.Request.Certificate.NotAfter.HasValue || !(context.Request.Certificate.NotBefore.Value &lt;= DateTime.Now &amp;&amp; context.Request.Certificate.NotAfter.Value &gt; DateTime.Now))">
                <return-response>
                    <set-status code="403" reason="Invalid client certificate" />
                    <set-body>Client certificate validation failed.</set-body>
                </return-response>
            </when>
            <when condition="@(context.Request.Certificate.Issuer != "CN=YourTrustedCA")">
                <return-response>
                    <set-status code="403" reason="Untrusted certificate issuer" />
                    <set-body>Untrusted certificate issuer.</set-body>
                </return-response>
            </when>
        </choose>
    </inbound>
    <backend>
        <base />
    </backend>
    <outbound>
        <base />
    </outbound>
    <on-error>
        <base />
    </on-error>
</policies>
```

### 6. Set Up Advanced Threat Protection

Configure Azure Web Application Firewall (WAF) for APIM:

1. Create an Application Gateway with WAF:
   - Go to "Application gateways" in Azure Portal
   - Click "Add"
   - Configure basic settings, frontend IP, and backend pool pointing to your APIM
   - In Settings, select "WAF V2" for the tier
   - Complete the configuration and create the gateway

2. Configure WAF policy:
   - Go to "Web Application Firewall policies"
   - Create a new policy
   - Enable OWASP 3.2 rule set
   - Configure custom rules for:
     - Rate limiting
     - Geographic restrictions if needed
     - Known bad IP addresses

3. Associate WAF policy with your Application Gateway:
   - Navigate to your Application Gateway
   - Go to "Web application firewall" under Settings
   - Associate the created WAF policy

### 7. Configure Comprehensive Logging and Monitoring

Set up detailed logging for security monitoring:

1. Enable diagnostic settings for APIM:
   - Go to "Diagnostic settings" under Monitoring in your APIM
   - Click "Add diagnostic setting"
   - Name: "SecurityMonitoring"
   - Select all log categories, especially:
     - GatewayLogs
     - RequestLogs
     - BackendLogs
   - Send logs to Log Analytics workspace
   - Click "Save"

2. Configure API request logging with sensitive data masking:

```xml
<policies>
    <inbound>
        <base />
        
        <!-- Log API requests but mask sensitive data -->
        <set-variable name="requestBody" value="@(context.Request.Body.As<string>(preserveContent: true))" />
        <set-variable name="maskedRequestBody" value="@{
            var body = context.Variables.GetValueOrDefault<string>("requestBody");
            if (body != null) {
                // Mask sensitive fields
                body = Regex.Replace(body, "\"password\":\\s*\"[^\"]*\"", "\"password\":\"******\"");
                body = Regex.Replace(body, "\"api_key\":\\s*\"[^\"]*\"", "\"api_key\":\"******\"");
                return body;
            }
            return null;
        }" />
        
        <trace source="API-Security-Trace" severity="information">
            <message>@($"API Request: {context.Request.Method} {context.Request.Url.Path}")</message>
            <metadata name="RequestBody" value="@(context.Variables.GetValueOrDefault<string>("maskedRequestBody"))" />
            <metadata name="ClientIP" value="@(context.Request.IpAddress)" />
            <metadata name="ClientCert" value="@(context.Request.Certificate != null ? "Present" : "Not Present")" />
        </trace>
    </inbound>
    <backend>
        <base />
    </backend>
    <outbound>
        <base />
    </outbound>
    <on-error>
        <base />
    </on-error>
</policies>
```

3. Set up Azure Monitor alerts for security events:
   - Go to "Alerts" under Monitoring in Azure Portal
   - Click "New alert rule"
   - Select your APIM resource
   - Configure alert conditions for:
     - High failure rates (4xx/5xx responses)
     - Unusual traffic patterns
     - Authentication failures

4. Create a Log Analytics query for security monitoring:

```kusto
// Failed authentication attempts
ApiManagementGatewayLogs
| where TimeGenerated > ago(1d)
| where ResponseCode == 401 or ResponseCode == 403
| summarize FailedAuthCount = count() by ClientIP = CallerIpAddress, bin(TimeGenerated, 5m)
| where FailedAuthCount > 5
| project TimeGenerated, ClientIP, FailedAuthCount

// Potential injection attacks
ApiManagementGatewayLogs
| where TimeGenerated > ago(1d)
| where UrlPath contains "'" or UrlPath contains "=" or UrlPath contains "--" or UrlPath contains "/*" 
  or RequestBody contains "'" or RequestBody contains "=" or RequestBody contains "--" or RequestBody contains "/*"
| project TimeGenerated, ClientIP = CallerIpAddress, UrlPath, Method, RequestBody

// Unusual traffic patterns
ApiManagementGatewayLogs
| where TimeGenerated > ago(7d)
| summarize RequestsPerDay = count() by Day = bin(TimeGenerated, 1d)
| summarize AvgRequests = avg(RequestsPerDay), StdDev = stdev(RequestsPerDay) by Day
| join kind=inner (
    ApiManagementGatewayLogs
    | where TimeGenerated > ago(1d)
    | summarize TodayRequests = count() by Day = bin(TimeGenerated, 1d)
) on Day
| where TodayRequests > AvgRequests + 2*StdDev
| project Day, TodayRequests, AvgRequests, StdDev
```

### 8. Implement Proper Secret Management

Configure secure handling of credentials and secrets:

1. Use Azure Key Vault for secret storage:
   - Go to "Key vaults" in Azure Portal
   - Create a new key vault or use an existing one
   - Add secrets for:
     - GitHub API tokens
     - SAP system credentials
     - Certificate passwords

2. Configure APIM to use Key Vault:
   - Go to "Managed identities" under Settings in your APIM
   - Enable system-assigned managed identity
   - Go to your Key Vault and grant the managed identity "Get" permissions on secrets

3. Create a policy to retrieve secrets from Key Vault:

```xml
<policies>
    <inbound>
        <base />
        
        <!-- Get secret from Key Vault -->
        <send-request mode="new" response-variable-name="keyVaultResponse" timeout="20" ignore-error="false">
            <set-url>https://{key-vault-name}.vault.azure.net/secrets/{secret-name}?api-version=7.3</set-url>
            <set-method>GET</set-method>
            <authentication-managed-identity resource="https://vault.azure.net" />
        </send-request>
        
        <!-- Extract the secret value -->
        <set-variable name="secretValue" value="@{
            var response = ((IResponse)context.Variables["keyVaultResponse"]);
            if (response.StatusCode == 200) {
                var secret = JObject.Parse(response.Body.As<string>());
                return secret["value"].ToString();
            }
            return null;
        }" />
        
        <!-- Use the secret -->
        <set-header name="Authorization" exists-action="override">
            <value>@($"Bearer {context.Variables.GetValueOrDefault<string>("secretValue")}")</value>
        </set-header>
    </inbound>
    <backend>
        <base />
    </backend>
    <outbound>
        <base />
    </outbound>
    <on-error>
        <base />
    </on-error>
</policies>
```

### 9. Implement Data Loss Prevention

Configure policies to prevent sensitive data exposure:

1. Create an outbound policy to scan and mask sensitive information:

```xml
<policies>
    <inbound>
        <base />
    </inbound>
    <backend>
        <base />
    </backend>
    <outbound>
        <base />
        
        <!-- Scan for and mask sensitive data patterns -->
        <choose>
            <when condition="@(context.Response.StatusCode == 200 && context.Response.Body.As<JObject>(preserveContent: true) != null)">
                <set-body>@{
                    var responseBody = context.Response.Body.As<JObject>(preserveContent: true);
                    
                    // Function to recursively process JSON objects
                    Action<JToken> maskSensitiveData = null;
                    maskSensitiveData = (token) => {
                        if (token is JObject obj)
                        {
                            foreach (var property in obj.Properties().ToList())
                            {
                                // Check for sensitive property names
                                if (property.Name.Equals("password", StringComparison.OrdinalIgnoreCase) ||
                                    property.Name.Equals("apiKey", StringComparison.OrdinalIgnoreCase) ||
                                    property.Name.Equals("secret", StringComparison.OrdinalIgnoreCase) ||
                                    property.Name.Equals("credential", StringComparison.OrdinalIgnoreCase))
                                {
                                    property.Value = "********";
                                }
                                else if (property.Value is JObject || property.Value is JArray)
                                {
                                    maskSensitiveData(property.Value);
                                }
                                else if (property.Value is JValue val && val.Type == JTokenType.String)
                                {
                                    // Check for patterns: SSN, Credit Card, etc.
                                    string strValue = val.Value<string>();
                                    if (strValue != null)
                                    {
                                        // SSN pattern: XXX-XX-XXXX
                                        if (Regex.IsMatch(strValue, @"\b\d{3}-\d{2}-\d{4}\b"))
                                        {
                                            property.Value = "XXX-XX-XXXX";
                                        }
                                        // Credit card pattern
                                        else if (Regex.IsMatch(strValue, @"\b(?:\d{4}[- ]?){3}\d{4}\b"))
                                        {
                                            property.Value = "XXXX-XXXX-XXXX-XXXX";
                                        }
                                    }
                                }
                            }
                        }
                        else if (token is JArray array)
                        {
                            foreach (var item in array)
                            {
                                maskSensitiveData(item);
                            }
                        }
                    };
                    
                    // Process the entire response body
                    maskSensitiveData(responseBody);
                    return responseBody.ToString();
                }</set-body>
            </when>
        </choose>
    </outbound>
    <on-error>
        <base />
    </on-error>
</policies>
```

### 10. Validate Security Configuration

Test your security configuration with these steps:

1. Run a comprehensive API security scan:
   - Use a tool like OWASP ZAP or Postman to test your API endpoints
   - Test for common API vulnerabilities:
     - Injection attacks
     - Authentication bypass
     - Authorization flaws

2. Verify rate limiting works:
   - Send multiple requests in quick succession
   - Confirm that requests are rejected after exceeding the limit

3. Test authentication:
   - Try accessing endpoints without authentication
   - Use expired tokens
   - Use valid tokens with insufficient permissions

4. Review logs for security events:
   - Check Log Analytics for any security-related events
   - Verify that alerts trigger appropriately

## End-to-End Security Testing Scenarios

Test these common scenarios to validate your security implementation:

### Scenario 1: Authentication Bypass Attempt

Test that unauthorized access is properly prevented:

1. Attempt to access API without authentication
2. Try using expired tokens
3. Use tokens with insufficient permissions
4. Verify proper 401/403 responses
5. Check logs for authentication failure events

### Scenario 2: Data Security

Verify that sensitive data is properly protected:

1. Make requests containing sensitive information
2. Review responses to ensure sensitive data is masked
3. Check backend logs to verify credentials aren't exposed
4. Validate that TLS encryption is enforced

### Scenario 3: Access Control

Test authorization boundaries:

1. Create test accounts with different permission levels
2. Try accessing endpoints with various permission combinations
3. Verify that operations are correctly allowed or denied
4. Test boundary cases like expired credentials or edge permissions

## Troubleshooting

| Issue | Solution |
|-------|----------|
| Authentication failures | Check key vault access, secret expiration, and token validation parameters |
| JWT validation errors | Verify your tenant ID, audience, and issuer settings in the validate-jwt policy |
| Rate limiting not working | Check policy application scope and counter-key configuration |
| Mutual TLS failures | Verify certificate validity, trust chain, and client configuration |
| WAF blocking legitimate traffic | Review WAF logs, adjust rule sensitivity, or create exception rules |

## ➡️ Next Steps

After securing your API Management layer, consider these advanced configurations:

- [Implementing Custom Security Scanning](./custom-scanning.md) for your ABAP code
- [Setting Up Microsoft Sentinel for SAP Monitoring](./sentinel-setup.md) to integrate API security monitoring
- Implementing automated security testing in your CI/CD pipeline
- Creating a comprehensive security incident response plan

---




