---
title: Enhanced Authentication Framework
description: Comprehensive authentication guide for SAP-GitHub integration using GitHub Apps and SAP Cloud Identity Services
author: SAP-GitHub Integration Team
date: 2024-05-15
version: 1.0.0
---

# Enhanced Authentication Framework

## Table of Contents

- [Overview](#overview)
- [GitHub Apps Authentication](#github-apps-authentication)
  - [Benefits Over Personal Access Tokens](#benefits-over-personal-access-tokens)
  - [Permission Scoping](#permission-scoping)
  - [Installation Flow](#installation-flow)
  - [Token Lifecycle Management](#token-lifecycle-management)
  - [Implementation Steps](#implementation-steps)
- [SAP Cloud Identity Services Integration](#sap-cloud-identity-services-integration)
  - [BTP Cockpit Configuration](#btp-cockpit-configuration)
  - [Trust Configuration](#trust-configuration)
  - [User Mapping](#user-mapping)
  - [Single Sign-On Setup](#single-sign-on-setup)
- [Multi-Factor Authentication](#multi-factor-authentication)
  - [MFA for GitHub Access](#mfa-for-github-access)
  - [MFA for SAP Systems](#mfa-for-sap-systems)
- [Cross-System Authentication](#cross-system-authentication)
  - [OAuth 2.0 Flow](#oauth-20-flow)
  - [JWT-Based Authentication](#jwt-based-authentication)
- [Audit and Compliance](#audit-and-compliance)
  - [Authentication Events Logging](#authentication-events-logging)
  - [Compliance Reporting](#compliance-reporting)
- [Troubleshooting](#troubleshooting)
- [Best Practices](#best-practices)
- [References](#references)

## Overview

This guide details a comprehensive authentication framework for SAP-GitHub integration. It covers implementing GitHub Apps for secure repository access, integrating SAP Cloud Identity Services for unified identity management, and establishing cross-system authentication patterns.

Modern enterprise integrations require a robust authentication approach that:
- Enforces least privilege principles
- Provides granular access controls
- Supports automatic credential rotation
- Offers detailed audit trails
- Complies with regulatory requirements

The recommended architecture uses GitHub Apps for GitHub access and SAP Cloud Identity Services for unified identity management across SAP landscape components.

## GitHub Apps Authentication

GitHub Apps provide a more secure, flexible alternative to personal access tokens for GitHub authentication. They operate as independent entities with their own identity and permissions.

### Benefits Over Personal Access Tokens

GitHub Apps offer significant improvements over PATs (Personal Access Tokens):

| Feature | GitHub Apps | Personal Access Tokens |
|---------|------------|------------------------|
| Permission granularity | Repository-specific | User-wide |
| Token lifespan | Short-lived (1 hour max) | Long-lived (up to 1 year) |
| Installation scope | Specific repositories or organizations | All user accessible resources |
| Rate limiting | Per-installation limits | Shared across user |
| Credential security | Private key stays on server | Token must be distributed |
| Webhook events | Receives repository-specific events | Requires separate webhook registration |

### Permission Scoping

Configure GitHub Apps with the following permission scoping approach:

1. **Repository-level permissions**: Grant only the specific permissions needed for each repository

   | Permission Category | Required Level | Justification |
   |---------------------|----------------|--------------|
   | Contents | Read/Write | For code operations and commits |
   | Metadata | Read-only | For repository information |
   | Pull requests | Read/Write | For automated PR creation/reviews |
   | Workflows | Read/Write | For CI/CD operations |
   | Commit statuses | Read/Write | For build status reporting |
   | Issues | Read/Write | For automated issue management |

2. **Organization-level permissions**: If organization-wide access is needed, restrict to only:
   - Membership (Read-only)
   - Team discussions (Read-only)

3. **User-level permissions**: Avoid requesting user-level permissions unless absolutely necessary

[DIAGRAM: GitHub Apps permission scoping flowchart showing decision points for granting minimal permissions]

### Installation Flow

The GitHub App installation process follows this sequence:

1. **App Registration**: 
   - Create GitHub App in organization settings
   - Configure permissions and webhook endpoints
   - Generate private key for authentication

2. **Installation URL Creation**:
   - Generate installation URL for the app
   - Distribute to repository/organization administrators

3. **User Installation**:
   - Admin accesses installation URL
   - Selects repositories for installation
   - Approves requested permissions

4. **Installation ID Receipt**:
   - App receives installation event webhook
   - Stores installation ID for future token generation

5. **Server Configuration**:
   - Integration server stores private key securely
   - Associates installation IDs with repositories
   - Sets up token generation process

[DIAGRAM: Sequence diagram showing installation flow from creation to usage]

### Token Lifecycle Management

GitHub App tokens are short-lived (1 hour maximum) and require proper lifecycle management:

1. **Token Generation**:
   - Generate JWT signed with private key
   - Exchange JWT for installation token using GitHub API
   - Cache token until near expiration

2. **Token Rotation**:
   - Monitor token expiration (typically set to expire after 1 hour)
   - Generate new token before expiration
   - Implement fallback mechanism if token generation fails

3. **Token Revocation**:
   - Implement mechanisms to revoke tokens if compromise is suspected
   - Handle GitHub App uninstallation events properly

```java
// Example token lifecycle management in Java
public class GitHubAppTokenManager {
    private final String appId;
    private final String privateKeyPath;
    private String currentToken;
    private long tokenExpiresAt;
    
    // Token generation with automatic renewal
    public synchronized String getToken() {
        if (currentToken == null || System.currentTimeMillis() > tokenExpiresAt - 300000) {
            // Token null or expiring in 5 minutes, generate new token
            try {
                String jwt = generateJWT();
                TokenResponse response = exchangeJWTForToken(jwt);
                currentToken = response.getToken();
                // Set expiry 5 minutes before actual to ensure seamless rotation
                tokenExpiresAt = System.currentTimeMillis() + (response.getExpiresIn() * 1000) - 300000;
            } catch (Exception e) {
                log.error("Failed to generate token", e);
                throw new AuthenticationException("Unable to authenticate with GitHub");
            }
        }
        return currentToken;
    }
    
    // Methods for JWT generation and exchange
    private String generateJWT() { /* ... */ }
    private TokenResponse exchangeJWTForToken(String jwt) { /* ... */ }
}
```

### Implementation Steps

Follow these steps to implement GitHub Apps authentication:

1. **Create a GitHub App**:
   ```
   GitHub Organization Settings > Developer settings > GitHub Apps > New GitHub App
   ```
   
   - Set app name and homepage URL
   - Disable webhook (if not needed)
   - Set permissions as per [Permission Scoping](#permission-scoping)
   - Generate and download private key

2. **Store App Credentials Securely**:
   - Store the App ID (visible in app settings)
   - Securely store the private key in a secrets manager
   - Never commit these values to source control

3. **Implement JWT Generation**:
   ```java
   private String generateJWT() throws JOSEException, IOException {
       // Load private key
       PrivateKey privateKey = loadPrivateKey();
       
       // Create JWT builder
       JWTClaimsSet claims = new JWTClaimsSet.Builder()
           .issuer(appId)
           .issueTime(new Date())
           .expirationTime(new Date(System.currentTimeMillis() + 600000)) // 10 min
           .build();
       
       // Sign JWT
       JWSHeader header = new JWSHeader.Builder(JWSAlgorithm.RS256).build();
       SignedJWT signedJWT = new SignedJWT(header, claims);
       JWSSigner signer = new RSASSASigner(privateKey);
       signedJWT.sign(signer);
       
       return signedJWT.serialize();
   }
   ```

4. **Implement Token Exchange**:
   ```java
   private TokenResponse exchangeJWTForToken(String jwt) {
       // Create HTTP client
       HttpClient client = HttpClient.newBuilder().build();
       
       // Create request for token
       HttpRequest request = HttpRequest.newBuilder()
           .uri(URI.create("https://api.github.com/app/installations/" + installationId + "/access_tokens"))
           .header("Authorization", "Bearer " + jwt)
           .header("Accept", "application/vnd.github+json")
           .POST(HttpRequest.BodyPublishers.noBody())
           .build();
       
       // Send request and parse response
       try {
           HttpResponse<String> response = client.send(request, HttpResponse.BodyHandlers.ofString());
           return parseTokenResponse(response.body());
       } catch (Exception e) {
           throw new RuntimeException("Failed to exchange JWT for token", e);
       }
   }
   ```

5. **Configure Installation**:
   - Generate installation URL from app settings
   - Send URL to repository owners
   - Store installation IDs returned after installation

6. **Integrate with GitHub API**:
   - Use token manager in API clients
   - Handle token expiration automatically
   - Implement proper error handling

## SAP Cloud Identity Services Integration

SAP Cloud Identity Services provides unified identity management for SAP and non-SAP systems. Integrating with GitHub enables single sign-on and centralized identity governance.

### BTP Cockpit Configuration

1. **Enable Identity Authentication Service**:
   - Log in to SAP BTP Cockpit
   - Navigate to **Services > Instances and Subscriptions**
   - Subscribe to **Identity Authentication**
   - Access the admin console

2. **Configure Identity Provider**:
   - In Identity Authentication admin console:
   - Navigate to **Applications & Resources > Applications**
   - Click **+ Create** to add a new application
   - Select **OpenID Connect Configuration**

3. **Configure OIDC Settings**:
   ```
   Protocol: OpenID Connect
   Client ID: [Generate or Enter Client ID]
   Secret: [Generate Secret]
   Redirect URIs: https://github.com/login/oauth/authorize
   Grant Types: Authorization Code, Refresh Token
   ```

[SCREENSHOT: BTP Cockpit Identity Authentication Service configuration]

### Trust Configuration

1. **Configure Federation with GitHub**:
   - Navigate to **Security > Trust Configuration** in BTP Cockpit
   - Select **Known Provider > SAML 2.0** 
   - Configure GitHub as a trusted identity provider

2. **Exchange Metadata**:
   - Download SAP IAS SAML metadata
   - Go to GitHub Organization Settings > Authentication
   - Click "Configure SAML SSO"
   - Upload the IAS metadata file

3. **Verify Trust**:
   - Test authentication flow from IAS to GitHub
   - Ensure SAML assertions are properly configured

[DIAGRAM: Trust configuration relationship between SAP IAS and GitHub]

### User Mapping

1. **Attribute Mapping Configuration**:
   - In SAP IAS, navigate to **Identity Provisioning**
   - Create a source system pointing to your central identity system
   - Create a target system pointing to GitHub
   - Configure attribute mappings:

   | SAP Attribute | GitHub Attribute |
   |---------------|------------------|
   | mail | email |
   | uid | login |
   | displayName | name |
   | groups | team_memberships |

2. **Group Mapping**:
   - Map SAP authorization groups to GitHub teams
   - Configure provisioning rules based on role assignments
   - Set up synchronization schedule

3. **Test User Provisioning**:
   - Provision test users to GitHub
   - Verify attributes are correctly mapped
   - Test authentication flows

```json
// Example IAS to GitHub attribute mapping
{
  "mappings": [
    {
      "sourcePath": "$.user.userName",
      "targetPath": "$.user.userName",
      "preserveArrayWithSingleElement": true
    },
    {
      "sourcePath": "$.user.emails[*].value",
      "targetPath": "$.user.emails[0].value",
      "preserveArrayWithSingleElement": true
    },
    {
      "condition": "$.user.groups[*].value CONTAINS 'SAP_DEVELOPERS'",
      "targetPath": "$.user.groups[0].members[?(@.value == 'github-developers')]"
    }
  ]
}
```

### Single Sign-On Setup

1. **GitHub Enterprise Configuration**:
   - Go to your GitHub Enterprise settings
   - Navigate to **Authentication > SAML configuration**
   - Enter the following details:
     - Sign-on URL: https://[your-tenant].accounts.ondemand.com/saml2/idp/sso
     - Issuer URL: https://[your-tenant].accounts.ondemand.com
     - Public Certificate: [Upload IAS certificate]

2. **Test SSO Flow**:
   - Initiate login from GitHub using SSO option
   - Verify redirection to SAP IAS login page
   - Check successful return to GitHub after authentication

3. **Enforce SSO**:
   - Optional: Enable SSO enforcement for organization
   - Configure recovery options for administrators
   - Plan for staged rollout to users

[SCREENSHOT: Successful SSO login flow from SAP IAS to GitHub]

## Multi-Factor Authentication

### MFA for GitHub Access

1. **Configure MFA Requirements**:
   - In GitHub organization settings, enforce MFA
   - Select acceptable second factors:
     - TOTP applications
     - Security keys (FIDO U2F)
     - Mobile device verification

2. **Integrate with SAP Identity Authentication**:
   - Configure multi-factor authentication in SAP IAS
   - Set risk-based authentication rules
   - Enable step-up authentication for sensitive operations

### MFA for SAP Systems

1. **Configure SAP IAS MFA**:
   - In SAP IAS admin console, enable MFA
   - Configure authentication methods:
     - TOTP (Time-based One-Time Password)
     - Email OTP
     - SMS OTP
   - Set MFA policies based on risk level

2. **Integrate SAP GUI Authentication**:
   - Use SAP Single Sign-On with SAML
   - Configure step-up authentication for critical transactions
   - Implement session management controls

## Cross-System Authentication

### OAuth 2.0 Flow

Implement OAuth 2.0 for secure API communications between SAP and GitHub:

1. **Configure OAuth Clients**:
   - Register OAuth client in GitHub
   - Configure OAuth client in SAP API Management
   - Set appropriate scopes and redirect URIs

2. **Implement Authorization Code Flow**:
   ```java
   // Example OAuth 2.0 flow implementation
   public class OAuth2Client {
       private final String clientId;
       private final String clientSecret;
       private final String redirectUri;
       
       public String getAuthorizationUrl(String state) {
           return "https://github.com/login/oauth/authorize" +
                  "?client_id=" + clientId +
                  "&redirect_uri=" + URLEncoder.encode(redirectUri, StandardCharsets.UTF_8) +
                  "&state=" + state +
                  "&scope=repo,read:org";
       }
       
       public TokenResponse exchangeCodeForToken(String code) {
           // Implementation for token exchange
           // ...
       }
   }
   ```

### JWT-Based Authentication

For service-to-service authentication without user context:

1. **Generate JWT Assertion**:
   - Sign JWT with private key
   - Include appropriate claims for authorization
   - Set short expiration time (5-15 minutes)

2. **Validate JWT on Receiver**:
   - Verify signature using public key
   - Check expiration time
   - Validate claims and issuer

```java
// Example JWT validation
public class JWTValidator {
    private final RSAPublicKey publicKey;
    
    public DecodedJWT validateToken(String token) throws JWTVerificationException {
        Algorithm algorithm = Algorithm.RSA256(publicKey, null);
        JWTVerifier verifier = JWT.require(algorithm)
            .withIssuer("sap-github-integration")
            .build();
            
        return verifier.verify(token);
    }
}
```

## Audit and Compliance

### Authentication Events Logging

1. **Configure Authentication Logging**:
   - Enable detailed logging for authentication events
   - Capture the following data points:
     - User ID
     - Source IP address
     - Authentication method
     - Target system
     - Timestamp
     - Success/failure

2. **Centralize Logs**:
   - Forward authentication logs to a central SIEM
   - Configure correlation rules
   - Set up retention policies

### Compliance Reporting

1. **Authentication Reports**:
   - Generate regular reports on authentication activities
   - Track failed authentication attempts
   - Monitor MFA adoption and compliance

2. **Access Reviews**:
   - Schedule periodic access reviews
   - Document approval workflows
   - Maintain audit trail for compliance

## Troubleshooting

| Issue | Symptoms | Solution |
|-------|----------|----------|
| GitHub App token generation failure | 401 Unauthorized responses | Check private key, app ID, and installation ID |
| JWT signature verification error | Authentication failures with signature errors | Verify key pair matches, check clock skew |
| SSO misconfiguration | Redirect loops or "invalid_request" errors | Verify SAML metadata and certificate configuration |
| User mapping issues | Users can authenticate but have incorrect permissions | Check attribute mapping configuration |
| MFA enrollment problems | Users unable to complete MFA setup | Provide alternative enrollment methods, temporary bypass |

## Best Practices

1. **Private Key Security**:
   - Store GitHub App private keys in a secure vault
   - Implement key rotation procedures
   - Avoid storing keys in code repositories

2. **Token Handling**:
   - Never log full tokens
   - Implement token storage encryption
   - Clear tokens from memory when not needed

3. **SSO Configuration**:
   - Test SSO thoroughly before enforcing
   - Maintain alternative access methods for administrators
   - Implement monitoring for SSO failures

4. **User Provisioning**:
   - Implement consistent offboarding workflows
   - Audit user accounts regularly
   - Use least privilege principle

5. **MFA Implementation**:
   - Provide multiple MFA options
   - Create recovery procedures
   - Train users on MFA importance

## References

- [GitHub Apps Documentation](https://docs.github.com/en/developers/apps/building-github-apps)
- [SAP Cloud Identity Authentication Documentation](https://help.sap.com/docs/IDENTITY_AUTHENTICATION)
- [OAuth 2.0 Specification](https://tools.ietf.org/html/rfc6749)
- [JWT Specification](https://tools.ietf.org/html/rfc7519)
- [SAML 2.0 Specification](http://docs.oasis-open.org/security/saml/v2.0/saml-core-2.0-os.pdf)

---

*Last Updated: 2024-05-15*
