# 📄 GitHub Enterprise Authentication Configuration

## 📋 Table of Contents

- [📋 Overview](#overview)
- [✅ Prerequisites](#prerequisites)
- [Detailed Content](#detailed-content)
- [🔧 Implementation Plan](#implementation-plan)
- [Best Practices](#best-practices)
- [Troubleshooting](#troubleshooting)
- [➡
️ ️ Next Steps](#next-steps)
- [References](#references)


## 📋 Overview

This document provides detailed instructions for configuring secure authentication methods for GitHub Enterprise in the context of SAP-GitHub integration. Proper authentication configuration is critical for maintaining security across your development pipeline while ensuring frictionless developer experience.

## ✅ Prerequisites

- GitHub Enterprise Cloud or Server license
- Administrative access to GitHub Enterprise
- Identity provider details (for SSO configuration)
- Network infrastructure that allows necessary connections
- Security team approval for chosen authentication methods
- Certificate management process (for certificate-based authentication)

## Detailed Content

### Authentication Strategy Selection

Choose the appropriate authentication strategy based on your organization's security requirements and existing infrastructure:

1. **Enterprise-level Authentication Strategies**:
   - Single Sign-On (SSO) with SAML
   - Enterprise Managed Users with SCIM
   - Certificate-based authentication
   - OAuth 2.0 app-based integration
   - Personal Access Tokens with IP restrictions

2. **Selection Criteria**:
   - Security requirements (compliance, regulations)
   - Integration with existing identity management systems
   - User experience considerations
   - Automation requirements
   - Audit and monitoring capabilities

3. **Authentication Flow Design**:
   - Developer authentication flows
   - Service account authentication
   - CI/CD system authentication
   - Cross-system integration authentication
   - Emergency access procedures

### SAML Single Sign-On Configuration

#### Identity Provider Integration

1. **Azure Active Directory Configuration**:
   - Navigate to Azure AD Enterprise Applications
   - Add GitHub Enterprise as a gallery application
   - Configure SAML settings in Azure:
     - Identifier (Entity ID): `https://github.com/enterprises/{enterprise-name}`
     - Reply URL: `https://github.com/enterprises/{enterprise-name}/saml/consume`
     - Sign-on URL: `https://github.com/enterprises/{enterprise-name}/sso`
   - Download Federation Metadata XML file
   - Assign users and groups to the application

2. **Okta Configuration**:
   - Add GitHub Enterprise application from Okta catalog
   - Configure SAML 2.0 settings:
     - Single Sign-On URL: `https://github.com/enterprises/{enterprise-name}/saml/consume`
     - Audience URI: `https://github.com/enterprises/{enterprise-name}`
     - Default RelayState: `https://github.com/orgs/{org-name}/sso`
   - Download IdP metadata
   - Assign application to relevant Okta groups

3. **ADFS Configuration**:
   - Add Relying Party Trust in ADFS management console
   - Configure claim rules:
     - Issue NameID as user's email address
     - Issue DisplayName attribute
     - Issue Email attribute
   - Export Federation Metadata XML
   - Configure endpoints and authentication policies

#### GitHub Enterprise Configuration

1. **SAML Configuration Steps**:
   - Navigate to GitHub Enterprise settings
   - Select "Security" > "Authentication security"
   - Click "Enable SAML authentication"
   - Upload identity provider metadata
   - Configure additional settings:
     - Sign-on URL matching
     - Username format
     - Verification certificate

2. **SAML Testing and Verification**:
   - Test SSO connection from GitHub
   - Verify login with test users
   - Troubleshoot common issues:
     - Certificate validation failures
     - Attribute mapping problems
     - Session timeout configuration

3. **Recovery Access Configuration**:
   - Configure recovery codes
   - Set up emergency administrator access
   - Document emergency access procedures

### SCIM User Provisioning

#### Identity Provider Configuration

1. **Azure AD SCIM Setup**:
   - Navigate to Enterprise Application Provisioning
   - Configure provisioning credentials:
     - Tenant URL: `https://api.github.com/scim/v2/enterprises/{enterprise-name}`
     - Secret Token: GitHub SCIM token
   - Configure attribute mappings:
     - userName → userName
     - displayName → displayName
     - active → active
     - emails[type eq "work"].value → emails
   - Start provisioning and monitor synchronization

2. **Okta SCIM Configuration**:
   - Configure provisioning in Okta admin console
   - Add SCIM connection details:
     - SCIM connector base URL: `https://api.github.com/scim/v2/enterprises/{enterprise-name}`
     - OAuth Bearer Token: GitHub SCIM token
   - Configure attribute mappings
   - Enable provisioning features:
     - Create users
     - Update attributes
     - Deactivate users

#### GitHub Enterprise Configuration

1. **SCIM Token Generation**:
   - Navigate to GitHub Enterprise settings
   - Select "Security" > "Authentication security"
   - Enable SCIM user provisioning
   - Generate and securely store SCIM token

2. **User Provisioning Rules**:
   - Configure team synchronization
   - Map identity provider groups to GitHub teams
   - Set default repository access permissions
   - Configure organization membership provisioning

3. **Audit and Monitoring**:
   - Monitor provisioning logs in identity provider
   - Review GitHub audit logs for provisioning events
   - Set up alerts for provisioning failures

### Certificate-Based Authentication

#### Certificate Authority Setup

1. **Internal PKI Configuration**:
   - Set up or identify existing certificate authority
   - Configure certificate templates for:
     - User authentication
     - Service account authentication
     - System integration
   - Establish certificate lifecycle management

2. **Certificate Distribution**:
   - Configure automated certificate distribution
   - Set up secure storage for certificates
   - Document certificate renewal procedures

#### GitHub Configuration

1. **SSH Certificate Authority**:
   - Add CA public key to GitHub organization:
     ```bash
     # Upload CA public key to GitHub
     gh api --method PUT /orgs/{org}/ssh_signing_keys \
       -f key="$(cat ca-public-key.pub)" \
       -f title="Corporate SSH CA"
     ```

2. **Client Certificate Setup**:
   - Generate user certificates signed by CA
   - Configure SSH config for certificate usage:
     ```
     # ~/.ssh/config
     Host github.com
       IdentityFile ~/.ssh/id_ed25519
       CertificateFile ~/.ssh/id_ed25519-cert.pub
     ```

3. **Certificate Rotation**:
   - Implement automated certificate rotation
   - Configure expiration notifications
   - Document emergency certificate revocation process

### OAuth Application Configuration

#### GitHub OAuth App Setup

1. **Creating OAuth Application**:
   - Navigate to GitHub organization settings
   - Select "Developer settings" > "OAuth Apps"
   - Click "New OAuth App"
   - Configure application:
     - Application name: "SAP-GitHub Integration"
     - Homepage URL: `https://example.com/sap-github-integration`
     - Authorization callback URL: `https://api-gateway.example.com/oauth/callback`
     - Enable device flow (if needed)

2. **Application Secret Management**:
   - Generate client secret
   - Store credentials in secure key vault
   - Implement secret rotation procedure
   - Set up monitoring for OAuth token usage

3. **Scope Configuration**:
   - Configure minimal required scopes:
     - `repo` (for repository access)
     - `workflow` (for GitHub Actions)
     - `admin:org` (for organization management)
     - `admin:repo_hook` (for webhook management)

#### Integration Configuration

1. **API Management Integration**:
   - Configure OAuth in API Management:
     - Set up OAuth authorization endpoints
     - Implement token validation
     - Configure scope verification
     - Set up token caching policies
   - Example API Management policy:
     ```xml
     <policies>
       <inbound>
         <validate-jwt header-name="Authorization" require-scheme="Bearer">
           <openid-config url="https://github.com/orgs/{org-name}/.well-known/oauth-authorization-server" />
           <required-claims>
             <claim name="scope" match="all">
               <value>repo</value>
             </claim>
           </required-claims>
         </validate-jwt>
       </inbound>
     </policies>
     ```

2. **Application Integration**:
   - Implement OAuth flow in integration applications
   - Configure token refresh logic
   - Implement token storage and retrieval
   - Set up error handling for authentication failures

### Personal Access Token Management

#### Token Generation Strategy

1. **Token Types and Scopes**:
   - Fine-grained personal access tokens
   - Classic personal access tokens
   - Scoping strategy based on:
     - Repository access requirements
     - Organization-level permissions
     - API access needs

2. **Token Creation Process**:
   - Generate tokens with minimal necessary permissions
   - Set appropriate expiration periods
   - Document token purpose and owner
   - Example token creation via API:
     ```bash
     # Create fine-grained PAT via API
     gh api --method POST /repos/{owner}/{repo}/personal-access-tokens \
       -f name="SAP Integration Token" \
       -f repository_id="12345678" \
       -f permissions[contents]="read" \
       -f permissions[actions]="read"
     ```

3. **Service Account Tokens**:
   - Create dedicated service accounts
   - Generate tokens for automated processes
   - Implement strict IP restrictions
   - Configure SSO enforcement

#### Token Security Controls

1. **Token Storage**:
   - Store tokens in secure key vault
   - Implement encryption for token values
   - Configure access control for token retrieval
   - Example Azure Key Vault storage:
     ```powershell
     # Store GitHub token in Azure Key Vault
     Set-AzKeyVaultSecret -VaultName "sap-integration-kv" `
       -Name "github-api-token" `
       -SecretValue (ConvertTo-SecureString -String "ghp_abcdef123456" -AsPlainText -Force) `
       -ContentType "text/plain" `
       -Tag @{Purpose="SAP Integration"; Owner="sap-admin"; Expires="2024-01-01"}
     ```

2. **Token Rotation**:
   - Implement automatic token rotation
   - Configure rotation schedule based on risk
   - Verify integration functionality after rotation
   - Document emergency token revocation procedures

3. **Token Monitoring**:
   - Monitor token usage via audit logs
   - Set up alerts for unusual token activity
   - Track token expirations
   - Implement automated token health checks

### Security Hardening

#### IP Allowlisting

1. **IP Restriction Configuration**:
   - Navigate to GitHub Enterprise security settings
   - Configure IP allow lists:
     - Development network ranges
     - CI/CD system IP addresses
     - API Management IP addresses
   - Example configuration:
     ```
     # Corporate office network
     192.168.1.0/24
     
     # Remote developer VPN ranges
     10.0.0.0/16
     
     # Cloud infrastructure (Azure)
     20.0.0.0/16
     ```

2. **Enforcement Configuration**:
   - Configure enforcement for:
     - Organization access
     - Repository access
     - API access
     - GitHub Actions workflows

3. **Exception Handling**:
   - Document exception process for emergencies
   - Configure notification for blocked access attempts
   - Create temporary access procedure

#### Two-Factor Authentication

1. **2FA Enforcement**:
   - Enable organization-wide 2FA requirement
   - Configure enforcement period
   - Prepare user communication
   - Monitor 2FA adoption

2. **Recovery Options**:
   - Configure recovery codes
   - Document backup device procedures
   - Set up emergency access process
   - Test account recovery workflow

3. **Hardware Security Key Integration**:
   - Configure WebAuthn/FIDO2 support
   - Distribute hardware security keys to admins
   - Document security key enrollment process
   - Implement backup authentication methods

### Audit and Compliance

#### Authentication Monitoring

1. **Audit Log Configuration**:
   - Enable detailed authentication logging
   - Stream authentication events to SIEM system
   - Configure retention period
   - Example log collection:
     ```bash
     # Collect GitHub audit log entries for authentication events
     gh api --paginate /enterprises/{enterprise}/audit-log?phrase=action:user.login
     ```

2. **Authentication Analytics**:
   - Monitor login patterns
   - Track failed authentication attempts
   - Analyze geographic access patterns
   - Set up anomaly detection

3. **Compliance Reporting**:
   - Generate authentication compliance reports
   - Document authentication controls
   - Prepare audit evidence collection
   - Map controls to compliance frameworks

#### Access Reviews

1. **Periodic Access Review**:
   - Schedule quarterly access reviews
   - Verify authentication method compliance
   - Review service account authentication
   - Document review results

2. **Remediation Process**:
   - Define process for non-compliant authentication
   - Implement automated remediation where possible
   - Track remediation status
   - Verify remediation effectiveness

## 🔧 Implementation Plan

Follow this phased approach to implement GitHub Enterprise authentication:

1. **Phase 1: Assessment (1-2 weeks)**
   - Audit existing authentication methods
   - Identify security requirements
   - Select appropriate authentication strategies
   - Design authentication flows

2. **Phase 2: Basic Authentication (2-3 weeks)**
   - Configure username/password authentication
   - Implement 2FA requirement
   - Set up IP restrictions
   - Configure basic audit logging

3. **Phase 3: SSO Implementation (3-4 weeks)**
   - Configure identity provider
   - Set up SAML integration
   - Implement SCIM provisioning
   - Test and troubleshoot SSO flows

4. **Phase 4: Advanced Authentication (2-3 weeks)**
   - Implement certificate-based authentication
   - Configure OAuth integrations
   - Set up service account authentication
   - Enhance monitoring and alerting

## Best Practices

1. **Security Principles**
   - Implement defense in depth
   - Follow least privilege principle
   - Separate duties for critical operations
   - Maintain authentication boundary control

2. **Operational Excellence**
   - Document all authentication configurations
   - Train administrators on management procedures
   - Implement change management for auth changes
   - Regularly test authentication flows

3. **User Experience Considerations**
   - Balance security with usability
   - Provide clear user documentation
   - Implement gradual enforcement of changes
   - Collect user feedback on authentication experience

4. **Integration Security**
   - Secure authentication for system integrations
   - Implement proper credential management
   - Monitor integration authentication
   - Regularly rotate integration credentials

## Troubleshooting

### Common Issues and Solutions

- **SSO Configuration Problems**:
  - Verify metadata configuration
  - Check certificate validity
  - Ensure attribute mapping is correct
  - Test with simplified configuration first

- **Token Authentication Failures**:
  - Verify token has correct scopes
  - Check token expiration
  - Confirm IP address is allowed
  - Review rate limiting status

- **2FA Issues**:
  - Verify time synchronization on devices
  - Check recovery codes availability
  - Ensure backup device is configured
  - Contact GitHub support for account recovery

## ➡️ Next Steps

After configuring GitHub Enterprise authentication, proceed to:

- [GitHub Organization Setup](./organization-setup.md) - Configure organization structure
- [Repository Security](./repository-security.md) - Implement repository-level security
- [GitHub Actions Security](./actions-security.md) - Secure CI/CD workflows

## References

- [GitHub Enterprise Security Documentation](https://docs.github.com/en/enterprise-cloud@latest/admin/authentication/managing-identity-and-access-for-your-enterprise)
- [SAML SSO Protocol Specification](https://docs.oasis-open.org/security/saml/v2.0/saml-core-2.0-os.pdf)
- [OAuth 2.0 Framework](https://oauth.net/2/)
- [NIST Authentication Guidelines](https://pages.nist.gov/800-63-3/sp800-63b.html)

---




