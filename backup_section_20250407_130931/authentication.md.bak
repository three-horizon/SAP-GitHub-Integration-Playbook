# GitHub Enterprise Authentication Setup

This guide explains how to configure authentication between SAP systems and GitHub Enterprise, which is a critical component of the SAP-GitHub integration.

## Overview

Proper authentication between SAP and GitHub Enterprise is essential for:
- Secure code exchange between systems
- Automated workflows that integrate with SAP transport management
- User identity mapping between SAP and GitHub
- Compliance with security policies and audit requirements

## Authentication Methods

Three primary authentication methods are supported:

1. **Personal Access Tokens (PATs)** - Simplest approach, suitable for initial setup and testing
2. **OAuth Apps** - Preferred for production environments with user-specific permissions
3. **GitHub Apps** - Most powerful option for enterprise-wide integration

## Method 1: Personal Access Token (PAT) Authentication

### Configuration in GitHub

1. Log in to GitHub Enterprise with administrative privileges
2. Navigate to your profile settings by clicking your profile icon → **Settings**
3. Select **Developer Settings** → **Personal Access Tokens** → **Tokens (classic)**
4. Click **Generate new token** → **Generate new token (classic)**
5. Configure token settings:
   - Note: "SAP Integration Token"
   - Expiration: Select appropriate duration (90 days recommended)
   - Select scopes:
     - `repo` (Full control)
     - `workflow` (if using GitHub Actions)
     - `read:org` (if organization access is needed)
6. Click **Generate token**
7. **Important**: Copy the token immediately; it won't be shown again

### Configuration in SAP

1. Log in to your SAP system
2. Execute transaction `/GOOG/ZGIT_ADMIN` (or your custom admin transaction)
3. Navigate to **Configuration** → **Authentication**
4. Select **PAT Authentication**
5. Enter the GitHub Personal Access Token
6. Specify the GitHub Enterprise URL
7. Test the connection
8. Save the configuration

## Method 2: OAuth App Authentication

### Creating an OAuth App in GitHub

1. Log in to GitHub Enterprise as an organization admin
2. Navigate to your organization settings
3. Select **Developer Settings** → **OAuth Apps**
4. Click **New OAuth App**
5. Fill in the application details:
   - Application name: "SAP Integration"
   - Homepage URL: Your internal documentation URL or SAP portal URL
   - Authorization callback URL: URL to your SAP system's OAuth handler endpoint (typically `https://<your-sap-host>:<port>/sap/bc/bsp/sap/zgit_oauth_cb`)
   - Description: "OAuth integration between SAP and GitHub Enterprise"
6. Click **Register application**
7. Note the **Client ID** and **Client Secret** for later use

### Configuration in SAP

1. Log in to your SAP system
2. Execute transaction `/GOOG/ZGIT_ADMIN` (or your custom transaction)
3. Navigate to **Configuration** → **Authentication**
4. Select **OAuth Authentication**
5. Enter:
   - GitHub Enterprise URL
   - Client ID
   - Client Secret
   - Callback URL (must match the one configured in GitHub)
6. Save the configuration
7. Execute transaction **SICF**
8. Activate the service path `/sap/bc/bsp/sap/zgit_oauth_cb` (create if not exists)
9. Configure SSL settings if needed

## Method 3: GitHub App Authentication

### Creating a GitHub App

1. Log in to GitHub Enterprise as an organization admin
2. Navigate to your organization settings
3. Select **Developer Settings** → **GitHub Apps**
4. Click **New GitHub App**
5. Configure the application:
   - GitHub App name: "SAP Integration"
   - Description: "Integration between SAP systems and GitHub Enterprise"
   - Homepage URL: Your internal documentation or SAP portal URL
   - Webhook URL: URL to your SAP webhook handler (e.g., `https://<sap-host>:<port>/sap/bc/bsp/sap/zgit_webhook`)
   - Webhook secret: Generate a secure random string
   - Permissions:
     - Repository permissions:
       - Contents: Read & write
       - Metadata: Read-only
       - Pull requests: Read & write
       - Workflows: Read & write
     - Organization permissions:
       - Members: Read-only
6. Where can this GitHub App be installed? Select "Only on this account"
7. Click **Create GitHub App**
8. Note the **App ID** displayed
9. Under "Private keys", click **Generate a private key**
10. Download the private key file (.pem)
11. Install the app in your organization by clicking **Install App**

### Configuration in SAP

1. Log in to your SAP system
2. Execute transaction `/GOOG/ZGIT_ADMIN` (or your custom transaction)
3. Navigate to **Configuration** → **Authentication**
4. Select **GitHub App Authentication**
5. Enter:
   - GitHub Enterprise URL
   - App ID
   - Upload the private key file (.pem)
   - Installation ID (available after installing the app)
   - Webhook secret (if using webhooks)
6. Save the configuration
7. Execute transaction **SICF**
8. Activate the service path for webhook handler
9. Configure SSL settings if needed

## User Mapping

To map SAP users to GitHub users:

1. Execute transaction `/GOOG/ZGIT_ADMIN`
2. Navigate to **User Management** → **User Mapping**
3. Create mappings between SAP user IDs and GitHub usernames
4. Alternatively, configure automatic mapping:
   - Based on email address patterns
   - Through Active Directory/LDAP integration
   - Via custom mapping logic

## Service User Configuration

For background processing and automated tasks:

1. Create a dedicated SAP service user (e.g., `ZGITHUB`)
2. Assign necessary authorizations:
   - S_DEVELOP for development objects
   - S_TRANSPRT for transport management
   - Custom authorizations for GitHub integration
3. Configure this user in the integration settings
4. Create a dedicated PAT or OAuth token for this user

## Securing Authentication Information

To secure authentication credentials:

1. Store secrets in SAP Secure Store (SSF):
   - Execute transaction **SECSTORE**
   - Create a new secure store entry
   - Store tokens and secrets
2. Configure encryption:
   - Execute transaction **SSFA**
   - Configure encryption settings
3. Regularly rotate credentials:
   - Implement a process for regular token rotation
   - Document in your security procedures

## Multi-System Authentication

For multiple SAP systems connecting to GitHub:

1. Create system-specific credentials:
   - Use naming convention including system ID
   - Configure appropriate access restrictions
2. Document credential management:
   - Which systems use which credentials
   - Renewal procedures
   - Emergency access process

## Testing Authentication

To verify your configuration:

1. Execute transaction `/GOOG/ZGIT_TEST` (or your custom test transaction)
2. Select **Test Authentication**
3. The system will:
   - Attempt to connect to GitHub Enterprise
   - Validate credentials
   - Test specific API operations
4. Review the test log for any errors

## Troubleshooting

### Common Issues

| Issue | Resolution |
|-------|------------|
| Connection timeout | Check network connectivity and firewall settings |
| Authentication failed | Verify token has not expired and has correct scopes |
| Certificate errors | Configure SSL trust settings in STRUST transaction |
| Permission denied | Check that token has appropriate repository permissions |
| User mapping errors | Verify user mapping configuration is correct |

### Logging and Debugging

For authentication issues:

1. Execute transaction **SLG1**
2. Select application log object `ZGITHUB`
3. Review authentication-related log entries
4. For additional debugging, enable verbose logging in admin configuration

## Next Steps

Once authentication is configured, proceed to:

- [Repository Structure Setup](repository-structure.md)
- [Branch Protection Configuration](branch-protection.md)
- [Workflow Configuration](../workflows/ci-cd-setup.md)

## Additional Resources

- [GitHub Authentication Documentation](https://docs.github.com/en/authentication)
- [OAuth 2.0 Specification](https://oauth.net/2/)
- [SAP Security Best Practices](../../security/best-practices.md)

---

*Last Updated: [YYYY-MM-DD] for [GitHub Enterprise X.Y.Z]* 