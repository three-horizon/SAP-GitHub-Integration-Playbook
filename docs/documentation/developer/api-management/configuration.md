# 📄 API Management Setup for SAP-GitHub Integration

## 📋 Table of Contents

- [Overview](#overview)
- [Prerequisites](#prerequisites)
- [Implementation Checklist](#implementation-checklist)
- [Step-by-Step Implementation](#step-by-step-implementation)
  - [Create API Management Service](#create-api-management-service)
  - [Configure API Gateways](#configure-api-gateways)
  - [Set Up Security Policies](#set-up-security-policies)
  - [Create SAP APIs](#create-sap-apis)
  - [Create GitHub APIs](#create-github-apis)
  - [Configure Backend Services](#configure-backend-services)
  - [Implement Authentication](#implement-authentication)
  - [Set Up Logging and Monitoring](#set-up-logging-and-monitoring)
- [API Endpoints Reference](#api-endpoints-reference)
- [Testing and Validation](#testing-and-validation)
- [Troubleshooting](#troubleshooting)
- [Next Steps](#next-steps)

## 📋 Overview

Microsoft API Management serves as the crucial middleware layer in the SAP-GitHub integration architecture. It provides secure, managed API endpoints that facilitate communication between SAP systems and GitHub, handling authentication, request transformation, and security policies.

This guide walks through the setup and configuration of API Management for the SAP-GitHub integration.

## ✅ Prerequisites

Before setting up API Management, ensure you have:

- Azure subscription with contributor access
- SAP system endpoints (DEV, QAS, PRD) and credentials
- GitHub Enterprise account and access tokens
- Network connectivity between API Management and both SAP and GitHub
- SSL certificates for secure communication
- Understanding of SAP Gateway services and GitHub APIs
- SAP Notes installed:
  - Note 2655547: OAuth authentication for external interfaces 
  - Note 2573788: Security for external integrations

For complete environment requirements, see the [Prerequisites](../getting-started/prerequisites.md) document.

## 🔧 Implementation Checklist

- [ ] Provision API Management service in Azure
- [ ] Configure network settings and connectivity
- [ ] Set up API Management developer portal (optional)
- [ ] Create API Management products
- [ ] Define and implement security policies
- [ ] Create and configure SAP APIs
- [ ] Create and configure GitHub APIs
- [ ] Set up authentication mechanisms
- [ ] Configure request/response transformations
- [ ] Implement logging and monitoring
- [ ] Test all API endpoints
- [ ] Set up alerting

## Step-by-Step Implementation

### Create API Management Service

1. **Provision API Management Service**
   - Log in to the Azure Portal
   - Navigate to "Create a resource" > "Integration" > "API Management"
   - Fill in the details:
     - Name: `sap-github-integration-apim`
     - Resource Group: `rg-sap-github-integration`
     - Location: [Select nearest region]
     - Organization name: Your organization
     - Administrator email: Your admin email
     - Pricing tier: Standard or Premium (for production)
   - Click "Review + create" and then "Create"

2. **Configure Virtual Network** (recommended for production)
   - Navigate to your API Management service
   - Go to "Virtual network"
   - Select "External" or "Internal" based on your requirements
   - Configure the virtual network settings
   - Click "Apply"

### Configure API Gateways

1. **Create Products**
   - Navigate to "Products" in API Management
   - Create the following products:
     - **SAP-GitHub-Integration**: All APIs for the integration
     - **SAP-APIs**: APIs specific to SAP
     - **GitHub-APIs**: APIs specific to GitHub
   - For each product:
     - Set subscription requirements
     - Define access controls
     - Apply usage quotas if needed

2. **Configure Custom Domains** (Optional)
   - Navigate to "Custom domains"
   - Add and configure custom domain names for your gateway
   - Upload SSL certificates

### Set Up Security Policies

1. **Create Global Policies**
   - Navigate to "APIs" > "All APIs" > "Policy"
   - Implement the following policies:
     ```xml
     <policies>
       <inbound>
         <cors>
           <allowed-origins>
             <origin>https://github.yourdomain.com</origin>
             <origin>https://sap.yourdomain.com</origin>
           </allowed-origins>
           <allowed-methods>
             <method>GET</method>
             <method>POST</method>
             <method>PUT</method>
           </allowed-methods>
           <allowed-headers>
             <header>Content-Type</header>
             <header>Authorization</header>
           </allowed-headers>
         </cors>
         <set-header name="X-Content-Type-Options" exists-action="override">
           <value>nosniff</value>
         </set-header>
         <set-header name="X-XSS-Protection" exists-action="override">
           <value>1; mode=block</value>
         </set-header>
         <rate-limit-by-key calls="5" renewal-period="60" counter-key="@(context.Request.IpAddress)" />
       </inbound>
     </policies>
     ```

2. **Create Security Policies**
   - Navigate to "Security" > "Policies"
   - Configure the following policies:
     - JWT validation
     - OAuth 2.0 / OpenID Connect
     - IP filtering
     - Rate limiting

### Create SAP APIs

1. **Create SAP Transport API**
   - Navigate to "APIs" > "Add API"
   - Select "OpenAPI" or "HTTP"
   - Configure the API:
     - Display name: `SAP Transport API`
     - Name: `sap-transport-api`
     - Web service URL: `https://sap-dev.yourdomain.com/sap/bc/adt/cts/` (for DEV)
   - Define operations:
     - `GET /transports`: List transports
     - `GET /transports/{id}`: Get transport details
     - `POST /transports`: Create transport
     - `POST /transports/{id}/release`: Release transport
   - Apply policies:
     ```xml
     <policies>
       <inbound>
         <base />
         <authentication-basic username="{{sap-tech-user}}" password="{{sap-tech-password}}" />
         <set-header name="SAP-Client" exists-action="override">
           <value>{{sap-client}}</value>
         </set-header>
       </inbound>
     </policies>
     ```

2. **Create SAP abapGit API**
   - Navigate to "APIs" > "Add API"
   - Select "OpenAPI" or "HTTP"
   - Configure the API:
     - Display name: `SAP abapGit API`
     - Name: `sap-abapgit-api`
     - Web service URL: `https://sap-dev.yourdomain.com/sap/bc/adt/abapgit/` (for DEV)
   - Define operations:
     - `GET /repos`: List repositories
     - `GET /repos/{name}`: Get repository details
     - `POST /repos/{name}/pull`: Pull from GitHub
     - `POST /repos/{name}/push`: Push to GitHub
   - Apply similar policies as SAP Transport API

### Create GitHub APIs

1. **Create GitHub Repository API**
   - Navigate to "APIs" > "Add API"
   - Select "OpenAPI" or "HTTP"
   - Configure the API:
     - Display name: `GitHub Repository API`
     - Name: `github-repo-api`
     - Web service URL: `https://api.github.com/repos/`
   - Define operations:
     - `GET /{owner}/{repo}`: Get repository details
     - `GET /{owner}/{repo}/branches`: List branches
     - `GET /{owner}/{repo}/contents`: Get contents
   - Apply policies:
     ```xml
     <policies>
       <inbound>
         <base />
         <set-header name="Authorization" exists-action="override">
           <value>token {{github-token}}</value>
         </set-header>
         <set-header name="Accept" exists-action="override">
           <value>application/vnd.github+json</value>
         </set-header>
       </inbound>
     </policies>
     ```

2. **Create GitHub Webhooks API**
   - Navigate to "APIs" > "Add API"
   - Select "OpenAPI" or "HTTP"
   - Configure the API:
     - Display name: `GitHub Webhooks API`
     - Name: `github-webhooks-api`
     - Web service URL: `https://api.github.com/repos/`
   - Define operations:
     - `POST /{owner}/{repo}/hooks`: Create webhook
     - `GET /{owner}/{repo}/hooks`: List webhooks
     - `DELETE /{owner}/{repo}/hooks/{id}`: Delete webhook
   - Apply similar policies as GitHub Repository API

### Configure Backend Services

1. **Create Named Values**
   - Navigate to "Named values"
   - Create the following named values:
     - `sap-dev-url`: URL for SAP DEV system
     - `sap-qas-url`: URL for SAP QAS system
     - `sap-prd-url`: URL for SAP PRD system
     - `sap-tech-user`: SAP technical user
     - `sap-tech-password`: SAP technical user password (secret)
     - `sap-client`: SAP client
     - `github-token`: GitHub personal access token (secret)

2. **Configure Backend Services**
   - For each SAP system (DEV, QAS, PRD):
     - Configure backend service in each API
     - Set up health probes
     - Configure timeouts and retries

### Implement Authentication

1. **Configure API Authentication**
   - Navigate to "Security" > "Identities"
   - Set up the following authentication methods:
     - API key authentication for external clients
     - JWT validation for secure system-to-system communication
     - OAuth 2.0 for user authentication (if needed)

2. **Set Up Role-Based Access Control**
   - Navigate to "Security" > "Roles"
   - Create the following roles:
     - `SAP-Admin`: Full access to SAP APIs
     - `GitHub-Admin`: Full access to GitHub APIs
     - `Transport-Manager`: Access to transport management
     - `Developer`: Limited access for developers

### Set Up Logging and Monitoring

1. **Configure Diagnostics**
   - Navigate to "Monitoring" > "Diagnostics"
   - Enable logging for:
     - Gateway requests
     - Backend requests
     - Policy evaluation
   - Send logs to:
     - Azure Monitor
     - Application Insights
     - Microsoft Sentinel (for security monitoring)

2. **Set Up Metrics**
   - Navigate to "Monitoring" > "Metrics"
   - Configure dashboards for:
     - API usage
     - Response times
     - Error rates
     - Security events

3. **Configure Alerts**
   - Navigate to "Monitoring" > "Alerts"
   - Set up alerts for:
     - High error rates
     - Slow response times
     - Authentication failures
     - Unusual traffic patterns

## API Endpoints Reference

This section lists all the API endpoints needed for the SAP-GitHub integration:

### SAP APIs

| API | Method | Endpoint | Description |
|-----|--------|----------|-------------|
| Transport | GET | `/transport/list` | List transport requests |
| Transport | GET | `/transport/{id}` | Get transport details |
| Transport | POST | `/transport/create` | Create transport request |
| Transport | POST | `/transport/{id}/release` | Release transport |
| abapGit | GET | `/abapgit/repos` | List repositories |
| abapGit | GET | `/abapgit/repos/{name}` | Get repository details |
| abapGit | POST | `/abapgit/repos/{name}/pull` | Pull from GitHub |
| abapGit | POST | `/abapgit/repos/{name}/push` | Push to GitHub |
| abapGit | POST | `/abapgit/repos/{name}/link` | Link repository |
| abapGit | POST | `/abapgit/repos/{name}/unlink` | Unlink repository |

### GitHub APIs

| API | Method | Endpoint | Description |
|-----|--------|----------|-------------|
| Repository | GET | `/repos/{owner}/{repo}` | Get repository details |
| Repository | GET | `/repos/{owner}/{repo}/branches` | List branches |
| Repository | GET | `/repos/{owner}/{repo}/contents/{path}` | Get file contents |
| Repository | POST | `/repos/{owner}/{repo}/dispatches` | Create repository dispatch |
| Webhooks | POST | `/repos/{owner}/{repo}/hooks` | Create webhook |
| Webhooks | GET | `/repos/{owner}/{repo}/hooks` | List webhooks |
| Webhooks | DELETE | `/repos/{owner}/{repo}/hooks/{id}` | Delete webhook |

## Testing and Validation

After setting up API Management, thoroughly test all endpoints:

1. **Test SAP Connectivity**
   - Test connections to all SAP systems
   - Verify authentication works
   - Check data transformation

2. **Test GitHub Connectivity**
   - Verify GitHub API access
   - Test webhook functionality
   - Validate token authentication

3. **End-to-End Testing**
   - Test complete workflows:
     - Code synchronization from SAP to GitHub
     - Pull request handling
     - Deployment back to SAP
     - Transport management

## Troubleshooting

Common issues and solutions:

1. **Authentication Failures**
   - Verify credentials in named values
   - Check API policy configurations
   - Ensure proper headers are set

2. **Connection Issues**
   - Verify network connectivity
   - Check firewall and NSG settings
   - Validate SSL certificates

3. **Policy Execution Failures**
   - Review policy configurations
   - Check for syntax errors
   - Use trace functionality to diagnose issues

## ➡️ Next Steps

After successful API Management configuration:

1. Set up [GitHub Actions Workflows](../github/actions-workflow-setup.md) for CI/CD automation
2. Ensure [abapGit](../implementation/abapgit-setup.md) is properly configured in your SAP environment
3. Review the overall [Implementation Plan](../getting-started/implementation-plan.md) for next phases

---

**Last Updated:** 2023-09-01 
---


