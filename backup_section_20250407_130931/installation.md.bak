# Installation Guide

This guide provides detailed steps to implement the SAP-GitHub integration with Microsoft security solutions. Before starting, ensure all [prerequisites](./prerequisites.md) are met.

## Implementation Timeline

| Phase | Estimated Duration | Description |
|-------|-------------------|-------------|
| Planning and Preparation | 1-2 weeks | Requirements gathering, system assessment, architecture validation |
| Infrastructure Setup | 1-2 weeks | Azure services deployment, GitHub Enterprise setup |
| SAP Configuration | 1-2 weeks | abapGit installation, system preparation |
| Integration Development | 2-4 weeks | API development, workflow configuration |
| Testing | 2-3 weeks | Integration testing, security validation |
| Deployment | 1 week | Production rollout |
| Post-Implementation | Ongoing | Monitoring, maintenance, optimization |

## 1. Azure Environment Setup

### 1.1 Azure API Management Deployment

1. In the Azure Portal, navigate to **Create a resource** > **Integration** > **API Management**.
2. Provide required details:
   - **Name**: `sap-github-apim`
   - **Subscription**: Select your subscription
   - **Resource Group**: Create or select existing (e.g., `rg-sap-github-integration`)
   - **Location**: Select region close to your SAP systems
   - **Organization name**: Your organization name
   - **Administrator email**: Admin contact email
   - **Pricing tier**: Standard or Premium (required for virtual network integration)

3. Deploy the service (may take 30-45 minutes).

4. Once deployed, configure network settings:
   - For on-premises SAP systems: Configure VNet integration
   - For cloud-based SAP systems: Configure appropriate network access

### 1.2 Microsoft Sentinel Setup

1. In the Azure Portal, navigate to **Create a resource** > **Security** > **Microsoft Sentinel**.
2. Create or select an existing Log Analytics workspace.
3. Add Sentinel to the workspace.
4. Configure data connectors:
   - Azure API Management
   - GitHub Enterprise (through API or REST connector)
   - Azure Activity
   - Azure Diagnostic Logs (for SAP on Azure)

### 1.3 Microsoft Defender for Cloud Configuration

1. In the Azure Portal, navigate to **Microsoft Defender for Cloud**.
2. Enable enhanced security features:
   - Defender for Servers (for SAP systems)
   - Defender for APIs (for APIM)
3. Configure security policies using example provided in [sample policy](../../examples/security/defender/sample-policy.json).

## 2. GitHub Enterprise Configuration

### 2.1 Organization and Repository Setup

1. Access your GitHub Enterprise instance.
2. Create an organization for SAP development (if not existing).
3. Create repositories following this structure:
   - One main repository for integration framework code
   - Separate repositories for major SAP packages/applications

Example repository creation:

```bash
# Using GitHub CLI
gh repo create my-org/sap-github-integration --private --template USER/sap-github-template
gh repo create my-org/sap-app-module1 --private
gh repo create my-org/sap-app-module2 --private
```

### 2.2 Branch Protection Rules

For each repository, configure branch protection rules:

1. Go to **Repository Settings** > **Branches** > **Branch protection rules** > **Add rule**.
2. Configure:
   - **Branch name pattern**: `main`, `qas`, `dev` (create separate rules for each)
   - **Require pull request reviews before merging**: Enabled
   - **Required approvals**: 1-2 (based on your governance requirements)
   - **Require status checks to pass before merging**: Enabled
   - **Require branches to be up to date**: Enabled
   - **Include administrators**: Based on your governance requirements
   - **Allow force pushes**: Disabled
   - **Allow deletions**: Disabled

### 2.3 GitHub Advanced Security Setup

1. Go to **Repository Settings** > **Security & analysis**.
2. Enable:
   - **Dependency graph**
   - **Dependabot alerts**
   - **Dependabot security updates**
   - **Code scanning**
   - **Secret scanning**

3. Configure custom CodeQL query for ABAP (place in `.github/codeql/abap-queries` directory).

### 2.4 GitHub Copilot Configuration

1. Assign GitHub Copilot seats to your development team.
2. Create a `.copilot/` directory with ABAP-specific configuration.
3. Add ABAP patterns to improve suggestions.

## 3. SAP System Configuration

### 3.1 abapGit Installation and Setup

1. Install the latest abapGit version in all SAP systems (DEV, QAS, PRD):
   - Open transaction `SE38` or `SE80`
   - Create a new report called `ZABAPGIT_STANDALONE`
   - Copy the contents from [abapGit](https://raw.githubusercontent.com/abapGit/build/main/zabapgit.abap)
   - Activate the report
   - Execute the report using transaction `SE38`

2. Install the developer version for better integration:
   - In abapGit standalone, select "Install abapGit repository version"
   - Create package `$ABAPGIT` (or as defined in your naming standards)
   - Create online repository pointing to `https://github.com/abapGit/abapGit.git`
   - Pull the repository

3. Configure abapGit settings using the sample configuration as a template:
   - See [sample configuration](../../examples/abapgit-config/sample-config.xml)
   - Adjust settings for your environment

### 3.2 SAP Transport Management Configuration

1. Create a dedicated transport layer for GitHub integration:
   - Execute transaction `STMS`
   - Go to **Overview** > **System Configuration** > **Transport Layer**
   - Define new transport layer (e.g., `ZGIT`)
   - Assign to development class

2. Define transport routes:
   - Configure routes from DEV → QAS → PRD
   - Ensure proper configuration for automated transport release

### 3.3 API and ICF Service Setup

1. Create ICF service for API integration:
   - Execute transaction `SICF`
   - Create new service under `/default_host/sap/bc/api/github_integration`
   - Assign handler class developed based on [sample API](../../examples/api-implementations/sample-api.abap)
   - Activate service

2. Configure security:
   - Execute transaction `SMICM`
   - Set appropriate security settings
   - Enable HTTPS for all communications

## 4. Integration Configuration

### 4.1 API Management Policy Configuration

1. Import the [sample policy XML](../../examples/apim-configs/sample-policy.xml) to Azure APIM.
2. Customize the policy with:
   - Your SAP system URLs
   - Authentication details (using Azure Key Vault for secrets)
   - IP ranges specific to your environment

3. Create API definitions for:
   - abapGit synchronization
   - Transport management
   - Status reporting

### 4.2 GitHub Actions Workflow Setup

1. Add the [sample workflow](../../examples/github-actions/sample-workflow.yml) to your repositories.
2. Customize variables, secrets, and endpoints.
3. Configure GitHub Secrets:
   - `SAP_API_ENDPOINT`: Your APIM endpoint URL
   - `APIM_SUBSCRIPTION_KEY`: Generated APIM subscription key
   - `SAP_TECHNICAL_USER`: Technical user name
   - `SAP_SYSTEM_ID`: SAP system ID

### 4.3 Security Integration Configuration

1. Deploy Sentinel Analytics Rules:
   - Import [sample KQL query](../../examples/security/sentinel/sample-query.kql)
   - Configure alert thresholds and response actions
   - Create custom workbooks for monitoring

2. Configure logging and auditing:
   - Enable diagnostic settings for APIM to send logs to Sentinel
   - Configure SAP security audit log forwarding
   - Set up GitHub audit log integration

## 5. Validation and Testing

### 5.1 Integration Testing

Test each component of the integration:

1. **SAP to GitHub Synchronization**
   - Create/modify ABAP objects in SAP DEV
   - Validate objects appear in GitHub repository
   - Check transport request creation

2. **GitHub to SAP Deployment**
   - Create pull request in GitHub
   - Verify automated workflows trigger
   - Confirm changes deploy to SAP QAS and PRD

3. **Security Validation**
   - Verify security logs are collected in Sentinel
   - Test detection of anomalous transports
   - Validate Defender is monitoring components

### 5.2 Performance Testing

1. Measure API response times and throughput.
2. Test with increasing volumes of code changes.
3. Monitor system performance during synchronization.

## 6. Cutover and Go-Live

### 6.1 Final Preparation

1. Complete user training.
2. Finalize documentation.
3. Update support procedures.

### 6.2 Production Deployment

1. Perform final validation of all components.
2. Migrate production configuration.
3. Enable monitoring and alerts.

### 6.3 Post-Implementation Review

1. Document lessons learned.
2. Identify optimization opportunities.
3. Plan for ongoing improvements.

## Next Steps

After completing the installation, proceed to the [Quick Start Guide](./quick-start.md) for initial usage instructions.

---

**Document Metadata:**
- Last Updated: 2023-09-01
- Contributors: Installation Team
- Version: 1.0.0
- Status: Published
