# 📄 GitHub Connector Setup for Microsoft Sentinel

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
This document provides detailed instructions for configuring the GitHub connector in Microsoft Sentinel to monitor security events from your GitHub Enterprise environment. This integration enables security teams to detect threats, investigate suspicious activities, and respond to security incidents within your GitHub repositories, especially those involved in the SAP-GitHub integration.

## ✅ Prerequisites
- Microsoft Sentinel workspace configured (refer to [Sentinel Workspace Setup](./sentinel-workspace.md))
- GitHub Enterprise Cloud or GitHub Enterprise Server instance
- Organization owner or security manager permissions in GitHub
- GitHub Enterprise with audit log capabilities enabled
- Azure Event Hubs namespace and authorization rules
- Appropriate permissions in Azure to create and manage resources
- Network connectivity between GitHub and Azure

## Detailed Content

### Understanding GitHub Security Events

Familiarize yourself with the GitHub security events to be monitored:

1. **Authentication Events**
   - User login attempts (successful and failed)
   - Two-factor authentication events
   - OAuth token usage
   - Personal Access Token (PAT) creation and usage
   - SSH key events
   - SAML single sign-on events

2. **Repository and Organization Events**
   - Repository creation, deletion, and transfers
   - Repository visibility changes
   - Branch protection rule modifications
   - Organization membership changes
   - Team membership and permission changes
   - Outside collaborator management

3. **Code and Secrets Events**
   - Secret scanning alerts
   - Code scanning alerts
   - Push events and commit details
   - Branch creation and deletion
   - Pull request activities
   - Code review events

4. **GitHub Actions and Workflow Events**
   - Workflow runs and statuses
   - Self-hosted runner events
   - Environment creation and configuration
   - Workflow file modifications
   - GitHub Actions usage
   - Environment secret access

5. **Administrative Events**
   - Organization settings changes
   - Enterprise settings modifications
   - Policy changes and enforcement
   - Billing and subscription changes
   - IP allow list modifications
   - GitHub Application installations

### Architecture Planning

Design the GitHub connector architecture:

1. **Integration Architecture**
   - Event Hubs for log ingestion
   - Log Analytics for log storage and analytics
   - Microsoft Sentinel for security monitoring
   - Function App for custom log processing (optional)
   - Key Vault for secret management
   - GitHub webhook for event delivery

2. **Scaling Considerations**
   - Event volume estimation
   - Throughput requirements
   - Storage capacity planning
   - Query performance needs
   - Retention requirements
   - Cost optimization

3. **Security Architecture**
   - Authentication method selection
   - Authorization and permissions model
   - Network security controls
   - Data encryption requirements
   - Secret management approach
   - Access control implementation

4. **Regional Architecture**
   - Event Hub region selection
   - Log Analytics workspace region
   - Network latency considerations
   - Data residency requirements
   - Disaster recovery planning
   - Multi-region deployment (if needed)

### Creating Azure Resources

Create the necessary Azure resources for the GitHub connector:

1. **Resource Group Creation**
   - Navigate to Azure Portal
   - Select Resource Groups
   - Click "Create"
   - Enter details:
     ```
     Subscription: <your-subscription>
     Resource Group: rg-github-sentinel-prod
     Region: <select-appropriate-region>
     ```
   - Click "Review + Create" and then "Create"

2. **Event Hubs Namespace Creation**
   - Navigate to Azure Portal
   - Search for "Event Hubs"
   - Click "Create"
   - Enter details:
     ```
     Subscription: <your-subscription>
     Resource Group: rg-github-sentinel-prod
     Namespace name: evhns-github-sentinel-prod
     Location: <select-same-region-as-resource-group>
     Pricing tier: Standard
     Throughput Units: 1 (start with 1, scale as needed)
     ```
   - Click "Review + Create" and then "Create"

3. **Event Hub Creation**
   - Navigate to your Event Hubs namespace
   - Click "Event Hubs" in the left menu
   - Click "+ Event Hub"
   - Enter details:
     ```
     Name: evh-github-auditlogs-prod
     Partition Count: 4
     Message Retention: 7 days
     Capture: Off (Sentinel will process events)
     ```
   - Click "Create"

4. **Create Event Hubs Authorization Rules**
   - Navigate to your Event Hub
   - Click "Shared access policies" in the left menu
   - Click "+ Add"
   - Enter details:
     ```
     Policy name: github-sender-policy
     Manage: Unchecked
     Send: Checked
     Listen: Unchecked
     ```
   - Click "Create"
   - Create another policy:
     ```
     Policy name: sentinel-listener-policy
     Manage: Unchecked
     Send: Unchecked
     Listen: Checked
     ```
   - Click "Create"

5. **Configure Diagnostic Settings**
   - Navigate to your Event Hub
   - Select "Diagnostic settings" in the left menu
   - Click "Add diagnostic setting"
   - Enter details:
     ```
     Diagnostic setting name: eventhub-logs
     Categories: All logs
     Destination details: Send to Log Analytics workspace
     Subscription: <your-subscription>
     Log Analytics workspace: <your-sentinel-workspace>
     ```
   - Click "Save"

### Configuring GitHub Enterprise

Configure GitHub Enterprise to send audit logs to Azure Event Hubs:

1. **Create GitHub OAuth App**
   - Log in to GitHub Enterprise as an admin
   - Navigate to your profile settings
   - Select "Developer settings"
   - Click "OAuth Apps"
   - Click "New OAuth App"
   - Enter details:
     ```
     Application name: Azure Sentinel Integration
     Homepage URL: https://portal.azure.com
     Application description: OAuth app for Azure Sentinel integration
     Authorization callback URL: https://portal.azure.com
     ```
   - Click "Register application"
   - Note the Client ID
   - Generate a new client secret and securely store it

2. **Configure GitHub Enterprise Audit Log Streaming**
   - Log in to GitHub Enterprise as an enterprise administrator
   - Navigate to Enterprise settings
   - Select "Audit log" in the left menu
   - Click "Configure streaming"
   - Select "Azure Event Hubs" as the stream destination
   - Enter the configuration details:
     ```
     Event Hubs namespace: evhns-github-sentinel-prod.servicebus.windows.net
     Event Hub name: evh-github-auditlogs-prod
     Connection string: <connection-string-from-github-sender-policy>
     ```
   - Click "Check endpoint" to validate
   - Click "Save" to enable streaming

3. **Configure GitHub Organization Webhooks (Optional, for additional events)**
   - Log in to GitHub as an organization owner
   - Navigate to the organization settings
   - Select "Webhooks" in the left menu
   - Click "Add webhook"
   - Enter details:
     ```
     Payload URL: <your-webhook-endpoint-or-azure-function>
     Content type: application/json
     Secret: <generate-and-securely-store-a-secret>
     Which events would you like to trigger this webhook? Select events:
       - Security alerts
       - Code scanning alerts
       - Repository
       - Organization
       - Member
       - Team
     ```
   - Ensure "Active" is checked
   - Click "Add webhook"

4. **Enable Advanced Security Features**
   - Log in to GitHub as an organization owner
   - Navigate to the organization settings
   - Select "Code security and analysis" in the left menu
   - Enable these security features:
     - Dependency graph
     - Dependabot alerts
     - Dependabot security updates
     - Secret scanning
     - Push protection
     - Code scanning

### Creating Data Connector in Microsoft Sentinel

Set up the GitHub connector in Microsoft Sentinel:

1. **Deploy the GitHub Connector Solution**
   - Navigate to Microsoft Sentinel
   - Select your workspace
   - Click "Content Hub" in the left menu
   - Search for "GitHub"
   - Select "GitHub Enterprise" solution
   - Click "Install"
   - Wait for installation to complete

2. **Configure the GitHub Data Connector**
   - Navigate to Microsoft Sentinel
   - Select your workspace
   - Click "Data connectors" in the left menu
   - Search for "GitHub Enterprise"
   - Select the connector and click "Open connector page"
   - Configure connection details:
     ```
     Event Hub namespace: evhns-github-sentinel-prod
     Event Hub name: evh-github-auditlogs-prod
     Event Hub policy name: sentinel-listener-policy
     Event Hub policy key: <key-from-sentinel-listener-policy>
     Consumer group: $Default (or create a dedicated one)
     ```
   - Click "Connect"
   - Verify the connection status

3. **Check Data Reception**
   - Navigate to Microsoft Sentinel
   - Select your workspace
   - Click "Logs" in the left menu
   - Run this query to check for GitHub audit logs:
     ```kql
     GitHubAuditLogPolling
     | where TimeGenerated > ago(1h)
     | take 10
     ```
   - Verify logs are being received

4. **Create Custom Parsers (if needed)**
   - Navigate to Log Analytics workspace
   - Click "Logs" in the left menu
   - Create and save a function to parse GitHub audit logs:
     ```kql
     let GitHubAudit = GitHubAuditLogPolling
     | extend
         Actor = tostring(parse_json(RawEventData).actor),
         Action = tostring(parse_json(RawEventData).action),
         Repository = tostring(parse_json(RawEventData).repo),
         Organization = tostring(parse_json(RawEventData).org),
         IPAddress = tostring(parse_json(RawEventData).actor_ip),
         CreatedAt = todatetime(parse_json(RawEventData).created_at),
         AdditionalFields = parse_json(RawEventData)
     | project-away RawEventData;
     ```
   - Save the function with a suitable name (e.g., GitHubAudit)

### Implementing Analytics Rules

Create detection rules for GitHub security monitoring:

1. **Enable Microsoft-Provided Rules**
   - Navigate to Microsoft Sentinel
   - Select your workspace
   - Click "Analytics" in the left menu
   - Filter by solution: "GitHub Enterprise"
   - Review and enable relevant templates:
     - GitHub authentication anomalies
     - GitHub privileged activity
     - GitHub repository modifications
     - GitHub secret scanning alerts
   - Customize thresholds and settings as needed

2. **Create Custom Detection Rules**
   - Navigate to Microsoft Sentinel
   - Select your workspace
   - Click "Analytics" in the left menu
   - Click "Create" > "Scheduled query rule"
   - Implement detection rules for:
     - Suspicious access patterns
     - Unusual repository activities
     - Code security events
     - GitHub Actions security
     - Authentication anomalies
   - Refer to [Standard Rules Deployment](./standard-rules.md) for rule examples

3. **Configure Entity Mapping**
   - Edit each analytics rule
   - Configure entity mapping for:
     - Account: map to actor or user
     - IP Address: map to actor_ip
     - URL: map to repository URLs
     - File: map to file paths
   - Enable entity mapping for incident correlation

4. **Validate Rule Detection**
   - Generate test events in GitHub
   - Verify rule detection in Sentinel
   - Tune rule parameters as needed
   - Document any false positives
   - Adjust thresholds and conditions
   - Test incident creation workflow

### Creating GitHub Workbooks

Create visualization workbooks for GitHub security monitoring:

1. **Deploy GitHub Activity Workbook**
   - Navigate to Microsoft Sentinel
   - Select your workspace
   - Click "Workbooks" in the left menu
   - Select "Templates" tab
   - Search for "GitHub"
   - Click on "GitHub Enterprise Activity"
   - Click "Save" to save it to your workspace
   - Click "View saved workbook"

2. **Customize GitHub Activity Workbook**
   - Open the GitHub Activity workbook
   - Click "Edit" to enter edit mode
   - Customize visualizations for:
     - Authentication activities
     - Repository security events
     - Organization changes
     - User access patterns
     - Security alert trends
   - Click "Save" to save changes

3. **Create Custom GitHub Security Workbook**
   - Navigate to Microsoft Sentinel
   - Select your workspace
   - Click "Workbooks" in the left menu
   - Click "New"
   - Add relevant visualizations:
     - Authentication metrics
     - Repository security posture
     - Code security status
     - GitHub Actions security
     - Secret scanning effectiveness
   - Add detailed KQL queries
   - Save the workbook with appropriate name

4. **Configure Workbook Parameters**
   - Add time range parameters
   - Add organization filter
   - Add repository filter
   - Add event type selection
   - Add user filter
   - Configure parameter dependencies

For detailed instructions on creating GitHub workbooks, refer to [GitHub Workbook](./github-workbook.md).

### Implementing Automated Response

Configure playbooks for automated response to GitHub security incidents:

1. **GitHub Account Lockdown Playbook**
   - Navigate to Microsoft Sentinel
   - Select your workspace
   - Click "Automation" in the left menu
   - Click "Create" > "Playbook with incident trigger"
   - Configure a playbook to:
     - Identify compromised GitHub account
     - Use GitHub API to suspend user
     - Revoke active sessions
     - Remove suspicious PATs
     - Delete suspicious SSH keys
     - Notify security team
   - Save and enable the playbook

2. **GitHub Repository Protection Playbook**
   - Create a playbook to:
     - Enable temporary stricter branch protection
     - Block force pushes
     - Require additional reviews
     - Lock issue discussions
     - Restrict workflow runs
     - Notify repository administrators
   - Save and enable the playbook

3. **Secret Rotation Playbook**
   - Create a playbook to:
     - Identify exposed secrets
     - Initiate secret rotation
     - Revoke compromised credentials
     - Update affected repositories
     - Create issues for follow-up
     - Document remediation actions
   - Save and enable the playbook

4. **Suspicious Activity Investigation Playbook**
   - Create a playbook to:
     - Gather additional context
     - Collect user activity timeline
     - Identify affected repositories
     - Create security investigation task
     - Prepare forensic data
     - Alert security analysts
   - Save and enable the playbook

For detailed instructions on response automation, refer to [Incident Response](./incident-response.md).

### Integration with SAP Security

Configure correlation between GitHub and SAP security events:

1. **User Activity Correlation**
   - Create analytics rules to correlate:
     - GitHub user activities with SAP user activities
     - SAP developer activities with GitHub commits
     - Authentication patterns across systems
     - Suspicious access from shared IP addresses
   - Configure entity mapping for correlation
   - Set up incident grouping

2. **Transport and Commit Correlation**
   - Create analytics rules to correlate:
     - SAP transports with GitHub commits
     - Development object changes in both systems
     - Unusual code modification patterns
     - Transport and commit timing anomalies
   - Configure entity mapping for correlation
   - Set up incident grouping

3. **Security Alert Correlation**
   - Create analytics rules to correlate:
     - SAP vulnerabilities with GitHub code changes
     - Security findings across systems
     - Authorization changes with repository permissions
     - Suspicious activity across systems
   - Configure entity mapping for correlation
   - Set up incident grouping

For detailed information on SAP integration, refer to [SAP Connector Setup](./sap-connector.md).

## 🔧 Implementation Plan

Follow this phased approach to implement the GitHub connector:

1. **Phase 1: Initial Setup (Day 1-3)**
   - Create Azure resources
   - Configure GitHub audit log streaming
   - Set up the GitHub data connector
   - Verify data collection
   - Enable basic analytics rules

2. **Phase 2: Enhanced Monitoring (Day 4-7)**
   - Deploy additional analytics rules
   - Customize GitHub workbooks
   - Configure entity mapping
   - Set up initial alert workflow
   - Begin correlation with other data sources

3. **Phase 3: Automation (Week 2)**
   - Implement automated response playbooks
   - Configure workflow automation
   - Test incident response procedures
   - Document response workflows
   - Train security analysts

4. **Phase 4: Integration and Tuning (Week 3-4)**
   - Implement SAP correlation rules
   - Tune detection thresholds
   - Optimize workbook performance
   - Document false positives
   - Refine incident handling procedures

## Best Practices

1. **Data Collection**
   - Collect all relevant audit events
   - Filter noisy events at the source
   - Ensure consistent data format
   - Monitor data ingestion
   - Document data fields
   - Validate data completeness

2. **Detection Rules**
   - Focus on high-value security events
   - Tune rules to reduce false positives
   - Include context in alerts
   - Implement appropriate severity levels
   - Document detection logic
   - Review rules regularly

3. **Operational Considerations**
   - Monitor connector health
   - Create operational runbooks
   - Document troubleshooting procedures
   - Establish maintenance windows
   - Implement change control
   - Train security analysts

4. **Security Hardening**
   - Use managed identities where possible
   - Store secrets in Key Vault
   - Implement least privilege
   - Enable encryption in transit
   - Configure network security
   - Regularly rotate credentials

## Troubleshooting

### Common Issues and Solutions

- **No Events Received**:
  - Verify GitHub streaming configuration
  - Check Event Hub connectivity
  - Validate Event Hub policies
  - Confirm GitHub permission settings
  - Check network connectivity
  - Verify Sentinel connector configuration

- **Parsing Errors**:
  - Check GitHub event format
  - Update custom parsers
  - Validate KQL syntax
  - Test parsing logic
  - Handle null values appropriately
  - Account for schema changes

- **Performance Issues**:
  - Optimize query efficiency
  - Check Event Hub throughput units
  - Monitor Sentinel workspace performance
  - Filter unnecessary events
  - Implement efficient data handling
  - Schedule resource-intensive queries

## ➡️ Next Steps

After configuring the GitHub connector, proceed to:

- [GitHub Workbook](./github-workbook.md) - Create detailed security dashboards
- [Incident Response](./incident-response.md) - Configure automated response
- [SAP Connector Setup](./sap-connector.md) - Integrate with SAP security
- [Integration Dashboard](./integration-dashboard.md) - Create comprehensive security view

## References

- [GitHub Enterprise Audit Log Documentation](https://docs.github.com/en/enterprise-cloud@latest/admin/monitoring-activity-in-your-enterprise/reviewing-audit-logs-for-your-enterprise/about-the-audit-log-for-your-enterprise)
- [GitHub API Documentation](https://docs.github.com/en/rest)
- [Microsoft Sentinel Documentation](https://docs.microsoft.com/en-us/azure/sentinel/)
- [Azure Event Hubs Documentation](https://docs.microsoft.com/en-us/azure/event-hubs/)
- [KQL Query Language Reference](https://docs.microsoft.com/en-us/azure/data-explorer/kusto/query/)

---




