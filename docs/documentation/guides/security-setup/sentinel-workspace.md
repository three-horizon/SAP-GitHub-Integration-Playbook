# 📄 Microsoft Sentinel Workspace Setup for SAP-GitHub Integration

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
This document provides detailed instructions for setting up and configuring a Microsoft Sentinel workspace to monitor security across the SAP-GitHub integration environment. Microsoft Sentinel provides security information and event management (SIEM) capabilities combined with security orchestration, automation, and response (SOAR) functionality to enable comprehensive security monitoring and incident response.

## ✅ Prerequisites
- Azure subscription with appropriate permissions
- Microsoft Sentinel licensing
- Resource provider registration for Microsoft.OperationalInsights and Microsoft.SecurityInsights
- Network connectivity to Azure from SAP and GitHub environments
- Service principals or managed identities for authentication
- Understanding of Log Analytics workspace concepts
- Basic familiarity with Azure portal

## Detailed Content

### Planning the Sentinel Implementation

Consider these key factors when planning your Sentinel workspace:

1. **Architecture Design**
   - Single workspace vs. multiple workspaces
   - Centralized vs. distributed model
   - Regional considerations
   - Data sovereignty requirements
   - Integration with existing security tools
   - Multi-tenant requirements

2. **Resource Requirements**
   - Data ingestion volume estimates
   - Query performance needs
   - Alert processing capacity
   - Retention requirements
   - Automated response capabilities
   - Reporting and dashboard needs

3. **Access Control Design**
   - Security operations team access
   - Developer access requirements
   - Executive reporting access
   - Role-based access control model
   - Privileged access management
   - Least privilege implementation

4. **Data Collection Strategy**
   - Event types to collect
   - Collection frequency
   - Filtering requirements
   - Enrichment needs
   - Normalization approach
   - Data quality management

### Creating the Sentinel Workspace

Follow these steps to create your Sentinel workspace:

1. **Create Resource Group**
   - Navigate to Azure Portal
   - Select Resource Groups
   - Click "Create"
   - Enter details:
     ```
     Subscription: <your-subscription>
     Resource Group: rg-sentinel-sap-github-prod
     Region: <select-appropriate-region>
     ```
   - Click "Review + Create" and then "Create"

2. **Create Log Analytics Workspace**
   - Navigate to Log Analytics workspaces
   - Click "Create"
   - Enter details:
     ```
     Subscription: <your-subscription>
     Resource Group: rg-sentinel-sap-github-prod
     Name: law-sentinel-sap-github-prod
     Region: <select-same-region-as-resource-group>
     ```
   - Select appropriate pricing tier based on data volume
   - Click "Review + Create" and then "Create"

3. **Enable Microsoft Sentinel**
   - Navigate to Microsoft Sentinel
   - Click "Create"
   - Select the Log Analytics workspace created in step 2
   - Click "Add"
   - Verify Sentinel is properly enabled on the workspace

4. **Configure Workspace Settings**
   - Navigate to the Log Analytics workspace
   - Select "Usage and estimated costs"
   - Configure data retention settings (default: 90 days)
   - Set daily cap if required
   - Configure diagnostic settings for workspace logs
   - Review and adjust capacity reservations if needed

### Configuring Workspace Security

Implement these security controls for your Sentinel workspace:

1. **Access Control Configuration**
   - Navigate to the Sentinel workspace
   - Select "Access control (IAM)"
   - Assign appropriate built-in roles:
     - Microsoft Sentinel Reader: View-only access
     - Microsoft Sentinel Responder: Respond to incidents
     - Microsoft Sentinel Contributor: Full management
     - Log Analytics Reader: View-only data access
   - Create custom roles for specialized needs
   - Implement Azure AD groups for role assignment

2. **Network Security Configuration**
   - Navigate to the Log Analytics workspace
   - Select "Networking"
   - Configure private link if required
   - Set up firewall rules to restrict access
   - Enable trusted Microsoft services if needed
   - Configure IP-based restrictions when appropriate
   - Document network access controls

3. **Encryption and Data Protection**
   - Navigate to the Log Analytics workspace
   - Select "Encryption"
   - Configure customer-managed keys if required
   - Enable double encryption if needed
   - Document key management procedures
   - Set up key rotation schedules
   - Configure access to encryption keys

4. **Data Exfiltration Protection**
   - Implement appropriate network controls
   - Configure workspace export restrictions
   - Set up data masking for sensitive fields
   - Implement table-level RBAC when required
   - Configure workspace-level protection
   - Document data protection controls

### Configuring Data Collection

Set up these data collection components:

1. **Azure Activity Logs**
   - Navigate to the Sentinel workspace
   - Select "Data connectors"
   - Find and open "Azure Activity"
   - Click "Open connector page"
   - Select subscriptions to connect
   - Click "Connect"
   - Validate data is flowing to Sentinel

2. **Azure AD Logs**
   - Navigate to the Sentinel workspace
   - Select "Data connectors"
   - Find and open "Azure Active Directory"
   - Click "Open connector page"
   - Select log types to collect
   - Click "Connect"
   - Validate data is flowing to Sentinel

3. **GitHub Enterprise Logs**
   - Navigate to the Sentinel workspace
   - Select "Data connectors"
   - Find and open "GitHub Enterprise"
   - Click "Open connector page"
   - Follow the configuration steps:
     - Set up Azure Event Hub
     - Configure GitHub Enterprise logging
     - Connect the Event Hub to Log Analytics
   - Click "Connect"
   - Validate data is flowing to Sentinel

4. **SAP Logs**
   - Navigate to the Sentinel workspace
   - Select "Data connectors"
   - Find and open "SAP" or "Common Event Format (CEF)"
   - Click "Open connector page"
   - Follow the configuration steps:
     - Set up log forwarding from SAP
     - Configure the Log Analytics agent
     - Validate connectivity
   - Click "Connect"
   - Validate data is flowing to Sentinel

5. **Custom Logs**
   - Navigate to the Log Analytics workspace
   - Select "Custom logs"
   - Click "Add custom log"
   - Upload sample log file
   - Define collection paths
   - Define custom fields if needed
   - Complete the wizard
   - Validate custom logs are being collected

6. **Azure Diagnostic Settings**
   - Navigate to each Azure resource
   - Select "Diagnostic settings"
   - Click "Add diagnostic setting"
   - Select logs and metrics to collect
   - Select the Sentinel Log Analytics workspace
   - Enable settings
   - Verify data collection

### Implementing Data Tables and Parsers

Create these data structures for effective analysis:

1. **Custom Tables Creation**
   - Navigate to the Log Analytics workspace
   - Select "Tables"
   - Create custom tables for specific data types
   - Define schema with appropriate columns
   - Configure data retention
   - Document table structure
   - Create sample queries for validation

2. **Parser Function Development**
   - Navigate to the Sentinel workspace
   - Select "Logs"
   - Create KQL functions to parse:
     - SAP security audit logs
     - GitHub security events
     - Integration component logs
     - Custom application logs
   - Save functions with appropriate names
   - Document parsing logic and expected output

3. **Data Normalization Functions**
   - Create KQL functions to normalize:
     - User identities across systems
     - Resource naming conventions
     - Event categorization
     - Severity classifications
     - Time zone standardization
   - Save functions with appropriate names
   - Document normalization logic

4. **Field Mapping Implementation**
   - Create field mapping documentation
   - Implement mapping logic in KQL functions
   - Map to MITRE ATT&CK framework
   - Align with common information model
   - Document entity mapping
   - Create entity relationship diagrams

### Configuring Analytics Rules

Implement these analytics rule categories:

1. **Microsoft-Provided Rules**
   - Navigate to the Sentinel workspace
   - Select "Analytics"
   - Review Microsoft-provided rule templates
   - Enable relevant rules for:
     - Azure AD security alerts
     - Azure resource security
     - Common attack patterns
     - Known threats
   - Customize settings as needed
   - Document enabled rules

2. **GitHub-Specific Rules**
   - Create analytics rules for:
     - Suspicious authentication activity
     - Privilege escalation attempts
     - Unusual repository access
     - Abnormal code commit patterns
     - Secret exposure in code
     - Configuration changes
   - Set appropriate severity levels
   - Configure alert enrichment
   - Enable entity mapping
   - Document rule logic and thresholds

3. **SAP-Specific Rules**
   - Create analytics rules for:
     - Privilege escalation in SAP
     - Transaction security violations
     - Authorization changes
     - Sensitive table access
     - Transport security anomalies
     - Interface security issues
   - Set appropriate severity levels
     - Configure alert enrichment
     - Enable entity mapping
     - Document rule logic and thresholds

4. **Integration-Specific Rules**
   - Create analytics rules for:
     - Authentication failures between systems
     - Data flow anomalies
     - Integration point failures
     - Unexpected changes in access patterns
     - Cross-system correlation of suspicious activity
     - Data integrity issues
   - Set appropriate severity levels
   - Configure alert enrichment
   - Enable entity mapping
   - Document rule logic and thresholds

### Configuring Workbooks

Create these visualization workbooks:

1. **Security Overview Workbook**
   - Navigate to the Sentinel workspace
   - Select "Workbooks"
   - Click "Add workbook"
   - Design overview dashboard with:
     - Incident summary by severity
     - Alert trend analysis
     - System health indicators
     - Top affected resources
     - Security posture indicators
   - Save and publish workbook
   - Configure appropriate sharing

2. **GitHub Security Workbook**
   - Create custom workbook for:
     - Authentication monitoring
     - Code repository security
     - Access pattern analysis
     - Secret management effectiveness
     - Suspicious activity timeline
     - GitHub-specific security metrics
   - Save and publish workbook
   - Document workbook KQL queries

3. **SAP Security Workbook**
   - Create custom workbook for:
     - Authorization monitoring
     - Transaction security
     - Custom code security
     - Table access patterns
     - Interface security
     - SAP-specific security metrics
   - Save and publish workbook
   - Document workbook KQL queries

4. **Integration Security Workbook**
   - Create custom workbook for:
     - Cross-system authentication
     - Data flow security
     - API security metrics
     - Integration failure analysis
     - System correlation
     - Integration-specific security metrics
   - Save and publish workbook
   - Document workbook KQL queries

### Configuring Playbooks

Implement these automation playbooks:

1. **Incident Assignment Playbook**
   - Navigate to the Sentinel workspace
   - Select "Automation"
   - Click "Create" > "Automation rule"
   - Design rule to:
     - Assign incidents to appropriate teams
     - Set incident priority based on criteria
     - Add relevant tags
     - Set status appropriately
   - Configure trigger conditions
   - Activate automation rule

2. **Incident Enrichment Playbook**
   - Create Logic App for:
     - Gathering additional context
     - Adding user information
     - Retrieving asset details
     - Including historical context
     - Adding threat intelligence
   - Configure Sentinel connection
   - Set up appropriate triggers
   - Test and deploy the playbook
   - Document playbook logic

3. **Response Action Playbooks**
   - Create Logic Apps for:
     - User account lockdown
     - IP address blocking
     - System isolation procedures
     - Evidence collection
     - Communication workflows
   - Configure appropriate approvals
   - Implement secure authentication
   - Test and deploy the playbooks
   - Document each playbook's purpose and function

4. **Integration-Specific Playbooks**
   - Create Logic Apps for:
     - GitHub repository isolation
     - SAP user containment
     - API gateway restrictions
     - Integration service management
     - Cross-system correlation
   - Configure connections to each system
   - Implement appropriate authentication
   - Test and deploy the playbooks
   - Document integration points and prerequisites

### Implementing Data Export

Configure these data export mechanisms:

1. **Long-term Archive Configuration**
   - Navigate to the Log Analytics workspace
   - Configure data export to Azure Storage
   - Set up appropriate retention policies
   - Implement encryption for export data
   - Configure access controls
   - Document archive structure
   - Create data retrieval procedures

2. **Security Information Sharing**
   - Configure data sharing with:
     - Central security operations
     - Partner security teams
     - Regulatory reporting systems
     - Executive dashboards
   - Implement appropriate data filtering
   - Configure secure transmission
   - Document sharing agreements
   - Create data usage guidelines

3. **SIEM Integration**
   - Configure integration with:
     - Enterprise SIEM systems
     - Threat intelligence platforms
     - Security orchestration systems
     - Central log management
   - Implement appropriate data mapping
   - Configure secure connectivity
   - Test data flow
   - Document integration architecture

### Implementing Cost Management

Configure these cost optimization components:

1. **Data Volume Management**
   - Implement data filtering at source
   - Configure appropriate sampling
   - Create data aggregation strategies
   - Implement tiered collection based on severity
   - Configure workspace data caps
   - Document data collection strategy

2. **Query Optimization**
   - Create efficient KQL queries
   - Implement appropriate functions
   - Use materialized views where beneficial
   - Configure query timeout limits
   - Document query patterns
   - Create query templates

3. **Retention Management**
   - Configure workspace retention settings
   - Implement table-specific retention
   - Create data archiving procedures
   - Configure data purging policies
   - Implement regulatory holds when needed
   - Document retention requirements

4. **Cost Monitoring**
   - Create cost monitoring dashboard
   - Implement cost anomaly detection
   - Configure cost alerts
   - Create cost allocation reporting
   - Implement cost optimization reviews
   - Document cost management procedures

## Best Practices

1. **Architecture Considerations**
   - Align with enterprise monitoring strategy
   - Consider scalability requirements
   - Plan for geographic distribution
   - Define clear workspace boundaries
   - Document architecture decisions
   - Create migration path for future changes

2. **Performance Optimization**
   - Implement efficient table design
   - Use appropriate partitioning
   - Create optimized query patterns
   - Configure workbooks for performance
   - Implement dashboard caching
   - Schedule resource-intensive operations

3. **Security Controls**
   - Implement least privilege access
   - Use managed identities
   - Enable multi-factor authentication
   - Implement private endpoints
   - Use encryption for sensitive data
   - Regularly review access controls

4. **Operational Guidelines**
   - Create clear runbooks
   - Document response procedures
   - Implement change management
   - Perform regular validation testing
   - Create operational metrics
   - Implement continuous improvement

## Troubleshooting

### Common Issues and Solutions

- **Data Collection Issues**:
  - Verify agent installation
  - Check connectivity to Log Analytics
  - Validate firewall and network settings
  - Confirm API keys and authentication
  - Check source log formatting
  - Verify data connector configuration

- **Query Performance Problems**:
  - Optimize KQL queries
  - Check query scope
  - Implement appropriate filtering
  - Use time-based constraints
  - Consider materialized views
  - Monitor workspace performance

- **Alert Challenges**:
  - Tune alert thresholds
  - Confirm scheduled query execution
  - Check alert action settings
  - Verify incident creation
  - Test alert triggering
  - Review alert suppression rules

## ➡️ Next Steps

After configuring the Sentinel workspace, proceed to:

- [Data Collection](./data-collection.md) - Configure additional data collection
- [Standard Rules Deployment](./standard-rules.md) - Implement security detections
- [GitHub Connector Setup](./github-connector.md) - Configure GitHub monitoring
- [SAP Connector Setup](./sap-connector.md) - Configure SAP monitoring

## References

- [Microsoft Sentinel Documentation](https://docs.microsoft.com/en-us/azure/sentinel/overview)
- [Log Analytics Workspace Design](https://docs.microsoft.com/en-us/azure/azure-monitor/logs/workspace-design)
- [KQL Query Language Reference](https://docs.microsoft.com/en-us/azure/data-explorer/kql-quick-reference)
- [Sentinel Best Practices](https://docs.microsoft.com/en-us/azure/sentinel/best-practices)
- [Microsoft Security Insights Community](https://techcommunity.microsoft.com/t5/microsoft-sentinel/bd-p/MicrosoftSentinel)

---




