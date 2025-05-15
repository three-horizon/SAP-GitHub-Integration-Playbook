# 📄 SAP Connector Setup for Microsoft Sentinel

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
This document provides detailed instructions for configuring the SAP connector in Microsoft Sentinel to monitor security events from your SAP systems. This integration enables security teams to detect threats, investigate suspicious activities, and respond to security incidents within your SAP landscape, especially those involved in the SAP-GitHub integration.

## ✅ Prerequisites
- Microsoft Sentinel workspace configured (refer to [Sentinel Workspace Setup](./sentinel-workspace.md))
- SAP NetWeaver ABAP systems (7.4 or higher recommended)
- SAP Security Audit Log (SAL) enabled
- System and Client-level permissions in SAP
- Azure Log Analytics workspace
- Azure Log Analytics agent (formerly MMA)
- Appropriate permissions in Azure to create and manage resources
- Network connectivity between SAP systems and Azure

## Detailed Content

### Understanding SAP Security Events

Familiarize yourself with the SAP security events to be monitored:

1. **Authentication Events**
   - Logon attempts (successful and failed)
   - User lockouts
   - RFC authentication
   - SSO activities
   - Password changes
   - Emergency user access

2. **Authorization Events**
   - User role assignments
   - Profile changes
   - Authorization object modifications
   - Sensitive transaction usage
   - Critical authorization checks
   - Privilege escalation activities

3. **System and Configuration Events**
   - Parameter changes
   - Table modifications
   - Critical transaction execution
   - System configuration changes
   - Client settings modifications
   - RFC destination changes

4. **Transport Events**
   - Transport creations
   - Transport releases
   - Transport imports
   - Emergency changes
   - Critical object modifications
   - Transport approval activities

5. **Custom Development Events**
   - ABAP program changes
   - Function module modifications
   - Class implementations
   - Table structure changes
   - View modifications
   - Program execution

### Architecture Planning

Design the SAP connector architecture:

1. **Integration Architecture**
   - SAP Security Audit Log as primary source
   - Additional logs and tables as needed
   - Log Analytics agent for log forwarding
   - Log Analytics for log storage
   - Microsoft Sentinel for security monitoring
   - Custom functions for data enrichment

2. **Scaling Considerations**
   - Event volume estimation
   - Log file sizes and growth
   - Collection frequency
   - Retention requirements
   - Performance impact on SAP
   - Azure resource sizing

3. **Security Architecture**
   - SAP user permissions
   - Network security controls
   - Data encryption requirements
   - Credential management
   - Access control implementation
   - Separation of duties

4. **Deployment Architecture**
   - Agent deployment strategy
   - Server placement
   - Network connectivity
   - High availability considerations
   - Disaster recovery planning
   - Multi-system deployment

### SAP Security Audit Log Configuration

Configure SAP Security Audit Log for comprehensive monitoring:

1. **Enable Security Audit Log**
   - Log in to SAP system
   - Execute transaction code SM19
   - Navigate to "Configuration" tab
   - Set "Active" status to "Yes"
   - Configure log destination (file or database)
   - Set appropriate file path if using file-based logging

2. **Configure Audit Categories**
   - In SM19, navigate to "Filter Settings"
   - Enable these audit categories:
     - User (logon, user changes)
     - Authorization checks
     - Dialog
     - RFC/ CPIC
     - RFC User Management
     - Configuration Changes
     - System Startup/Shutdown
     - Critical System Events
     - Security Admin Functions

3. **Define Auditing Filters**
   - Configure user filters (specify users to audit or exclude)
   - Define transaction code filters (focus on sensitive transactions)
   - Set terminal filters (if specific client machines need monitoring)
   - Configure audit class filters
   - Set appropriate filter combinations
   - Document filter settings

4. **Configure Log Size and Rotation**
   - Set maximum log file size
   - Configure log rotation settings
   - Define archiving strategy
   - Establish backup procedures
   - Set appropriate retention period
   - Monitor log file growth

### Additional SAP Log Sources

Configure these additional SAP logs for comprehensive security monitoring:

1. **System Log (SYSLOG)**
   - Access via transaction SM21
   - Configure appropriate filters
   - Set retention period
   - Define log backup procedures
   - Configure log server settings
   - Monitor log growth

2. **Table Change Logs**
   - Enable change logging for critical tables:
     - USR* (User master data)
     - AGR_* (Roles and authorizations)
     - TSTC* (Transaction codes)
     - DEVK* (Development keys)
     - RSAU* (Audit settings)
   - Configure via transaction SE13
   - Set appropriate logging parameters
   - Define archiving strategy

3. **User Information System**
   - Configure scheduled reports via SUIM
   - Set up regular extracts of:
     - User role assignments
     - Critical authorization assignments
     - User list and attributes
     - Last logon information
   - Schedule automated delivery

4. **Transport System Logs**
   - Enable detailed logging in transaction STMS
   - Configure transport layer monitoring
   - Set up transport approval logging
   - Enable logging for critical objects
   - Configure transport logging parameters
   - Set appropriate verbosity

### Configuring Log Collection

Set up the Azure Log Analytics agent to collect SAP logs:

1. **Prepare Log Analytics Workspace**
   - Navigate to Azure Portal
   - Access Log Analytics workspaces
   - Select your Sentinel workspace
   - Note workspace ID and primary key
   - Configure appropriate retention settings
   - Enable relevant solutions

2. **Deploy Log Analytics Agent**
   - On the SAP application server:
     - For Windows:
       - Download the Windows agent from Azure Portal
       - Install with the appropriate workspace ID and key
       - Configure proxy settings if needed
       - Enable Azure AD authentication if applicable
     - For Linux:
       - Install the OMS agent package
       - Configure with workspace ID and key
       - Set appropriate permissions
       - Configure proxy if needed
   - Verify agent is reporting to Azure

3. **Configure Log Collection Rules**
   - Navigate to agent settings
   - Configure SAP log file paths:
     ```
     /usr/sap/*/*/log/audit*.log
     /usr/sap/*/*/log/system*.log
     /usr/sap/*/*/work/dev_w*
     ```
   - Set appropriate collection frequency
   - Configure log filtering if needed
   - Enable multi-line log collection
   - Set up log rotation handling

4. **Create Custom Log Collection**
   - In Log Analytics, create custom log configuration
   - Upload sample SAP audit log
   - Define the custom log format
   - Configure collection paths
   - Set timestamp format
   - Define log file encoding

### Creating Data Connector in Microsoft Sentinel

Set up the SAP connector in Microsoft Sentinel:

1. **Deploy the SAP Security Solution**
   - Navigate to Microsoft Sentinel
   - Select your workspace
   - Click "Content Hub" in the left menu
   - Search for "SAP"
   - Select the SAP solution
   - Click "Install"
   - Wait for installation to complete

2. **Configure Custom Logs Connection**
   - Navigate to Microsoft Sentinel
   - Select your workspace
   - Click "Data connectors" in the left menu
   - Search for "Custom Logs"
   - Configure the connection for SAP logs
   - Validate log collection

3. **Check Data Reception**
   - Navigate to Microsoft Sentinel
   - Select your workspace
   - Click "Logs" in the left menu
   - Run this query to check for SAP audit logs:
     ```kql
     SAPAudit_CL
     | where TimeGenerated > ago(1h)
     | take 10
     ```
   - Verify logs are being received

4. **Create Custom Parsers**
   - Navigate to Log Analytics workspace
   - Click "Logs" in the left menu
   - Create and save functions to parse SAP logs:
     ```kql
     let SAPAudit = SAPAudit_CL
     | extend
         EventTime = extract("(\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2})", 1, RawData),
         EventType = extract("AUT\\((\\w+)\\)", 1, RawData),
         User = extract("User '(\\w+)'", 1, RawData),
         Terminal = extract("Terminal (\\S+)", 1, RawData),
         Client = extract("Client (\\d+)", 1, RawData),
         TransactionCode = extract("Transaction (\\w+)", 1, RawData),
         Action = extract("Action '([^']+)'", 1, RawData),
         Status = iif(RawData contains "successful", "SUCCESS", "FAILURE")
     | extend EventTime = todatetime(EventTime)
     | project-away RawData;
     ```
   - Save the function with a suitable name (e.g., SAPAudit)

### Implementing SAP-Specific Analytics Rules

Create detection rules for SAP security monitoring:

1. **Authentication Rules**
   - Navigate to Microsoft Sentinel
   - Select your workspace
   - Click "Analytics" in the left menu
   - Click "Create" > "Scheduled query rule"
   - Create rules for:
     - Multiple failed logon attempts
     - Emergency user access
     - Unusual user logon patterns
     - Off-hours authentication
     - Dormant account usage
     - Session hijacking indicators

2. **Authorization Rules**
   - Create rules for:
     - Privilege escalation
     - Critical authorization assignments
     - Sensitive transaction usage
     - Unauthorized access attempts
     - Role and profile changes
     - Authorization bypasses

3. **Transaction Monitoring Rules**
   - Create rules for:
     - Sensitive transaction execution
     - Critical table access
     - System configuration changes
     - Parameter modifications
     - Client setting changes
     - System maintenance activities

4. **Transport Monitoring Rules**
   - Create rules for:
     - Emergency change transports
     - Critical object modifications
     - Unusual transport patterns
     - Transport approval anomalies
     - Production transport imports
     - Transport failures

For detailed rule examples, refer to [Standard Rules Deployment](./standard-rules.md) and [Custom SAP Rules](./custom-sap-rules.md).

### Creating SAP Security Workbooks

Create visualization workbooks for SAP security monitoring:

1. **Deploy SAP Security Workbook**
   - Navigate to Microsoft Sentinel
   - Select your workspace
   - Click "Workbooks" in the left menu
   - Select "Templates" tab
   - Search for "SAP"
   - Select and save the workbook

2. **Customize SAP Security Workbook**
   - Open the SAP Security workbook
   - Click "Edit" to enter edit mode
   - Customize visualizations for:
     - Authentication activities
     - Authorization usage
     - Sensitive transaction execution
     - Transport activities
     - System changes
     - Security violations
   - Click "Save" to save changes

3. **Create Custom SAP Workbook**
   - Navigate to Microsoft Sentinel
   - Select your workspace
   - Click "Workbooks" in the left menu
   - Click "New"
   - Add relevant visualizations:
     - User activity metrics
     - Critical transaction usage
     - Authorization metrics
     - Transport statistics
     - System security posture
     - Compliance status
   - Add detailed KQL queries
   - Save the workbook with appropriate name

4. **Configure Workbook Parameters**
   - Add time range parameters
   - Add system and client filters
   - Add user and transaction filters
   - Add event type selection
   - Configure parameter dependencies
   - Save parameter configurations

For detailed instructions on creating SAP workbooks, refer to [SAP Workbook](./sap-workbook.md).

### Implementing Automated Response

Configure playbooks for automated response to SAP security incidents:

1. **SAP Account Lockdown Playbook**
   - Navigate to Microsoft Sentinel
   - Select your workspace
   - Click "Automation" in the left menu
   - Click "Create" > "Playbook with incident trigger"
   - Configure a playbook to:
     - Identify compromised SAP account
     - Lock user account via SAP API
     - Terminate active sessions
     - Document user activities
     - Create investigation task
     - Notify security team
   - Save and enable the playbook

2. **Transport Security Playbook**
   - Create a playbook to:
     - Identify suspicious transports
     - Alert transport administrators
     - Document transport contents
     - Create investigation task
     - Correlate with GitHub changes
     - Suggest remediation actions
   - Save and enable the playbook

3. **Authorization Change Playbook**
   - Create a playbook to:
     - Document unauthorized changes
     - Alert security administrators
     - Create investigation task
     - Suggest remediation steps
     - Track investigation progress
     - Generate compliance evidence
   - Save and enable the playbook

4. **Sensitive Transaction Playbook**
   - Create a playbook to:
     - Document transaction usage
     - Gather transaction details
     - Create investigation task
     - Assess business impact
     - Notify business owners
     - Track investigation progress
   - Save and enable the playbook

For detailed instructions on automation, refer to [Transport Rules](./transport-rules.md) and [Auth Rules](./auth-rules.md).

### Integration with GitHub Security

Configure correlation between SAP and GitHub security events:

1. **User Activity Correlation**
   - Create analytics rules to correlate:
     - SAP developer activities with GitHub commits
     - SAP user activities with GitHub user activities
     - Authentication patterns across systems
     - Suspicious access from shared IP addresses
   - Configure entity mapping for correlation
   - Set up incident grouping

2. **Code and Transport Correlation**
   - Create analytics rules to correlate:
     - SAP transports with GitHub commits
     - Development object changes in both systems
     - Code modification patterns
     - Release and commit timing
   - Configure entity mapping for correlation
   - Set up incident grouping

3. **Security Alert Correlation**
   - Create analytics rules to correlate:
     - SAP vulnerabilities with GitHub code scanning
     - Security findings across systems
     - Authorization changes with repository permissions
     - Suspicious activity across systems
   - Configure entity mapping for correlation
   - Set up incident grouping

For detailed information on GitHub integration, refer to [GitHub Connector Setup](./github-connector.md).

## 🔧 Implementation Plan

Follow this phased approach to implement the SAP connector:

1. **Phase 1: SAP Preparation (Week 1)**
   - Configure SAP Security Audit Log
   - Set up additional logging sources
   - Define log rotation and management
   - Test log generation
   - Document logging configuration

2. **Phase 2: Azure Configuration (Week 1-2)**
   - Deploy Log Analytics agent
   - Configure log collection
   - Set up custom logs in Log Analytics
   - Create parsing functions
   - Verify data ingestion

3. **Phase 3: Analytics Rules (Week 2)**
   - Deploy authentication monitoring rules
   - Implement authorization rules
   - Configure transaction monitoring
   - Set up transport detection
   - Test rule effectiveness

4. **Phase 4: Advanced Features (Week 3-4)**
   - Create custom workbooks
   - Configure automated response playbooks
   - Implement GitHub correlation
   - Tune detection thresholds
   - Document operational procedures

## Best Practices

1. **SAP Logging Configuration**
   - Balance detail with performance impact
   - Focus on security-relevant events
   - Configure appropriate filtering
   - Monitor log file growth
   - Implement log rotation
   - Document logging settings

2. **Data Collection**
   - Optimize collection frequency
   - Filter unnecessary events
   - Ensure consistent data format
   - Monitor agent performance
   - Document collection settings
   - Validate data completeness

3. **Detection Rules**
   - Focus on high-value security events
   - Tune thresholds to reduce false positives
   - Include context in alerts
   - Implement appropriate severity levels
   - Document detection logic
   - Review rules regularly

4. **Operational Considerations**
   - Monitor agent health
   - Create operational runbooks
   - Document troubleshooting procedures
   - Establish maintenance windows
   - Implement change control
   - Train security analysts

## Troubleshooting

### Common Issues and Solutions

- **SAP Log Access Issues**:
  - Verify SAP user permissions
  - Check file system permissions
  - Validate log file locations
  - Ensure log rotation is working
  - Check disk space
  - Verify audit settings in SM19

- **Agent Collection Problems**:
  - Verify agent installation
  - Check workspace ID and key
  - Confirm file paths configuration
  - Test network connectivity
  - Check agent service status
  - Validate proxy settings if used

- **Parsing Issues**:
  - Check log format consistency
  - Update parsing functions
  - Handle special characters properly
  - Account for log format changes
  - Test parsing logic
  - Implement error handling

- **Performance Concerns**:
  - Monitor SAP system impact
  - Adjust logging verbosity
  - Optimize collection frequency
  - Implement appropriate filtering
  - Balance detail with volume
  - Monitor Azure resource usage

## ➡️ Next Steps

After configuring the SAP connector, proceed to:

- [Custom SAP Rules](./custom-sap-rules.md) - Implement advanced detection rules
- [SAP Workbook](./sap-workbook.md) - Create detailed security dashboards
- [Transport Rules](./transport-rules.md) - Implement transport monitoring
- [Integration Dashboard](./integration-dashboard.md) - Create comprehensive security view

## References

- [SAP Security Audit Log Guide](https://help.sap.com/viewer/c6e6d078ab99452db94ed7b3b7bbcccf/7.5.9/en-US/c769f2302ee10147b58f81e70ace488c.html)
- [Microsoft Sentinel Documentation](https://docs.microsoft.com/en-us/azure/sentinel/)
- [Log Analytics Agent Documentation](https://docs.microsoft.com/en-us/azure/azure-monitor/agents/log-analytics-agent)
- [KQL Query Language Reference](https://docs.microsoft.com/en-us/azure/data-explorer/kusto/query/)
- [SAP on Azure Documentation](https://docs.microsoft.com/en-us/azure/virtual-machines/workloads/sap/get-started)

---




