# Incident Response Automation for Microsoft Sentinel

## Overview
This document provides detailed instructions for configuring automated incident response in Microsoft Sentinel for your SAP-GitHub integration. This implementation enables security teams to respond quickly and consistently to security incidents, reducing mean time to remediate (MTTR) and improving overall security posture.

## Prerequisites
- Microsoft Sentinel workspace configured (refer to [Sentinel Workspace Setup](./sentinel-workspace.md))
- Data connectors configured for SAP and GitHub (refer to [SAP Connector Setup](./sap-connector.md) and [GitHub Connector Setup](./github-connector.md))
- Analytics rules deployed (refer to [Standard Rules Deployment](./standard-rules.md))
- Azure Logic Apps subscription with appropriate permissions
- Access to Microsoft Sentinel playbooks
- Appropriate permissions to create and manage automation rules

## Detailed Content

### Understanding SOAR Capabilities in Microsoft Sentinel

Security Orchestration, Automation, and Response (SOAR) in Microsoft Sentinel offers:

1. **Incident Management**
   - Centralized incident tracking
   - Automated enrichment
   - Priority-based assignment
   - Resolution status tracking

2. **Response Automation**
   - Playbooks for common scenarios
   - Automated containment actions
   - Evidence collection
   - Standardized investigation procedures

3. **Integration Capabilities**
   - Integration with ticketing systems
   - Communication tool alerts
   - Custom webhook actions
   - Third-party security tool integration

### Configuring Automation Rules

Automation rules trigger actions when incidents are created:

1. **Creating Basic Automation Rules**
   - Navigate to Microsoft Sentinel > Automation
   - Click "Create" > "Automation rule"
   - Configure rule conditions:
     - **Name**: Descriptive name (e.g., "Assign GitHub Security Incidents")
     - **Trigger**: When incident is created
     - **Conditions**: Specify criteria (e.g., product name contains "GitHub")
   - Add actions:
     - **Assign incident**: Assign to specific user or group
     - **Change severity**: Set appropriate severity level
     - **Add tags**: Apply relevant tags
     - **Run playbook**: Execute response playbook
   - Click "Apply" to save the rule

2. **SAP-GitHub Incident Assignment Rules**
   ```
   Rule Name: Assign SAP-GitHub Integration Incidents
   Conditions:
     - Analytics rule name contains one of: "SAP", "GitHub", "Transport"
     - Product name equals one of: "GitHub", "SAP Application"
     - Severity is one of: High, Medium
   Actions:
     - Assign to: SAP Security Team
     - Add tags: "SAP-GitHub-Integration"
     - Run playbook: "Enrich-SAP-GitHub-Incident"
   ```

3. **Severity-Based Escalation Rules**
   ```
   Rule Name: Escalate Critical SAP-GitHub Incidents
   Conditions:
     - Analytics rule name contains one of: "SAP", "GitHub", "Transport"
     - Severity equals: High
     - Tags contain: "Production"
   Actions:
     - Change status: Active
     - Assign to: Security Operations Manager
     - Change severity: High
     - Add tags: "Escalated", "Critical"
     - Run playbook: "Critical-Incident-Notification"
   ```

### Creating Response Playbooks

Playbooks automate security response actions:

1. **GitHub User Containment Playbook**
   - Navigate to Microsoft Sentinel > Automation
   - Click "Create" > "Playbook with incident trigger"
   - Configure basic information:
     - **Name**: GitHub-User-Containment
     - **Subscription**: Your Azure subscription
     - **Resource group**: Your Sentinel resource group
   - In the Logic Apps Designer:
     - Use the incident trigger template
     - Add "For each" block for entities of type account
     - Add GitHub API connector action to:
       - Revoke active sessions
       - Reset API keys
       - Disable user account
     - Add notification actions
     - Add incident update actions
   - Save and test the playbook

2. **SAP User Lockout Playbook**
   - Create a new playbook for SAP user containment
   - Configure SAP system connections
   - Add logic to:
     - Lock user accounts in SAP
     - Revoke active sessions
     - Log containment actions
   - Include notification and documentation steps

3. **Transport Rollback Playbook**
   - Create a playbook for SAP transport remediation
   - Add steps to:
     - Identify affected transport
     - Create rollback transport
     - Execute emergency correction
     - Document changes
   - Include approval workflow for critical systems

### Creating Key Integration Playbooks

Implement these essential playbooks for your SAP-GitHub environment:

1. **Incident Enrichment Playbook**

   ```
   Playbook: Enrich-SAP-GitHub-Incident
   Trigger: When an incident is created
   Actions:
     1. Get incident details and entities
     2. For user entities:
        - Retrieve SAP user details (SU01 data)
        - Retrieve GitHub user activity history
        - Add user role information
     3. For host entities:
        - Get SAP system information
        - Retrieve recent system changes
     4. For IP entities:
        - Perform geolocation lookup
        - Check IP reputation
     5. Add all gathered information to incident comments
     6. Update incident with enriched entities
   ```

2. **Cross-System Correlation Playbook**

   ```
   Playbook: Correlate-SAP-GitHub-Activity
   Trigger: When a SAP or GitHub incident is created
   Actions:
     1. Extract relevant identifiers (user, transport, commit)
     2. Query GitHub activity for related events
     3. Query SAP logs for related activities
     4. Identify timeline of events across systems
     5. Create relationship graph visualization
     6. Add correlation summary to incident
     7. Tag related incidents
   ```

3. **Approval-Based Remediation Playbook**

   ```
   Playbook: Approval-Based-Remediation
   Trigger: Manual trigger from incident
   Actions:
     1. Extract remediation details from incident
     2. Create approval email with remediation actions
     3. Wait for approval response
     4. If approved:
        - Execute containment actions in GitHub (disable user, revoke tokens)
        - Execute containment actions in SAP (lock user, revoke sessions)
        - Add documentation to incident
     5. If rejected:
        - Document rejection reason
        - Escalate to next level approver if needed
   ```

### Integrating with External Systems

Connect your incident response to external systems:

1. **ServiceNow Integration**
   - Create a playbook using ServiceNow connector
   - Synchronize incidents bi-directionally
   - Map severity and priority fields
   - Ensure status updates flow between systems

2. **Microsoft Teams Integration**
   - Create adaptive cards for incident notifications
   - Enable response actions directly from Teams
   - Create incident war room channels
   - Add relevant stakeholders automatically

3. **Email Notification System**
   - Configure email templates for different incident types
   - Set up escalation workflows
   - Include relevant incident details
   - Provide response options in the email

### Implementing Automated Investigation

Configure automated investigation workflows:

1. **User Investigation Workflow**
   - Create entity-focused investigation playbooks
   - Automatically gather user activity timeline
   - Correlate across SAP and GitHub systems
   - Generate investigation summary

2. **Transport Investigation**
   - Track transport chain of custody
   - Identify related GitHub commits
   - Generate object impact analysis
   - Recommend remediation options

3. **Environment Impact Assessment**
   - Evaluate affected systems
   - Determine business impact
   - Identify related business processes
   - Document affected data

### Building a Custom Incident Dashboard

Create a specialized dashboard for tracking incidents:

1. **Dashboard Design**
   - Navigate to Microsoft Sentinel > Workbooks
   - Create a new workbook for incident tracking
   - Add visualizations for:
     - Incident volume by system
     - Incident response metrics
     - MTTR by incident type
     - Trending security issues

2. **SAP-GitHub Specific Metrics**
   ```kql
   SecurityIncident
   | where TimeGenerated > ago(30d)
   | where Tags has_any ("SAP", "GitHub", "SAP-GitHub-Integration")
   | extend TimeToResolve = datetime_diff('minute', TimeResolved, TimeCreated)/60
   | summarize AvgTimeToResolve=avg(TimeToResolve) by Title, Severity
   | order by AvgTimeToResolve desc
   ```

## Best Practices

1. **Playbook Management**
   - Use source control for playbook development
   - Implement a testing environment
   - Document all automations
   - Review and update regularly

2. **Approval Workflows**
   - Implement approval flows for destructive actions
   - Document approval decision criteria
   - Define escalation paths
   - Set timeouts for approvals

3. **Response Standardization**
   - Create response runbooks for common scenarios
   - Document manual steps when needed
   - Implement post-incident reviews
   - Update automations based on lessons learned

4. **Performance Considerations**
   - Monitor playbook execution times
   - Optimize Logic Apps for performance
   - Set appropriate timeouts
   - Implement error handling

## Troubleshooting

### Common Issues and Solutions

- **Playbook Execution Failures**:
  - Check connector authentication
  - Verify API permissions
  - Review Logic Apps run history
  - Validate JSON schemas for outputs

- **Missing Incident Information**:
  - Verify entity mapping in analytics rules
  - Check alert details configuration
  - Ensure data connectors are working properly
  - Validate enrichment playbook execution

- **Integration Failures**:
  - Check API connectivity
  - Verify service account permissions
  - Test connections independently
  - Check network restrictions

## Next Steps

After configuring incident response automation, proceed to:

- [Remediation Actions](./remediation.md) - Set up automated remediation capabilities
- [Notification Configuration](./notifications.md) - Configure security notifications
- [Security Governance](./security-governance.md) - Establish security governance framework
- [Compliance Reporting](./compliance-setup.md) - Configure compliance reporting

## References

- [Microsoft Sentinel Playbooks Documentation](https://docs.microsoft.com/en-us/azure/sentinel/automate-responses-with-playbooks)
- [Logic Apps Documentation](https://docs.microsoft.com/en-us/azure/logic-apps/)
- [GitHub API Documentation](https://docs.github.com/en/rest)
- [SAP Automation Best Practices](https://www.sap.com/documents/2016/06/e2070a5b-757c-0010-82c7-eda71af511fa.html)
- [NIST Incident Response Guide](https://nvlpubs.nist.gov/nistpubs/SpecialPublications/NIST.SP.800-61r2.pdf)

---

**Document Metadata:**
- Last Updated: 2023-08-10
- Contributors: Security Operations Team, Automation Team
- Version: 1.1.0 