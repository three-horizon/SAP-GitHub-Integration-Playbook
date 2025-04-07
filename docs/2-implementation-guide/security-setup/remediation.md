# 📄 Remediation Actions for Microsoft Sentinel

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
This document provides detailed instructions for configuring and implementing automated remediation actions in Microsoft Sentinel for security incidents in your SAP-GitHub integration environment. These remediation capabilities enable rapid response to security threats, reducing exposure time and potential impact.

## ✅ Prerequisites
- Microsoft Sentinel workspace configured (refer to [Sentinel Workspace Setup](./sentinel-workspace.md))
- Data connectors configured for all components (refer to [Data Collection](./data-collection.md))
- SAP connector deployed (refer to [SAP Connector Setup](./sap-connector.md))
- GitHub connector deployed (refer to [GitHub Connector Setup](./github-connector.md))
- Azure connector configured (refer to [Azure Connector Setup](./azure-connector.md))
- Incident response automation configured (refer to [Incident Response](./incident-response.md))
- Appropriate permissions to create and deploy Logic Apps

## Detailed Content

### Understanding Automated Remediation

Automated remediation provides these key benefits:

1. **Rapid Response**
   - Immediate containment actions
   - Reduced manual intervention
   - Consistent response execution
   - Decreased mean time to remediate

2. **Risk Reduction**
   - Limit attack impact
   - Contain lateral movement
   - Preserve evidence
   - Prevent data exfiltration

3. **Operational Efficiency**
   - Reduced analyst workload
   - Standardized response procedures
   - Documented remediation steps
   - Traceable security actions

4. **Integration Benefits**
   - Cross-system coordinated response
   - Synchronized containment actions
   - Cohesive recovery procedures
   - Unified remediation tracking

### Designing a Remediation Framework

Implement a structured approach to remediation:

1. **Remediation Tiers**
   - **Tier 0**: Fully automated response (no human approval)
   - **Tier 1**: Semi-automated with SOC approval
   - **Tier 2**: Semi-automated with business owner approval
   - **Tier 3**: Guided manual remediation

2. **Response Considerations**
   - Business impact assessment
   - Service level implications
   - Risk-based decision matrix
   - Recovery time objectives

3. **Process Integration**
   - Align with incident management
   - Integrate with change management
   - Connect to business continuity
   - Coordinate with disaster recovery

4. **Authorization Framework**
   - Define approval authorities
   - Document escalation paths
   - Establish emergency procedures
   - Implement verification mechanisms

### Implementing Core SAP Remediation Actions

Implement these automated responses for SAP security incidents:

1. **SAP User Account Containment**
   - Create a Logic App that:
     - Receives incident details from Sentinel
     - Extracts user information from entities
     - Connects to SAP using RFC
     - Executes user lock function (BAPI_USER_LOCK)
     - Updates incident with action details
   - Configure approval workflow for production systems
   - Include rollback mechanism

2. **SAP Session Termination**
   - Create a Logic App that:
     - Identifies active sessions for compromised users
     - Connects to SAP system
     - Executes session termination (TH_DELETE_SESSION)
     - Logs termination events
     - Updates incident with action details
   - Implement selective termination options

3. **SAP Authorization Restriction**
   - Create a Logic App that:
     - Extracts user and role information
     - Connects to SAP system
     - Removes sensitive roles (BAPI_USER_ROLE_REMOVE)
     - Applies emergency profile (restricted access)
     - Updates incident with action details
   - Include business continuity considerations

4. **SAP Transport Block**
   - Create a Logic App that:
     - Identifies suspicious transport
     - Connects to SAP Transport Management System
     - Places transport on hold
     - Notifies transport administrators
     - Updates incident with action details
   - Implement verification procedures

### Implementing Core GitHub Remediation Actions

Implement these automated responses for GitHub security incidents:

1. **GitHub User Account Suspension**
   - Create a Logic App that:
     - Extracts user information from entities
     - Connects to GitHub API
     - Executes user suspension API call
     - Invalidates user sessions
     - Updates incident with action details
   - Include organization membership consideration

2. **GitHub Access Token Revocation**
   - Create a Logic App that:
     - Identifies compromised PAT or OAuth token
     - Connects to GitHub API
     - Executes token revocation API call
     - Lists all user tokens for review
     - Updates incident with action details
   - Add procedures for service account tokens

3. **GitHub Repository Protection**
   - Create a Logic App that:
     - Identifies affected repository
     - Connects to GitHub API
     - Enables temporary branch protection
     - Restricts merges and pushes
     - Updates incident with action details
   - Include CI/CD impact assessment

4. **GitHub Webhook Suspension**
   - Create a Logic App that:
     - Identifies compromised webhook
     - Connects to GitHub API
     - Disables webhook temporarily
     - Notifies integration administrators
     - Updates incident with action details
   - Include integration impact assessment

### Implementing Azure Remediation Actions

Implement these automated responses for Azure security incidents:

1. **Azure User Account Disablement**
   - Create a Logic App that:
     - Extracts user information from entities
     - Connects to Microsoft Graph API
     - Disables user account
     - Revokes refresh tokens
     - Updates incident with action details
   - Include MFA reset procedures

2. **Azure Resource Isolation**
   - Create a Logic App that:
     - Identifies compromised VM or resource
     - Connects to Azure Management API
     - Applies network security group isolation
     - Snapshots VM for investigation
     - Updates incident with action details
   - Include business impact assessment

3. **Azure Key Vault Secret Rotation**
   - Create a Logic App that:
     - Identifies compromised secret
     - Connects to Key Vault API
     - Creates new secret version
     - Updates dependent services
     - Updates incident with action details
   - Implement secret dependency mapping

4. **Azure RBAC Adjustment**
   - Create a Logic App that:
     - Identifies compromised service principal
     - Connects to Azure Management API
     - Revokes role assignments
     - Applies least privilege temporary role
     - Updates incident with action details
   - Include service impact assessment

### Implementing API Management Remediation

Secure the integration points with these API remediation actions:

1. **API Rate Limiting**
   - Create a Logic App that:
     - Identifies abused API
     - Connects to API Management
     - Applies strict throttling policy
     - Monitors for legitimate traffic impact
     - Updates incident with action details
   - Include business impact assessment

2. **API Key Rotation**
   - Create a Logic App that:
     - Identifies compromised API key
     - Connects to API Management
     - Generates new API key
     - Updates authorized applications
     - Updates incident with action details
   - Include service continuity procedures

3. **API IP Restriction**
   - Create a Logic App that:
     - Identifies malicious IP addresses
     - Connects to API Management
     - Applies IP filtering policy
     - Monitors for legitimate traffic impact
     - Updates incident with action details
   - Include whitelisting procedures for legitimate traffic

4. **API Error Response Customization**
   - Create a Logic App that:
     - Modifies error responses during active attack
     - Limits information disclosure
     - Implements deception techniques
     - Logs all access attempts
     - Updates incident with action details
   - Include normal operations restoration procedures

### Building Cross-System Remediation Playbooks

Implement these integrated remediation actions across systems:

1. **Identity Compromise Response**
   ```
   Playbook: Cross-System-Identity-Containment
   Trigger: Identity compromise incident
   Actions:
     1. Verify identity compromise indicators
     2. Request SOC analyst approval
     3. Disable Azure AD account
     4. Revoke active tokens
     5. Lock SAP user account
     6. Terminate active SAP sessions
     7. Suspend GitHub user
     8. Revoke GitHub tokens
     9. Disable service account access
     10. Update incident with all actions taken
     11. Notify security team with summary
   ```

2. **Lateral Movement Containment**
   ```
   Playbook: Lateral-Movement-Containment
   Trigger: Lateral movement detection incident
   Actions:
     1. Identify affected systems and accounts
     2. Request SOC analyst approval
     3. Isolate affected systems using NSGs
     4. Apply network filtering between systems
     5. Restrict cross-system API calls
     6. Implement enhanced monitoring
     7. Disable non-essential integration points
     8. Limit authentication scope
     9. Update incident with all actions taken
     10. Initiate cross-system investigation
   ```

3. **Data Exfiltration Prevention**
   ```
   Playbook: Data-Exfiltration-Prevention
   Trigger: Data exfiltration incident
   Actions:
     1. Identify exfiltration channel
     2. Request SOC analyst approval
     3. Block external communication paths
     4. Revoke compromised credentials
     5. Disable relevant storage access
     6. Apply enhanced DLP policies
     7. Restrict GitHub repository access
     8. Implement SAP table access logging
     9. Update incident with all actions taken
     10. Initiate data impact assessment
   ```

4. **Malicious Code Quarantine**
   ```
   Playbook: Malicious-Code-Quarantine
   Trigger: Malicious code detection incident
   Actions:
     1. Identify affected code repositories
     2. Request SOC and developer approval
     3. Isolate affected branches
     4. Block transport release
     5. Disable CI/CD pipelines
     6. Create quarantine environment
     7. Extract code for analysis
     8. Block deployment paths
     9. Update incident with all actions taken
     10. Initiate code review process
   ```

### Building Approval Workflows

Implement these approval mechanisms for sensitive remediation actions:

1. **SOC Analyst Approval**
   - Create a Logic App that:
     - Sends approval request to SOC team
     - Provides incident context and proposed actions
     - Allows approve/reject with comments
     - Implements timeout with escalation
     - Logs all approval decisions
   - Include emergency override procedure

2. **Business Owner Approval**
   - Create a Logic App that:
     - Identifies appropriate business owner
     - Sends approval request with business impact
     - Provides incident context in business terms
     - Implements SLA-based timeout
     - Logs all approval decisions
   - Include delegated approval options

3. **Multi-level Approval**
   - Create a Logic App that:
     - Implements sequential approval workflow
     - Routes based on system and action type
     - Provides full context at each level
     - Allows conditional approval paths
     - Logs approval chain for compliance
   - Include expedited path for critical incidents

4. **Approval Documentation**
   - Create a Logic App that:
     - Captures all approval metadata
     - Documents decision justification
     - Attaches approval to incident
     - Creates approval audit trail
     - Provides approval summary report
   - Include regulatory compliance information

### Post-Remediation Actions

Implement these critical follow-up activities:

1. **System Verification**
   - Create a Logic App that:
     - Verifies system state after remediation
     - Checks service availability
     - Validates security controls
     - Tests authentication flows
     - Documents system status
   - Include integration testing procedures

2. **Security Posture Validation**
   - Create a Logic App that:
     - Runs security compliance checks
     - Verifies new security controls
     - Validates detection capabilities
     - Tests monitoring coverage
     - Documents security improvements
   - Include security regression testing

3. **Recovery Procedures**
   - Create a Logic App that:
     - Defines restoration sequence
     - Implements phased recovery
     - Validates business functions
     - Tests integration points
     - Documents recovery timeline
   - Include business sign-off requirement

4. **Lessons Learned Process**
   - Create a Logic App that:
     - Captures remediation effectiveness
     - Documents response timeline
     - Identifies improvement areas
     - Creates follow-up tasks
     - Schedules review meeting
   - Include control enhancement recommendations

## Best Practices

1. **Remediation Design Principles**
   - Implement least disruptive response
   - Focus on containment before eradication
   - Consider business impact of all actions
   - Document all remediation dependencies
   - Test remediation actions thoroughly

2. **Technical Safeguards**
   - Implement circuit breakers
   - Apply rate limiting to actions
   - Include validation steps
   - Create rollback mechanisms
   - Test failure scenarios

3. **Operational Considerations**
   - Maintain up-to-date documentation
   - Train responders on remediation options
   - Conduct regular tabletop exercises
   - Review and update actions quarterly
   - Practice complex remediation scenarios

4. **Governance Guidelines**
   - Align with incident classification
   - Document approval thresholds
   - Maintain remediation runbooks
   - Review actions after execution
   - Update playbooks based on lessons learned

## Troubleshooting

### Common Issues and Solutions

- **Remediation Action Failures**:
  - Verify service connections and authentication
  - Check permissions and access rights
  - Validate input parameters and formats
  - Review API version compatibility
  - Test actions independently before integration

- **Approval Workflow Issues**:
  - Check email notification delivery
  - Verify approval timeout settings
  - Test escalation paths
  - Validate mobile approval functionality
  - Ensure approver list is current

- **Integration Challenges**:
  - Test cross-system dependencies
  - Verify service account permissions
  - Check network connectivity between systems
  - Validate API rate limits and quotas
  - Test sequence-dependent actions

## ➡️ Next Steps

After configuring remediation actions, proceed to:

- [Notification Configuration](./notifications.md) - Configure security notifications
- [Security Governance](./security-governance.md) - Establish governance framework
- [Compliance Setup](./compliance-setup.md) - Configure compliance reporting
- [Audit Configuration](./audit-setup.md) - Set up comprehensive auditing

## References

- [Microsoft Sentinel Playbooks Documentation](https://docs.microsoft.com/en-us/azure/sentinel/automate-responses-with-playbooks)
- [Logic Apps Documentation](https://docs.microsoft.com/en-us/azure/logic-apps/)
- [SAP API Business Hub](https://api.sap.com/)
- [GitHub REST API Documentation](https://docs.github.com/en/rest)
- [Microsoft Graph API Documentation](https://docs.microsoft.com/en-us/graph/overview)
- [NIST Incident Response Guidelines](https://nvlpubs.nist.gov/nistpubs/SpecialPublications/NIST.SP.800-61r2.pdf)

---

**Document Metadata:**
- Last Updated: 2023-09-05
- Contributors: Security Operations Team, Automation Team, Integration Team
- Version: 1.1.0 
---

<details>
<summary><strong>📊 Document Metadata</strong></summary>

- **Last Updated:** 2025-04-07
- **Version:** 1.0.0
- **Status:** Published
</details>
