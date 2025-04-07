# 📄 Branch Protection Rules for SAP-GitHub Integration

## 📋 Table of Contents

- [📋 Overview](#overview)
- [✅ Prerequisites](#prerequisites)
- [Basic Branch Protection Configuration](#basic-branch-protection-configuration)
- [Advanced Configuration](#advanced-configuration)
- [Status Checks Configuration](#status-checks-configuration)
- [Protecting Source Files Integrity](#protecting-source-files-integrity)
- [Temporary Branch Exceptions](#temporary-branch-exceptions)
- [Best Practices](#best-practices)
- [Testing Your Branch Protection](#testing-your-branch-protection)
- [Troubleshooting](#troubleshooting)
- [➡
️ ️ Next Steps](#next-steps)
- [Additional Resources](#additional-resources)


This guide provides detailed instructions for configuring branch protection rules in GitHub Enterprise for SAP code repositories, ensuring code quality and governance in your development workflows.

## 📋 Overview

Branch protection rules are critical for enforcing quality standards, security practices, and governance in SAP development. When properly configured, they help:

- Prevent direct changes to production code
- Enforce code review processes
- Maintain SAP transport management integrity
- Ensure compliance with organizational policies
- Prevent accidental deletion of important branches

## ✅ Prerequisites

Before configuring branch protection:

- Administrator access to your GitHub Enterprise organization
- Completed [GitHub Enterprise setup](index.md)
- Understanding of your organization's development workflow
- [User access management](authentication.md) configured

## Basic Branch Protection Configuration

### Setting Up Protection for Main Branch

1. Log in to GitHub Enterprise with administrator privileges
2. Navigate to your SAP repository
3. Click **Settings** → **Branches**
4. Under "Branch protection rules," click **Add rule**
5. Configure the following settings:
   - Branch name pattern: `main` (or your primary branch)
   - Check "Require a pull request before merging"
   - Set required approvals to at least 1 (recommend 2 for critical code)
   - Check "Dismiss stale pull request approvals when new commits are pushed"
   - Check "Require status checks to pass before merging"
   - Check "Require branches to be up to date before merging"
   - For critical repositories, consider enabling "Require signed commits"
   - Check "Include administrators" to apply rules to everyone
   - Check "Restrict who can push to matching branches" and select appropriate teams
6. Click **Create** to save the rule

### Protecting Development Branches

For SAP development branches, create an additional rule:

1. Follow the same steps as above
2. Set branch name pattern to `dev/*` or `development/*` (based on your naming convention)
3. Configure less restrictive rules to enable developer workflow:
   - Check "Require a pull request before merging" 
   - Set required approvals to 1
   - Allow specific development teams to push directly
4. Click **Create** to save the rule

## Advanced Configuration

### SAP Transport-aware Rules

To align with SAP transport management:

1. Create branch rules matching your SAP landscape:
   - `sap/dev/*` for development branches
   - `sap/qas/*` for quality assurance branches
   - `sap/prd/*` for production branches
2. Apply increasingly strict protection as you move toward production:
   - Production: Require signed commits, multiple reviews, all status checks
   - QA: Require reviews and critical status checks
   - Development: Basic protection without blocking team productivity

### Feature Branch Protection

For feature-based development workflows:

1. Add a rule with pattern `feature/*`
2. Configure moderate protection:
   - Require pull request and at least one review
   - Required status checks for ABAP lint and basic tests
   - Allow feature owners to manage their branches

### Release Branch Protection

For release management branches:

1. Add a rule with pattern `release/*`
2. Configure strict protection:
   - Require pull request with at least two reviews
   - Required status checks for all tests
   - Restrict push access to release managers only
   - Enable "Allow force pushes" for release managers only to handle exceptional cases

## Status Checks Configuration

Configure required status checks for SAP code validation:

1. In branch protection settings, under "Require status checks to pass before merging"
2. Select relevant status checks:
   - `abaplint` for ABAP syntax checking
   - `transport-validation` for SAP transport validation
   - `security-scan` for security vulnerability checks
   - `unit-tests` for ABAP unit tests

**Note**: These status checks must be implemented as GitHub Actions or other CI integrations (see [CI/CD Setup](../workflows/ci-cd-setup.md)).

## Protecting Source Files Integrity

For critical SAP configuration files:

1. Create a [CODEOWNERS file](https://docs.github.com/en/repositories/managing-your-repositorys-settings-and-features/customizing-your-repository/about-code-owners) in the repository root
2. Add entries for critical files:
```
# SAP critical configuration files
src/config/*.xml @sap-admins
src/transport/*.config @transport-team
```
3. Ensure "Require review from code owners" is enabled in the branch protection rules

## Temporary Branch Exceptions

For special circumstances (urgent fixes, migrations):

1. Document the process for temporarily modifying branch protection
2. Create a temporary exception procedure:
   - Requires approval from a designated authority
   - Time-limited exceptions (24-48 hours maximum)
   - Post-exception review to ensure compliance

## Best Practices

### Rule Hierarchy and Inheritance

Organize rules from general to specific:

1. Create a base rule for all branches (`*`)
2. Apply more specific rules for categories (`feature/*`, `release/*`)
3. Configure most specific rules for individual branches (`main`, `develop`)

### Regular Rule Review

Institute a process for regularly reviewing branch protection:

1. Schedule quarterly reviews of branch protection rules
2. Verify that rules align with current development practices
3. Update rules when new compliance requirements emerge
4. Document rule changes in version-controlled configuration

### Monitoring and Enforcement

Set up monitoring for branch protection:

1. Configure notifications for branch protection bypass attempts
2. Regularly audit branch operations through GitHub audit logs
3. Create dashboards to visualize rule compliance
4. Document and address patterns of rule circumvention

## Testing Your Branch Protection

Verify your branch protection rules work correctly:

1. Attempt to push directly to protected branches (should be rejected)
2. Create a pull request without required reviews (should be blocked from merging)
3. Create a pull request with failing status checks (should be blocked from merging)
4. Create a compliant pull request (should successfully merge)

## Troubleshooting

### Common Issues

| Issue | Resolution |
|-------|------------|
| Protection rules not enforced | Verify "Include administrators" is checked |
| Status checks never complete | Check GitHub Actions workflow configuration |
| Required reviewers not prompted | Verify code owners file syntax |
| Too restrictive rules blocking work | Consider more granular branch patterns |

### Special Cases for SAP Development

For SAP-specific edge cases:

1. **Transport Urgency**: Create documented exception process for urgent transports
2. **System Migration**: Create temporary branches with reduced protection during migrations
3. **External Consultants**: Configure team-based push restrictions rather than blanket rules

## ➡️ Next Steps

Once branch protection is configured, proceed to:

- [Repository Structure Setup](repository-structure.md)
- [Code Review Process Configuration](code-review.md)
- [CI/CD Workflow Configuration](../workflows/ci-cd-setup.md)

## Additional Resources

- [GitHub Branch Protection Documentation](https://docs.github.com/en/repositories/configuring-branches-and-merges-in-your-repository/defining-the-mergeability-of-pull-requests/about-protected-branches)
- [ABAP Development Guidelines](../../best-practices/abap-development.md)
- [Governance Framework](../../governance/index.md)

---

*Last Updated: [YYYY-MM-DD] for [GitHub Enterprise X.Y.Z]* 
---

<details>
<summary><strong>📊 Document Metadata</strong></summary>

- **Last Updated:** 2025-04-07
- **Version:** 1.0.0
- **Status:** Published
</details>
