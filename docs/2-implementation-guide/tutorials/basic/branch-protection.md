# 📄 Configuring Branch Protection Rules for SAP Development

## 📋 Table of Contents

- [📋 Overview](#overview)
- [✅ Prerequisites](#prerequisites)
- [Step 1: Analyze Your SAP Development Workflow](#step-1-analyze-your-sap-development-workflow)
- [Step 2: Access Branch Protection Settings](#step-2-access-branch-protection-settings)
- [Step 3: Configure Protection for Main Branch](#step-3-configure-protection-for-main-branch)
- [Step 4: Configure Protection for Development Branch](#step-4-configure-protection-for-development-branch)
- [Step 5: Configure Pattern-Based Rules for Feature Branches](#step-5-configure-pattern-based-rules-for-feature-branches)
- [Step 6: Configure SAP Transport-Aligned Branch Protection](#step-6-configure-sap-transport-aligned-branch-protection)
- [Step 7: Create a CODEOWNERS File](#step-7-create-a-codeowners-file)
- [Step 8: Test Your Branch Protection Rules](#step-8-test-your-branch-protection-rules)
- [Step 9: Document Your Branch Protection Strategy](#step-9-document-your-branch-protection-strategy)
- [Validation](#validation)
- [Troubleshooting](#troubleshooting)
- [➡
️ ️ Next Steps](#next-steps)
- [Additional Resources](#additional-resources)


This tutorial provides a step-by-step guide for implementing effective branch protection rules for SAP development in GitHub, ensuring code quality and governance in your development workflows.

## 📋 Overview

Branch protection rules are critical safeguards that help maintain code quality and governance in SAP development projects. When properly configured, they:

- Prevent accidental or unauthorized changes to critical branches
- Enforce code review processes and quality standards
- Maintain alignment between GitHub workflows and SAP transport management
- Support compliance requirements in regulated environments
- Ensure proper testing before code reaches production

This tutorial walks you through setting up effective branch protection rules specifically tailored for SAP development.

## ✅ Prerequisites

Before you begin, ensure you have:

- [ ] Administrative access to your GitHub Enterprise organization
- [ ] A GitHub repository set up for SAP development (see [Setting Up Your First SAP-GitHub Repository](./first-repository.md))
- [ ] Understanding of your organization's development and release processes
- [ ] Familiarity with your SAP transport landscape

## Step 1: Analyze Your SAP Development Workflow

1. Document your current SAP development workflow, considering:
   - Development system architecture (DEV, QAS, PRD)
   - Transport management processes
   - Release management approach
   - Current approval workflows
   - Compliance requirements

2. Map your SAP workflow to GitHub branching strategy:
   - Main branch → Production-ready code
   - Development branch → Development system
   - Feature branches → Individual features/fixes
   - Release branches → Release candidates

## Step 2: Access Branch Protection Settings

1. Log in to GitHub Enterprise with administrator privileges
2. Navigate to your SAP repository
3. Click on **Settings** in the top navigation menu
4. In the left sidebar, select **Branches**
5. Under "Branch protection rules", click the **Add rule** button

## Step 3: Configure Protection for Main Branch

The main branch typically represents production-ready code and requires the strictest protection:

1. In the "Branch name pattern" field, enter `main`
2. Configure the following protection settings:
   - ✅ **Require a pull request before merging**
     - ✅ **Require approvals** - Set to 2 (or appropriate number)
     - ✅ **Dismiss stale pull request approvals when new commits are pushed**
     - ✅ **Require review from Code Owners**
   - ✅ **Require status checks to pass before merging**
     - ✅ **Require branches to be up to date before merging**
     - Add required status checks:
       - SAP transport validation
       - ABAP syntax check
       - ABAP unit tests
   - ✅ **Require conversation resolution before merging**
   - ✅ **Include administrators** (ensures no bypassing of rules)
   - ✅ **Restrict who can push to matching branches** (limit to release managers)
3. Click **Create** to save the rule

## Step 4: Configure Protection for Development Branch

The development branch typically represents your SAP development system:

1. Click **Add rule** again
2. In "Branch name pattern", enter `develop` (or your development branch name)
3. Configure slightly less restrictive protection:
   - ✅ **Require a pull request before merging**
     - ✅ **Require approvals** - Set to 1
     - ✅ **Dismiss stale pull request approvals when new commits are pushed**
   - ✅ **Require status checks to pass before merging**
     - Add basic checks:
       - ABAP syntax check
   - ✅ **Require conversation resolution before merging**
   - ✅ **Include administrators**
   - ✅ **Allow force pushes** - Limited to specific users (e.g., technical lead)
4. Click **Create** to save the rule

## Step 5: Configure Pattern-Based Rules for Feature Branches

For feature branches, you can use pattern matching:

1. Click **Add rule** again
2. In "Branch name pattern", enter `feature/*`
3. Configure appropriate protection:
   - ✅ **Require a pull request before merging**
     - ✅ **Require approvals** - Set to 1
   - ✅ **Require status checks to pass before merging**
     - Add basic checks:
       - ABAP syntax check
   - ❌ **Restrict who can push** (allows developers to work freely on feature branches)
4. Click **Create** to save the rule

## Step 6: Configure SAP Transport-Aligned Branch Protection

To align with SAP transport landscape:

1. Click **Add rule** again
2. In "Branch name pattern", enter `transport/*`
3. Configure protection specific to transport validation:
   - ✅ **Require a pull request before merging**
     - ✅ **Require approvals** - Set to 1
   - ✅ **Require status checks to pass before merging**
     - Add transport-specific checks:
       - SAP transport validation
       - SAP dependency check
   - ✅ **Require conversation resolution before merging**
4. Click **Create** to save the rule

## Step 7: Create a CODEOWNERS File

1. In your repository, create a new file named `CODEOWNERS` in the `.github` directory
2. Define ownership for critical SAP components:
   ```
   # SAP Core Components
   /src/z_core_*           @sap-core-team
   
   # Transport Configuration
   /src/transport/*        @transport-admins
   
   # Security Components
   /src/z_auth_*           @security-team
   
   # Default owners for everything else
   *                       @sap-developers
   ```
3. Commit the file to your repository

## Step 8: Test Your Branch Protection Rules

To verify your configuration works as expected:

1. **Test Main Branch Protection**:
   - Try to push directly to main (should be rejected)
   - Create a PR to main without approvals (should block merge)
   - Create a PR that fails checks (should block merge)
   - Create a proper PR with approvals and passing checks (should allow merge)

2. **Test Development Branch Protection**:
   - Follow the same process for the development branch

3. **Test Feature Branch Workflow**:
   - Create a feature branch
   - Make changes and push
   - Create a PR to the development branch
   - Verify status checks run
   - Get approvals and merge

## Step 9: Document Your Branch Protection Strategy

1. Create a `BRANCHING.md` file in your repository documenting:
   - Branch naming conventions
   - Protection rules implemented
   - Required reviews and approvals
   - Relationship to SAP transport process
   - Emergency override procedures (if applicable)

2. Share this document with all developers

## Validation

Your branch protection setup is successful when:

- Direct pushes to protected branches are prevented
- Pull requests require the configured number of approvals
- Status checks (ABAP syntax, transport validation, etc.) must pass
- Code owners are automatically requested for review when their code is changed
- The process aligns with your SAP transport management workflow

## Troubleshooting

### Common Issues

| Issue | Solution |
|-------|----------|
| Cannot create branch protection rule | Verify you have admin permissions for the repository |
| Status checks not appearing | Ensure workflows are properly configured to report status |
| Too restrictive rules blocking work | Adjust rules to balance protection with productivity |
| CODEOWNERS not triggering reviews | Check syntax and ensure users/teams exist in the organization |

### Advanced Troubleshooting

If you encounter persistent issues:

1. Check the GitHub audit log for detailed information
2. Temporarily disable specific protection settings to isolate the problem
3. Ensure all GitHub Actions workflows are correctly configured
4. Verify that service users have appropriate permissions

## ➡️ Next Steps

Now that you've configured branch protection rules, consider:

- [Setting Up GitHub Actions Workflows](./github-actions-setup.md) for automated testing and validation
- [Working with Feature Branches](../development/feature-branches.md) for effective development workflows
- [Managing Pull Requests](../development/pull-requests.md) for streamlined review processes
- [Automating Transport Creation](../transport/automated-creation.md) to integrate with SAP transport management

## Additional Resources

- [GitHub Branch Protection Documentation](https://docs.github.com/en/repositories/configuring-branches-and-merges-in-your-repository/defining-the-mergeability-of-pull-requests/about-protected-branches)
- [SAP Transport Management Best Practices](../../implementation/sap-setup/transport-layer.md)
- [Code Review Guidelines](../development/code-reviews.md)

---

*Last Updated: [YYYY-MM-DD] for [GitHub Enterprise X.Y.Z]* 
---

<details>
<summary><strong>📊 Document Metadata</strong></summary>

- **Last Updated:** 2025-04-07
- **Version:** 1.0.0
- **Status:** Published
</details>
