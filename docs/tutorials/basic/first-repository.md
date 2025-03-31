# Setting Up Your First SAP-GitHub Repository

This tutorial walks you through the complete process of setting up your first SAP package with GitHub integration. By following these steps, you'll establish the foundation for modern SAP development with version control, collaboration, and automated workflows.

## Prerequisites

Before you begin, ensure you have:

- [ ] Access to your SAP development system (DEV)
- [ ] abapGit installed in your SAP system (see [abapGit Installation](../../implementation/sap-setup/abapgit-installation.md))
- [ ] Access to GitHub Enterprise with repository creation permissions
- [ ] The SAP-GitHub integration components installed (see [Installation Guide](../../getting-started/installation.md))
- [ ] Required authorizations in SAP and GitHub

## Step 1: Create a New ABAP Package

Start by creating a new ABAP package in your SAP system:

1. Log in to your SAP system and open transaction **SE80** or **SE21**
2. Create a new package:
   - Name: `Z_GITHUB_DEMO` (or your preferred package name)
   - Description: "GitHub Integration Demo"
   - Package type: Development
   - Software component: Select appropriate component (e.g., `HOME`)
   - Transport layer: Select your transport layer (e.g., `ZDEV`)
3. Save the package and create a transport request when prompted
4. Make note of the transport request number for later use

**Note**: Package naming conventions may vary by organization. Follow your organization's standards.

## Step 2: Create a GitHub Repository

Next, create a new repository in GitHub Enterprise:

1. Log in to your GitHub Enterprise instance
2. Click **New** to create a new repository
3. Configure the repository:
   - Owner: Select your organization
   - Repository name: `sap-github-demo` (or align with your package name)
   - Description: "SAP GitHub Integration Demo"
   - Visibility: Private (recommended for SAP code)
   - Initialize with README: Yes
   - Add .gitignore: None (we'll use abapGit's configuration)
   - License: As appropriate for your organization
4. Click **Create repository**

## Step 3: Connect the SAP Package to GitHub

Now, connect your SAP package to the GitHub repository:

1. In SAP, execute transaction `/GOOG/ZGIT_ADMIN` (or your custom transaction)
2. Select **Create New Repository**
3. Fill in the details:
   - SAP Package: `Z_GITHUB_DEMO` (your package from Step 1)
   - GitHub Repository: `your-org/sap-github-demo` (from Step 2)
   - Transport Request: Enter the transport request from Step 1
   - Branch: `main` (or your default branch name)
4. Click **Create Connection**

The system will:
- Configure abapGit for the package
- Set up the connection to GitHub
- Prepare for the initial synchronization

## Step 4: Initial Repository Structure

Before pushing your ABAP objects, set up an optimal repository structure:

1. In the admin transaction, select your newly created repository
2. Go to **Repository Settings**
3. Configure the following:
   - Folder logic: PREFIX (recommended for SAP packages)
   - Main language: EN (or your primary development language)
   - Ignore configuration: Accept defaults for most cases
4. Save the settings

## Step 5: Create Sample ABAP Objects

For demonstration purposes, create some sample ABAP objects:

1. In transaction SE80, navigate to your package `Z_GITHUB_DEMO`
2. Create a new program:
   - Name: `Z_GITHUB_DEMO_REPORT`
   - Description: "GitHub Integration Demo Report"
   - Type: Executable program
3. Add simple code, for example:
   ```abap
   REPORT z_github_demo_report.
   
   PARAMETERS: p_text TYPE string DEFAULT 'Hello, GitHub!'.
   
   START-OF-SELECTION.
     WRITE: / p_text.
     WRITE: / 'This is a simple demo for GitHub integration.'.
   ```
4. Save and activate the program
5. Assign the program to your transport request

## Step 6: Push to GitHub

Now, push your package content to GitHub:

1. Execute transaction `/GOOG/ZGIT_DEV` (or your custom transaction)
2. Select your package `Z_GITHUB_DEMO`
3. Click **Push to GitHub**
4. Configure the push:
   - Branch: `main` (for initial setup)
   - Commit message: "Initial commit of demo package"
   - Transport: Select your transport request
5. Click **Push**

The system will:
- Serialize all ABAP objects in the package to XML format
- Create a commit in the GitHub repository
- Establish the link between SAP and GitHub objects

## Step 7: Verify the Repository

After pushing, verify that your repository has been set up correctly:

1. Open your GitHub repository in a web browser
2. You should see:
   - The abapGit configuration files (`.abapgit.xml`)
   - A `src` directory containing your ABAP objects
   - Your demo program in XML format

## Step 8: Configure GitHub Actions

Set up basic GitHub Actions workflows for your repository:

1. In your GitHub repository, go to **Actions** tab
2. Click **New workflow**
3. Select **Set up a workflow yourself**
4. Use the following basic workflow:

```yaml
name: SAP Code Validation

on:
  push:
    branches: [ main ]
  pull_request:
    branches: [ main ]

jobs:
  abap_lint:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - name: abaplint
        uses: abaplint/actions-abaplint@main
        env:
          GITHUB_TOKEN: ${{ secrets.GITHUB_TOKEN }}
```

5. Commit the workflow file to your repository

## Step 9: Configure Branch Protection

Implement branch protection for your main branch:

1. In your GitHub repository, go to **Settings** > **Branches**
2. Click **Add rule** under Branch protection rules
3. Configure the rule:
   - Branch name pattern: `main`
   - Require pull request reviews before merging: Checked
   - Required approvers: 1 (or as appropriate)
   - Require status checks to pass before merging: Checked
   - Require branches to be up to date: Checked
4. Click **Create**

## Step 10: Development Workflow Test

Test the development workflow with a simple change:

1. In SAP transaction SE80, open your demo program
2. Make a simple change, such as adding a new line of code
3. Save and activate the change
4. Execute transaction `/GOOG/ZGIT_DEV`
5. Select your package
6. Click **Push to GitHub**
7. Configure the push:
   - Branch: `feature/test-change` (create a new feature branch)
   - Commit message: "Add test change to demo program"
   - Transport: Select your transport request
8. Click **Push**
9. In GitHub, create a pull request from your feature branch to main
10. Review and merge the pull request

## Troubleshooting

If you encounter issues during setup:

- **Connection errors**: Check your network configuration and ensure your SAP system can reach GitHub
- **Authorization errors**: Verify your GitHub credentials and SAP authorizations
- **Object serialization issues**: Check abapGit logs for specific error messages
- **Push/pull failures**: Ensure your transport request is correct and your package is properly configured

Refer to the [Troubleshooting Guide](../../troubleshooting/index.md) for detailed solutions.

## Next Steps

Now that you have your first repository set up, consider these next steps:

- Set up [Branch Protection Rules](../../implementation/github-setup/branch-protection.md) for additional branches
- Configure [Advanced GitHub Actions Workflows](../../implementation/workflows/advanced-workflows.md)
- Implement [Code Review Processes](../../implementation/github-setup/code-review.md) for your team
- Explore [Transport Automation](../../implementation/workflows/transport-automation.md) for streamlined deployments

## Related Resources

- [abapGit Documentation](https://docs.abapgit.org/)
- [GitHub Actions Documentation](https://docs.github.com/en/actions)
- [SAP Transport Management Guide](../../implementation/sap-setup/transport-layer.md)

---

*Last Updated: [YYYY-MM-DD] for [Component Versions: GitHub Enterprise X.Y.Z, SAP NetWeaver X.Y.Z, etc.]* 