# 🔄 abapGit Installation and Configuration Guide

<div align="center" class="svg-container">
  <!-- Using both object and img as fallback for maximum compatibility -->
  <object type="image/svg+xml" data="\1" style="width: 700px; max-width: 100%;" aria-label="\1">
    <img src="\1" alt="\1" width="700" />
  </object>
  
  *\1*
</div>

## 📋 Table of Contents

- [📋 Overview](#-overview)
- [✅ Prerequisites](#-prerequisites)
- [📝 Installation Checklist](#-installation-checklist)
- [💾 Step-by-Step Installation](#-step-by-step-installation)
- [📋 Configuration Checklist](#-configuration-checklist)
- [⚙️ Step-by-Step Configuration](#️-step-by-step-configuration)
- [🔄 Repository Setup](#-repository-setup)
- [🤖 Automation Integration](#-automation-integration)
- [🔍 Troubleshooting](#-troubleshooting)
- [➡️ Next Steps](#️-next-steps)

## 📋 Overview

This guide provides detailed instructions for installing and configuring abapGit in your SAP system as part of the SAP-GitHub integration. abapGit is the core component that enables version control for ABAP objects and facilitates the synchronization between SAP and GitHub.

## ✅ Prerequisites

Before installing abapGit, ensure your environment meets the following requirements:

- SAP NetWeaver 7.02 or higher (7.40 or higher recommended)
- SAP_BASIS 702 or higher
- Developer access to the SAP system
- Required authorizations:
  - S_DEVELOP - For development objects
  - S_TCODE - For transaction codes
  - S_CTS_ADMI - For transport management
- SSL/HTTPS connection to GitHub Enterprise
- GitHub Enterprise account with repository access
- SAP Notes installed:
  - Note 2297281: abapGit compatibility
  - Note 2655547: OAuth authentication for external interfaces 
  - Note 2573788: Security for external integrations
  - Note 2937996: Support packages for abapGit

## 📝 Installation Checklist

- [ ] Download the latest abapGit version
- [ ] Install the abapGit repository
- [ ] Install the abapGit standalone program (optional)
- [ ] Configure SSL certificates in STRUST
- [ ] Verify developer access and authorizations
- [ ] Create a dedicated service user for automation
- [ ] Test connectivity to GitHub

## 💾 Step-by-Step Installation

### 1. Download Latest abapGit Version

1. Go to the [abapGit releases page](https://github.com/abapGit/abapGit/releases)
2. Download the latest build ZIP file
3. Save the ZIP file to your local system

### 2. Install abapGit Repository

#### Option 1: Using ZABAPGIT_STANDALONE

1. Open the SAP Logon and log in to your SAP system
2. Run transaction `SE38` or `SE80`
3. Create a new program named `ZABAPGIT_STANDALONE`
4. Paste the contents of the standalone version into the program
5. Save and activate the program
6. Execute the program using transaction `SE38`
7. The abapGit standalone version will start
8. Click "Clone Online Repo"
9. Enter the repository URL: `https://github.com/abapGit/abapGit.git`
10. Enter package name (create a new package if needed, e.g., `$ABAPGIT`)
11. Click "Clone Online Repo" to start the installation

#### Option 2: Using abapGit XML File

1. Download the latest abapGit build ZIP file
2. Extract the ZIP file
3. Open the SAP system using transaction `SE80`
4. Create a new package for abapGit (e.g., `$ABAPGIT`)
5. Import the abapGit objects using transaction `SE80` > right-click package > Import > ABAP Objects

> **💡 Tip:** Use Option 1 for initial installation and exploration, and Option 2 for more controlled deployments in enterprise environments.

### 3. Configure SSL Certificates

1. Run transaction `STRUST`
2. Navigate to "SSL client SSL Client (Standard)"
3. Double-click on "SSL client SSL Client (Standard)"
4. Click "Import" to import the certificate for GitHub
5. Select the GitHub certificate file
6. Click "Add to Certificate List"
7. Save the changes

### 4. Create Service User

1. Run transaction `SU01`
2. Create a new user (e.g., `ABAPGIT_SYNC`)
3. Assign required authorization roles
4. Set password or certificate-based authentication
5. Save the user

## 📋 Configuration Checklist

- [ ] Create dedicated package for SAP-GitHub integration
- [ ] Configure offline repository (if needed)
- [ ] Set up online repository connection
- [ ] Configure authentication (user/password or token)
- [ ] Define branch handling strategy
- [ ] Set up transport management integration
- [ ] Configure ignore list for non-versioned objects
- [ ] Set up background processing (optional)
- [ ] Create abapGit favorites (optional)

## ⚙️ Step-by-Step Configuration

### 1. Launch abapGit

1. Run transaction `ZABAPGIT`
2. If this is your first time, the abapGit welcome screen appears

### 2. Configure Repository Settings

1. Click "New Online" or "New Offline"
2. For Online Repository:
   - Enter GitHub repository URL
   - Enter package name
   - Select main language
   - Select transport request
3. For Offline Repository:
   - Enter package name
   - Select main language
   - Select transport request
4. Click "Create"

### 3. Configure Authentication

1. Click "Settings" (wrench icon)
2. In the "Remote Settings" section:
   - Enter your GitHub username
   - Enter your password or personal access token
3. Alternatively, store credentials in secure storage:
   - Run transaction `ZABAPGIT_AUTH`
   - Enter your credentials
   - Associate with repository URL

> **⚠️ Warning:** Always use personal access tokens instead of passwords for improved security and more granular access control.

### 4. Configure Branch Handling

1. Click the branch icon in the repository
2. Select the branch you want to use (e.g., main, develop)
3. To create a new branch:
   - Click "Create"
   - Enter branch name
   - Select base branch
   - Click "Create"

### 5. Set Up Transport Integration

1. Click "Settings" (wrench icon)
2. In the "Transport Settings" section:
   - Select transport layer
   - Configure automatic transport creation
   - Set transport owner
3. Save the settings

### 6. Configure Ignore List

1. Click "Settings" (wrench icon)
2. In the "Ignore Settings" section:
   - Add files or patterns to ignore
   - Common ignores:
     - `.gitignore`
     - `LICENSE`
     - `README.md`
     - `package.json`
     - `.github/`
3. Save the settings

## 🔄 Repository Setup

### 1. Initial Repository Synchronization

1. Navigate to your repository in abapGit
2. For a new repository:
   - Click "Add all objects" to stage all objects
   - Enter commit message
   - Click "Commit"
3. For existing repository:
   - Click "Pull" to get the latest changes
   - Resolve any conflicts
   - Activate objects

### 2. Working with Branches

1. Click the branch icon
2. Select the appropriate branch
3. Development workflow:
   - Work on feature branches
   - Merge changes to main branch
   - Use pull requests for code reviews

## 🤖 Automation Integration

To enable automated synchronization with GitHub Actions:

1. Configure a service user in SAP with required authorizations
2. Store credentials securely in GitHub Secrets
3. Create GitHub Actions workflow that calls abapGit API
4. Set up webhook triggers for automated synchronization

Example API call for automation:

```abap
DATA(lo_repo) = zcl_abapgit_repo_srv=>get_instance( )->get_repository_by_key( '12345' ).
lo_repo->refresh( ).
lo_repo->deserialize( ).
```

## 🔍 Troubleshooting

### Common Issues

1. **SSL/TLS Connection Issues**
   - Check SSL certificates in STRUST
   - Verify proxy settings

2. **Authorization Errors**
   - Verify user authorizations
   - Check if transport layer is authorized

3. **Object Activation Failures**
   - Check object dependencies
   - Verify syntax
   - Review activation order

4. **Background Processing Errors**
   - Check SM37 for background job logs
   - Verify background user authorizations

### Logs and Debugging

1. Run transaction `SLG1` to check application logs
2. Enable debugging in abapGit settings
3. Use transaction `ST22` to check for dumps

## ➡️ Next Steps

After completing the abapGit installation and configuration:

1. Set up [GitHub Actions Workflows](./github-setup/workflow-guide.md) for CI/CD
2. Configure [API Management](./apim-setup/index.md) for secure integration
3. Review the overall [Implementation Plan](./getting-started/implementation-plan.md) for next phases

---

<details>
<summary><strong>📊 Document Metadata</strong></summary>

- **Last Updated:** 2025-04-07
- **Author:** SAP-GitHub Integration Team
- **Version:** 1.0.0
- **Status:** Published
</details>
