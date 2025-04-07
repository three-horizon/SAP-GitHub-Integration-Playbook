# 📄 abapGit Installation Guide

## 📋 Table of Contents

- [📋 Overview](#overview)
- [✅ Prerequisites](#prerequisites)
- [Installation Methods](#installation-methods)
- [Method 1: Online Installation](#method-1-online-installation)
- [Method 2: Offline Installation](#method-2-offline-installation)
- [Post-Installation Steps](#post-installation-steps)
- [Troubleshooting](#troubleshooting)
- [Creating a Transaction Code (Optional)](#creating-a-transaction-code-optional)
- [Setting up a Development Package for abapGit (Recommended)](#setting-up-a-development-package-for-abapgit-recommended)
- [Updating abapGit](#updating-abapgit)
- [➡
️ ️ Next Steps](#next-steps)
- [Additional Resources](#additional-resources)


This guide provides detailed instructions for installing abapGit in your SAP system, which is a prerequisite for the SAP-GitHub integration.

## 📋 Overview

abapGit is an open-source git client for ABAP, allowing you to version-control your ABAP objects using git repositories. It serves as the foundation for our SAP-GitHub integration approach.

## ✅ Prerequisites

Before installing abapGit, ensure you have:

- SAP NetWeaver 7.02 or higher (recommended: 7.40 or higher)
- Developer authorization in your SAP system
- S_TCODE authorization for transaction SE38, SE80, and SE24
- S_DEVELOP authorization with object type PROG, CLAS, INTF, etc.
- Authorization to create Z* objects

## Installation Methods

There are two primary methods to install abapGit:

1. **Online Installation** (Recommended if your SAP system has internet access)
2. **Offline Installation** (For systems without direct internet access)

## Method 1: Online Installation

This method requires your SAP system to have internet access to GitHub.

1. Log in to your SAP system
2. Execute transaction code **SE38** (ABAP Editor)
3. Enter program name `ZABAPGIT_FULL`
4. Click **Create**
5. In the program attributes dialog:
   - Enter a title (e.g., "abapGit client")
   - Select type "Executable program"
   - Click **Save**
6. When prompted, create the program in your local package ($TMP or a Z package)
7. Copy the latest abapGit source code from [https://raw.githubusercontent.com/abapGit/build/main/zabapgit.abap](https://raw.githubusercontent.com/abapGit/build/main/zabapgit.abap)
8. Paste the code in the editor
9. Click **Save** and then **Activate**
10. Execute the program using F8 or the **Execute** button

abapGit should now start in your SAP GUI.

## Method 2: Offline Installation

If your SAP system doesn't have internet access, use this method:

1. On a computer with internet access, download the latest abapGit build:
   - Go to [https://github.com/abapGit/abapGit/releases](https://github.com/abapGit/abapGit/releases)
   - Download the latest `zabapgit.abap` file
2. Log in to your SAP system
3. Execute transaction code **SE38**
4. Enter program name `ZABAPGIT_FULL`
5. Click **Create**
6. In the program attributes dialog:
   - Enter a title (e.g., "abapGit client")
   - Select type "Executable program"
   - Click **Save**
7. When prompted, create the program in your local package ($TMP or a Z package)
8. Open the downloaded `zabapgit.abap` file on your computer
9. Copy the entire content and paste it into the SAP editor
10. Click **Save** and then **Activate**
11. Execute the program using F8 or the **Execute** button

## Post-Installation Steps

After successful installation:

1. Execute abapGit by running the program `ZABAPGIT_FULL` in SE38
2. You should see the abapGit main screen
3. Configure SSL/TLS settings if needed:
   - Go to **Settings** (gear icon)
   - Check the "Enable SSL verification" based on your system setup
   - Configure proxy settings if necessary
4. Create a desktop shortcut for convenience:
   - Go to **Settings**
   - Click **Create shortcut to abapGit**
   - Follow the prompts to create a system shortcut

## Troubleshooting

If you encounter issues during installation:

### SSL/TLS Certificate Issues

If you experience SSL/TLS handshake failures:

1. Go to transaction **STRUST**
2. Navigate to **SSL client > SSL Client (Standard)**
3. Import your organization's certificate authority or GitHub's certificate
4. Or disable SSL verification in abapGit settings (less secure)

### Authorization Issues

If you encounter authorization errors:

1. Ensure you have the necessary S_DEVELOP authorizations
2. Check with your SAP basis team for the correct profile assignment
3. Temporarily elevate your developer rights if needed

### System Compatibility

If you're using an older NetWeaver version (< 7.40):

1. You may need to install the compatibility version of abapGit
2. Check the abapGit documentation for specific system requirements
3. Consider upgrading your SAP system for better compatibility

## Creating a Transaction Code (Optional)

To create a dedicated transaction code for abapGit:

1. Execute transaction **SE93**
2. Enter a new transaction code (e.g., `ZABAPGIT`)
3. Select "Program and selection screen (parameter transaction)"
4. Enter `ZABAPGIT_FULL` as the program
5. Enter a short description
6. Save and assign to a package

## Setting up a Development Package for abapGit (Recommended)

For a more permanent installation, create a dedicated development package:

1. Execute transaction **SE80**
2. Create a new package (e.g., `ZABAPGIT`)
3. Move the `ZABAPGIT_FULL` program to this package
4. Consider installing abapGit as abapGit repository (see [Advanced Installation](https://docs.abapgit.org/))

## Updating abapGit

To update your abapGit installation:

1. Execute the `ZABAPGIT_FULL` program
2. Click the "Update abapGit" link at the top right
3. Confirm the update

If using offline installation, repeat the installation steps with the newer version of the code.

## ➡️ Next Steps

Once abapGit is installed, proceed to:

- [SAP Transport Layer Configuration](transport-layer.md)
- [GitHub Enterprise Authentication Setup](../github-setup/authentication.md)
- [SAP-GitHub Integration Setup](../../getting-started/installation.md)

## Additional Resources

- [Official abapGit Documentation](https://docs.abapgit.org/)
- [abapGit GitHub Repository](https://github.com/abapGit/abapGit)
- [SAP Community Wiki: abapGit](https://wiki.scn.sap.com/wiki/display/ABAP/abapGit)

---

*Last Updated: [YYYY-MM-DD] for [abapGit version X.Y.Z]* 
---


