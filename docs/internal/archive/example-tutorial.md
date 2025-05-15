# Installing abapGit in SAP System

## Introduction

This tutorial guides you through the process of installing abapGit in your SAP system, which is the first technical step in establishing the SAP-GitHub integration. abapGit is an open-source git client for ABAP that enables version control for your SAP development objects.

## Prerequisites

Before beginning this tutorial, ensure you have:

- SAP NetWeaver 7.40 or higher (7.50+ recommended)
- Developer authorization in your SAP system
- SAP_BASIS component with appropriate support package level
- Required SAP Notes installed (listed in system requirements)
- Developer access to transaction SE38 or SE80
- Authorization for Z* development objects

## Installation Overview

[DIAGRAM: abapGit installation process showing SAP system, developer workstation, and GitHub connections]

The installation process follows these key steps:
1. Installing the standalone version of abapGit
2. Creating a package for abapGit
3. Installing the developer version of abapGit
4. Configuring SSL certificates for GitHub connectivity
5. Verifying the installation

## Step-by-Step Installation

### 1. Install Standalone Version

1. Open SAP GUI and connect to your SAP system
2. Launch transaction **SE38** (ABAP Editor)
3. Enter program name `ZABAPGIT_STANDALONE` and click **Create**

   [SCREENSHOT: SE38 transaction with program name input field]

4. In the program attributes dialog:
   - Set program type to **Executable Program**
   - Enter a description like "abapGit Standalone"
   - Click **Save**
   
   [SCREENSHOT: Program attributes dialog]

5. Select appropriate package (local object is fine for initial installation)
6. Copy the latest standalone abapGit code from the [official repository](https://raw.githubusercontent.com/abapGit/build/main/zabapgit.abap)
7. Paste the code into the editor

   [SCREENSHOT: ABAP Editor with abapGit code]

8. Click **Save** and then **Activate** (or use keyboard shortcuts Ctrl+S, Ctrl+F3)

### 2. Create a Package for abapGit

After successfully installing the standalone version, you'll need to create a proper package for the developer version of abapGit:

9. Execute transaction **SE80**
10. Select **Package** from the dropdown menu
11. Enter `$ABAPGIT` as the package name (or use your own naming convention)
12. Click **Create**

   [SCREENSHOT: SE80 package creation screen]

13. In the package properties dialog:
    - Enter a meaningful description: "abapGit Development Package"
    - Set package type to "Development"
    - Assign to an appropriate transport request
    - Click **Save**

### 3. Install Developer Version of abapGit

Now that you have created the package to hold the abapGit objects, you can proceed with installing the full developer version:

14. Execute the standalone version by running transaction **SE38** and executing program `ZABAPGIT_STANDALONE`

    [SCREENSHOT: abapGit standalone initial screen]

15. In the abapGit interface, click on "New Online"
16. Enter the following details:
    - Git Repository URL: `https://github.com/abapGit/abapGit.git`
    - Package: `$ABAPGIT` (the package you created in steps 9-13)
    - Branch: main (default)
    - Click **Create Online Repo**

    [SCREENSHOT: abapGit repository creation screen]

17. abapGit will show the repository overview with available objects
18. Click **Pull** to download and install all objects

    [SCREENSHOT: abapGit repository with pull button highlighted]

19. Confirm any dialogs regarding package assignment or transport requests
20. Wait for the installation to complete

### 4. Configure SSL Certificate for GitHub

With abapGit now installed, you need to set up the SSL certificate to enable secure communication with GitHub:

21. Execute transaction **STRUST**
22. Navigate to "SSL client SSL Client (Standard)"
23. Double-click to open the certificate manager

    [SCREENSHOT: STRUST transaction initial screen]

24. Click on the **Import Certificate** button
25. In the dialog:
    - Enter `github.com` in the "PSE Object name" field
    - Set options to "Binary" and "Add to certificate list"
    - Click **Continue**

    [SCREENSHOT: Certificate import dialog]

26. Navigate to GitHub in your browser and download the SSL certificate:
    - In most browsers, click the lock icon in the address bar
    - View certificate details
    - Export or save the certificate to your local machine
   
27. In STRUST, import the downloaded certificate
28. Save the changes

### 5. Verify The Installation

After completing the SSL certificate configuration, let's verify that your abapGit installation is working correctly:

29. Create a new program to validate the installation:
    - Execute transaction **SE38**
    - Create a new program named `Z_ABAPGIT_TEST`
    - Add the following code:

    ```abap
    REPORT z_abapgit_test.
    
    TRY.
        NEW zcl_abapgit_repo_online( ).
        WRITE: / 'abapGit installed successfully!'.
      CATCH cx_root INTO DATA(lx_error).
        WRITE: / 'Error:', lx_error->get_text( ).
    ENDTRY.
    ```

30. Execute the program
31. Verify that it displays "abapGit installed successfully!"

    [SCREENSHOT: Successful execution of test program]

## Common Issues and Solutions

### SSL Certificate Problems

**Issue**: "SSL handshake failed" or "SSL connection failed" errors

**Solution**:
1. Ensure you've correctly imported the GitHub certificate in STRUST (steps 21-28)
2. Verify your SAP system has the required support packages for SSL
3. Check if your network allows HTTPS connections to GitHub

### Authorization Issues

**Issue**: "Not authorized" errors during installation

**Solution**:
1. Ensure your user has developer authorization
2. Check SAP_BASIS authorization for required objects
3. Consult with your SAP Basis team for specific authorizations

### Code Activation Errors

**Issue**: Syntax errors or activation problems

**Solution**:
1. Ensure you're using the latest abapGit version
2. Verify your SAP system meets the minimum version requirements (SAP NetWeaver 7.40+)
3. Check for any missing dependent objects

## Next Steps

Now that you have successfully installed abapGit in your SAP system, you're ready to proceed to the next phase of the integration setup:

1. **[Configuring abapGit for Your Project](../3-setup-guide/configure-abapgit.md)** - In this next tutorial, you'll set up your first ABAP project for GitHub integration, building directly on the abapGit installation you just completed.

2. After configuring your ABAP project, you'll need to **[Set Up GitHub Repository](../documentation/guides/github-setup/repository-setup.md)** to store your code.

3. Finally, you'll establish connectivity between both systems in **[Configuring API Management](../documentation/guides/sap-setup/api-setup.md)**.

By completing this tutorial, you've taken the first step toward integrating your SAP development with GitHub. The abapGit installation will serve as the foundation for all subsequent steps in the integration process.