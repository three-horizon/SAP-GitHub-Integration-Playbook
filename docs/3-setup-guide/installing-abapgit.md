# Tutorial 1: Installing abapGit in SAP System

## Introduction

This tutorial guides you through the process of installing abapGit in your SAP system, which is the first technical step in establishing the SAP-GitHub integration. abapGit is an open-source git client for ABAP that enables version control for your SAP development objects. Following this tutorial sets the foundation for all subsequent integration steps.

## Prerequisites

Before beginning this tutorial, ensure you have:

- SAP NetWeaver 7.40 or higher (7.50+ recommended)
- Developer authorization in your SAP system
- SAP_BASIS component with appropriate support package level
- Required SAP Notes installed (listed in system requirements)
- Developer access to transaction SE38 or SE80
- Authorization for Z* development objects

## Installation Overview

[DIAGRAM: abapGit installation process showing:
1. Developer workstation with SAP GUI connected to SAP system
2. Installation of standalone version in SAP system
3. Creation of abapGit package
4. Installation of developer version from GitHub
5. SSL certificate configuration for secure connection
6. Verification and testing
Arrows showing sequential steps and connections between components]

The installation process follows these key steps:

## Step-by-Step Installation

### Step 1: Install Standalone Version

1. Open SAP GUI and connect to your SAP system
2. Launch transaction **SE38** (ABAP Editor)
3. Enter program name `ZABAPGIT_STANDALONE` and click **Create**

4. In the program attributes dialog:
   - Set program type to **Executable Program**
   - Enter a description like "abapGit Standalone"
   - Click **Save**
   
5. Select appropriate package (local object is fine for initial installation)
6. Copy the latest standalone abapGit code from the [official repository](https://raw.githubusercontent.com/abapGit/build/main/zabapgit.abap)
7. Paste the code into the editor

8. Click **Save** and then **Activate** (or use keyboard shortcuts Ctrl+S, Ctrl+F3)

### Step 2: Create a Package for abapGit

After successfully installing the standalone version in Steps 1-8, you'll need to create a proper package for the developer version of abapGit:

9. Execute transaction **SE80**
10. Select **Package** from the dropdown menu
11. Enter `$ABAPGIT` as the package name (or use your own naming convention)
12. Click **Create**

13. In the package properties dialog:
    - Enter a meaningful description: "abapGit Development Package"
    - Set package type to "Development"
    - Assign to an appropriate transport request
    - Click **Save**

### Step 3: Install Developer Version of abapGit

Now that you have created the package to hold the abapGit objects in Steps 9-13, you can proceed with installing the full developer version:

14. Execute the standalone version by running transaction **SE38** and executing program `ZABAPGIT_STANDALONE` that you created in Steps 1-8

15. In the abapGit interface, click on "New Online"
16. Enter the following details:
    - Git Repository URL: `https://github.com/abapGit/abapGit.git`
    - Package: `$ABAPGIT` (the package you created in Steps 9-13)
    - Branch: main (default)
    - Click **Create Online Repo**

17. abapGit will show the repository overview with available objects
18. Click **Pull** to download and install all objects

19. Confirm any dialogs regarding package assignment or transport requests
20. Wait for the installation to complete

### Step 4: Configure SSL Certificate for GitHub

With abapGit now installed in Steps 14-20, you need to set up the SSL certificate to enable secure communication with GitHub:

21. Execute transaction **STRUST**
22. Navigate to "SSL client SSL Client (Standard)"
23. Double-click to open the certificate manager

24. Click on the **Import Certificate** button
25. In the dialog:
    - Enter `github.com` in the "PSE Object name" field
    - Set options to "Binary" and "Add to certificate list"
    - Click **Continue**

26. Navigate to GitHub in your browser and download the SSL certificate:
    - In most browsers, click the lock icon in the address bar
    - View certificate details
    - Export or save the certificate to your local machine
   
27. In STRUST, import the downloaded certificate
28. Save the changes

### Step 5: Verify The Installation

After completing the SSL certificate configuration in Steps 21-28, let's verify that your abapGit installation is working correctly:

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

## Verification Checkpoint

Before proceeding to the next tutorial, verify that:

- You can launch the abapGit program without errors
- You can see the abapGit repository you created
- The test program shows a successful connection
- You don't receive any SSL certificate errors

## Common Issues and Solutions

### SSL Certificate Problems

**Issue**: "SSL handshake failed" or "SSL connection failed" errors

**Solution**:
1. Ensure you've correctly imported the GitHub certificate in STRUST (Steps 21-28)
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

Now that you have successfully installed abapGit in your SAP system in Steps 1-31, you're ready to proceed to the next phase of the integration setup:

- **[Tutorial 2: Configuring abapGit for Your Project](./configure-abapgit.md)** - In this next tutorial, you'll configure abapGit for your specific ABAP project, building directly on the installation you just completed.

In Tutorial 2, you'll learn how to create your first ABAP project repository and establish the connection between your SAP development objects and GitHub, utilizing the abapGit installation you've just completed. 