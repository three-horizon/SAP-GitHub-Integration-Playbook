# Tutorial 2: Configuring abapGit for Your Project

## Introduction

In [Tutorial 1](./installing-abapgit.md), you installed abapGit in your SAP system. Now, we'll configure abapGit for your specific ABAP project to enable version control with GitHub. This tutorial builds directly on the abapGit installation you completed in Steps 1-31 of the previous tutorial.

## Prerequisites

Before beginning this tutorial, ensure you have:

- Completed all steps in [Tutorial 1: Installing abapGit in SAP System](./installing-abapgit.md)
- Verified abapGit is working correctly (Step 31 of Tutorial 1)
- Authorization to create and modify ABAP packages
- An existing ABAP development project or the authority to create one
- Your GitHub account credentials ready

## Configuration Overview

[DIAGRAM: abapGit project configuration workflow showing connections between SAP objects, abapGit, and GitHub repository]

## Step-by-Step Configuration

### Step 6: Create an ABAP Package for Your Project

Continuing from Tutorial 1 where you verified the abapGit installation in Steps 29-31, you'll now create a dedicated package for your SAP development objects:

32. Execute transaction **SE80**
33. Select **Package** from the dropdown menu
34. Enter a suitable package name (e.g., `Z_GITHUB_DEMO`) and click **Create**

    [SCREENSHOT: SE80 package creation screen with Z_GITHUB_DEMO entered]

35. In the package properties dialog:
    - Enter a meaningful description: "Demo Package for GitHub Integration"
    - Set package type to "Development"
    - Assign to an appropriate transport request
    - Click **Save**

### Step 7: Set Up GitHub Repository

Now that you have your ABAP package (created in Steps 32-35), you need to create a corresponding GitHub repository:

36. Log in to your GitHub account
37. Click the "+" icon in the top-right corner and select "New repository"
38. Enter a repository name that corresponds to your ABAP package (e.g., `sap-github-demo`)
39. Add a description that references your SAP project
40. Choose "Public" or "Private" visibility based on your requirements
41. Initialize the repository with a README file
42. Click **Create repository**

    [SCREENSHOT: GitHub create repository screen with settings highlighted]

### Step 8: Create abapGit Repository for Your Project

With both your ABAP package (Steps 32-35) and GitHub repository (Steps 36-42) ready, you'll now connect them using abapGit:

43. Execute the abapGit program by running transaction **SE38** and executing program `ZABAPGIT` that you installed in Tutorial 1

    [SCREENSHOT: abapGit main screen]

44. Click on the "+" icon (New Online)
45. Enter the following details:
    - Git Repository URL: `https://github.com/YOUR-USERNAME/sap-github-demo.git` (the repository created in Steps 36-42)
    - Package: `Z_GITHUB_DEMO` (the package you created in Steps 32-35)
    - Branch: main (default)
    - Click **Create Online Repo**

    [SCREENSHOT: abapGit repository creation screen with fields filled]

46. abapGit will show your new repository connection with no objects

### Step 9: Create a Sample ABAP Program

Now that you've established the connection between your ABAP package and GitHub repository, let's create a sample ABAP program to demonstrate the workflow:

47. Return to transaction **SE80**
48. Navigate to your package `Z_GITHUB_DEMO` created in Steps 32-35
49. Right-click on the package and select **Create > Program**
50. Enter a program name (e.g., `Z_GITHUB_HELLO_WORLD`)
51. Add the following simple code:

    ```abap
    REPORT z_github_hello_world.
    
    PARAMETERS: p_name TYPE string DEFAULT 'World'.
    
    START-OF-SELECTION.
      WRITE: / 'Hello,', p_name, '!'.
      WRITE: / 'This program is version controlled with GitHub.'.
    ```

52. Save and activate the program

    [SCREENSHOT: SE80 with the created program]

### Step 10: Push Your Code to GitHub

Now that you have created a sample program in Steps 47-52, you'll push it to your GitHub repository:

53. Return to abapGit by running transaction **SE38** and executing program `ZABAPGIT`
54. Navigate to your repository (created in Steps 43-46)
55. You should see your program `Z_GITHUB_HELLO_WORLD` with a new (non-staged) status
56. Check the box next to your program to stage it
57. Click the **Stage** button

    [SCREENSHOT: abapGit stage screen with program selected]

58. Enter a commit message that describes your changes (e.g., "Add hello world program")
59. Click **Commit**

    [SCREENSHOT: abapGit commit screen with message entered]

60. When prompted, enter your GitHub username and password or personal access token
61. Click **Execute** to complete the push

### Step 11: Verify the GitHub Integration

After pushing your code in Steps 53-61, let's verify that everything is working correctly:

62. Open your web browser and navigate to your GitHub repository
63. Verify that your program appears in the repository
64. Click on the program file to view its content
65. Check that the commit message is displayed in the repository history

    [SCREENSHOT: GitHub repository showing the pushed ABAP program]

## Verification Checkpoint

Before proceeding to the next tutorial, verify that:

- Your ABAP package is properly linked to your GitHub repository
- You can successfully push changes from SAP to GitHub
- The program content appears correctly in GitHub
- The commit history shows your changes

## Common Issues and Solutions

### Authentication Problems

**Issue**: "Authentication failed" when pushing to GitHub

**Solution**:
1. GitHub may require a personal access token instead of your password
2. Generate a new token in GitHub (Settings > Developer settings > Personal access tokens)
3. Ensure the token has the appropriate repository permissions
4. Use this token instead of your password when prompted by abapGit

### Package Not Found

**Issue**: "Package not found" error when creating repository

**Solution**:
1. Ensure the package you specified in Step 34 exists and is correctly named
2. Check that you have authorization to use this package
3. Try creating the package again if necessary

### Connection Issues

**Issue**: Timeout or connection errors when pushing to GitHub

**Solution**:
1. Check that the SSL certificate was properly imported in Tutorial 1 (Steps 21-28)
2. Verify your network connectivity to GitHub
3. Ensure your proxy settings are correctly configured if you're behind a corporate firewall

## Next Steps

Now that you have successfully configured abapGit for your project in Steps 32-65, you're ready to proceed to the next phase of the integration setup:

- **[Tutorial 3: Implementing Development Workflow](../4-workflow-tutorials/development-workflow.md)** - In this next tutorial, you'll learn how to implement a proper development workflow using the integration you've just configured.

In Tutorial 3, you'll learn about branching strategies, collaborative development, and how to manage changes between SAP and GitHub using the foundation you've built in Tutorials 1 and 2. 