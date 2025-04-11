# Tutorial 3: Implementing Development Workflow

## Introduction

In [Tutorial 1](../3-setup-guide/installing-abapgit.md), you installed abapGit in your SAP system, and in [Tutorial 2](../3-setup-guide/configure-abapgit.md), you configured abapGit for your specific ABAP project. Now that you have successfully connected your SAP system to GitHub (Steps 1-65), it's time to implement a proper development workflow that leverages this integration to improve your development process.

## Prerequisites

Before beginning this tutorial, ensure you have:

- Completed all steps in [Tutorial 1: Installing abapGit in SAP System](../3-setup-guide/installing-abapgit.md) (Steps 1-31)
- Completed all steps in [Tutorial 2: Configuring abapGit for Your Project](../3-setup-guide/configure-abapgit.md) (Steps 32-65)
- Verified your GitHub integration is working correctly (Steps 62-65 of Tutorial 2)
- Basic understanding of Git concepts (branches, commits, pull requests)
- Developer authorization in your SAP system

## Workflow Overview

![SAP-GitHub Development Workflow](../../assets/images/workflows/development-workflow.svg)

This tutorial covers the implementation of a standard Git-based development workflow adapted for SAP development:

1. Creating feature branches
2. Making changes in SAP
3. Committing and pushing to GitHub
4. Creating pull requests
5. Reviewing and merging code
6. Pulling changes back to SAP

## Step-by-Step Implementation

### Step 12: Create a Feature Branch

Continuing from Tutorial 2 where you verified your GitHub integration in Steps 62-65, you'll now implement a proper branching strategy:

66. Open abapGit by running transaction **SE38** and executing program `ZABAPGIT`
67. Navigate to your repository (the one you created in Steps 43-46 of Tutorial 2)
68. Click on the branch icon in the abapGit toolbar (usually labeled "main")

    [SCREENSHOT: abapGit repository screen showing the main interface with the branch button highlighted in red circle in the toolbar]

69. In the branch dialog, click **Create New Branch**
70. Enter a feature branch name using a descriptive convention: `feature/add-customer-list`
71. Select "Create from: main" to base your new branch on the main branch
72. Click **Create**

    [SCREENSHOT: abapGit create branch dialog with branch name field filled and creation options]

73. Verify that abapGit indicates you're now working on the new branch

### Step 13: Implement a Feature in SAP

With your feature branch created in Steps 66-73, you'll now implement a new feature in SAP:

74. Return to transaction **SE80**
75. Navigate to your package `Z_GITHUB_DEMO` (created in Steps 32-35 of Tutorial 2)
76. Right-click on the package and select **Create > Program**
77. Enter a program name (e.g., `Z_CUSTOMER_LIST`)
78. Add the following code:

    ```abap
    REPORT z_customer_list.
    
    TABLES: kna1. " Customer master data table
    
    PARAMETERS: p_limit TYPE i DEFAULT 10.
    
    START-OF-SELECTION.
      " Select customer data
      SELECT kunnr name1 land1 ort01
        FROM kna1
        INTO TABLE @DATA(lt_customers)
        UP TO @p_limit ROWS.
    
      " Display results
      WRITE: / 'Customer List', / '============'.
      LOOP AT lt_customers INTO DATA(ls_customer).
        WRITE: / 'ID:', ls_customer-kunnr, 
               'Name:', ls_customer-name1,
               'Country:', ls_customer-land1,
               'City:', ls_customer-ort01.
      ENDLOOP.
    ```

79. Save and activate the program

    [SCREENSHOT: SE80 editor showing the customer list program code with syntax highlighting]

### Step 14: Commit and Push the Changes

After implementing your feature in Steps 74-79, you'll commit and push it to your feature branch:

80. Return to abapGit by running transaction **SE38** and executing program `ZABAPGIT`
81. Navigate to your repository and verify that you're still on the `feature/add-customer-list` branch
82. You should see your program `Z_CUSTOMER_LIST` with a new (non-staged) status
83. Check the box next to your program to stage it
84. Click the **Stage** button

    [SCREENSHOT: abapGit stage screen showing the new program selected with checkbox and Stage button highlighted]

85. Enter a descriptive commit message: "Add customer list program for data reporting"
86. Click **Commit**

    [SCREENSHOT: abapGit commit screen with message field filled and Commit button highlighted]

87. When prompted, enter your GitHub credentials
88. Click **Execute** to complete the push
89. Verify in abapGit that the push was successful

### Step 15: Create a Pull Request

With your changes committed to the feature branch in Steps 80-89, it's time to create a pull request for code review:

90. Open your web browser and navigate to your GitHub repository
91. GitHub should show a notification about your recently pushed branch
92. Click on the **Compare & pull request** button

    [SCREENSHOT: GitHub repository page showing the compare & pull request notification banner for the newly pushed branch]

93. Enter a pull request title: "Add customer list program"
94. In the description, provide details about your changes:
   ```
   This pull request adds a new program to display customer information.
   
   The program allows:
   - Viewing customer master data
   - Limiting the number of records displayed
   - Displaying key information including ID, name, country, and city
   ```
95. Click **Create pull request**

    [SCREENSHOT: GitHub create pull request screen with title and description filled in and Create pull request button highlighted]

### Step 16: Review and Merge the Pull Request

In a real-world scenario, another team member would review your changes. For this tutorial, you'll review and merge your own pull request:

96. In GitHub, open the pull request you created in Steps 90-95
97. Click on the "Files changed" tab to see the code changes
98. Review the code changes, ensuring everything looks correct
99. Add a comment to simulate a code review: "Code looks good. The field selection is appropriate for the report requirements."
100. Click the **Merge pull request** button
101. Click **Confirm merge**

     [SCREENSHOT: GitHub merged pull request screen showing successful merge message and delete branch option]

102. Verify that GitHub shows the pull request as merged
103. Click on the **Delete branch** button to clean up the feature branch (optional)

### Step 17: Pull Changes to SAP Main Branch

Now that your changes are merged into the main branch on GitHub in Steps 96-103, you need to update your SAP system:

104. Return to abapGit by running transaction **SE38** and executing program `ZABAPGIT`
105. Navigate to your repository
106. Switch back to the main branch by clicking on the branch icon and selecting "main"

     [SCREENSHOT: abapGit branch selection dialog with "main" branch highlighted]

107. Click the **Pull** button to fetch the latest changes from GitHub
108. abapGit will display the changes from your merged pull request
109. Confirm the pull operation

     [SCREENSHOT: abapGit pull confirmation dialog showing affected objects]

110. Verify that your `Z_CUSTOMER_LIST` program is now part of the main branch in your SAP system

## Verification Checkpoint

Before concluding this tutorial, verify that:

- Your feature branch was successfully created in GitHub
- The new program was properly pushed to GitHub
- The pull request was created, reviewed, and merged
- Changes were successfully pulled back to the main branch in SAP
- The program runs correctly in your SAP system

## Common Issues and Solutions

### Branch Switching Issues

**Issue**: "Local objects modified" error when switching branches

**Solution**:
1. Commit or revert any local changes before switching branches
2. Check if transport requests are open for modified objects
3. Release or reassign transports if necessary

### Merge Conflicts

**Issue**: Conflicts between your changes and the main branch

**Solution**:
1. Pull the latest changes from the main branch before creating your feature branch
2. If conflicts occur, resolve them in abapGit's merge tool
3. For complex conflicts, consider manually resolving in SAP before committing

### Transport Request Management

**Issue**: Objects locked in transport requests

**Solution**:
1. Ensure objects are assigned to appropriate transport requests
2. Release completed transport requests before switching branches
3. Consider implementing a transport request strategy aligned with your branching strategy

## Next Steps

Now that you have successfully implemented a development workflow in Steps 66-110, you're ready to proceed to the next phase of the integration:

- **[Tutorial 4: Implementing Continuous Integration](./continuous-integration.md)** - In this next tutorial, you'll set up automated testing and validation for your SAP developments using GitHub Actions.

In Tutorial 4, you'll learn how to implement automated testing, code quality checks, and validation processes to ensure your SAP developments meet quality standards before being deployed. This builds on the workflow established in this tutorial, adding automation to improve quality and efficiency. 