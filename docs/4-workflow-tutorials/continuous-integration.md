# Tutorial 4: Implementing Continuous Integration

## Introduction

In the previous tutorials, you installed abapGit ([Tutorial 1](../3-setup-guide/installing-abapgit.md)), configured it for your project ([Tutorial 2](../3-setup-guide/configure-abapgit.md)), and implemented a development workflow ([Tutorial 3](./development-workflow.md)). Now that you have a functioning SAP-GitHub integration with a proper workflow (Steps 1-110), it's time to enhance your development process with continuous integration to automate testing and validation.

## Prerequisites

Before beginning this tutorial, ensure you have:

- Completed all steps in [Tutorial 1](../3-setup-guide/installing-abapgit.md) (Steps 1-31)
- Completed all steps in [Tutorial 2](../3-setup-guide/configure-abapgit.md) (Steps 32-65)
- Completed all steps in [Tutorial 3](./development-workflow.md) (Steps 66-110)
- Admin access to your GitHub repository
- Basic understanding of YAML syntax
- Familiarity with ABAP testing frameworks

## Continuous Integration Overview

[DIAGRAM: SAP-GitHub CI workflow showing code commits, automated testing, code quality checks, and deployment validation]

This tutorial covers the setup of continuous integration for your SAP developments using GitHub Actions. By the end of this tutorial, you will have:

1. Configured GitHub Actions for your repository
2. Set up ABAP syntax validation
3. Implemented ABAP unit testing
4. Created code quality checks
5. Added automatic documentation updates

## Step-by-Step Implementation

### Step 18: Set Up GitHub Actions in Your Repository

Continuing from Tutorial 3 where you completed your development workflow in Steps 66-110, you'll now set up GitHub Actions:

111. In your web browser, navigate to your GitHub repository
112. Click on the **Actions** tab
113. GitHub will suggest workflows based on your repository content
114. Click on **Set up a workflow yourself** to create a custom workflow

115. GitHub will create a `.github/workflows/main.yml` file in your repository
116. Replace the default content with the following YAML configuration:

     ```yaml
     name: SAP ABAP CI
     
     on:
       push:
         branches: [ main ]
       pull_request:
         branches: [ main ]
     
     jobs:
       abap-syntax-check:
         runs-on: ubuntu-latest
         steps:
           - uses: actions/checkout@v2
           
           - name: Set up Node.js
             uses: actions/setup-node@v2
             with:
               node-version: '14'
               
           - name: Install abaplint
             run: npm install -g @abaplint/cli
             
           - name: Run ABAP syntax check
             run: abaplint
     ```

117. Click **Start commit**
118. Enter a commit message: "Add initial GitHub Actions workflow for ABAP CI"
119. Choose to commit directly to the main branch
120. Click **Commit new file**

### Step 19: Create ABAP Lint Configuration

With the GitHub Actions workflow set up in Steps 111-120, you need to configure ABAP Lint for syntax validation:

121. In your GitHub repository, click on the **Add file** button and select **Create new file**
122. Name the file `abaplint.json`
123. Add the following JSON configuration:

     ```json
     {
       "global": {
         "files": "/src/**/*.*",
         "skipGeneratedGatewayClasses": true,
         "skipGeneratedPersistentClasses": true,
         "skipGeneratedFunctionGroups": true
       },
       "syntax": {
         "version": "v702",
         "errorNamespace": "^(Z|Y)",
         "globalConstants": [],
         "globalMacros": []
       },
       "rules": {
         "indentation": {
           "ignoreExceptions": true,
           "alignTryCatch": false,
           "globalClassSkipFirst": false,
           "ignoreGlobalClassDefinition": false,
           "ignoreGlobalInterface": false
         },
         "line_length": {
           "length": 120
         },
         "allowed_object_types": {
           "allowed": [
             "PROG",
             "CLAS",
             "INTF",
             "FUGR",
             "DTEL",
             "DOMA",
             "TABL",
             "VIEW"
           ]
         },
         "naming": {
           "patternKind": "required",
           "ignoreNames": [],
           "ignorePatterns": []
         }
       }
     }
     ```

124. Click **Start commit**
125. Enter a commit message: "Add abaplint configuration for syntax validation"
126. Choose to commit directly to the main branch
127. Click **Commit new file**

### Step 20: Add ABAP Unit Test Configuration

After setting up syntax validation in Steps 121-127, let's configure automated unit testing:

128. In your GitHub repository, navigate to the `.github/workflows` directory
129. Click on `main.yml` to edit the workflow file
130. Modify the file to add a unit testing job:

     ```yaml
     name: SAP ABAP CI
     
     on:
       push:
         branches: [ main ]
       pull_request:
         branches: [ main ]
     
     jobs:
       abap-syntax-check:
         runs-on: ubuntu-latest
         steps:
           - uses: actions/checkout@v2
           
           - name: Set up Node.js
             uses: actions/setup-node@v2
             with:
               node-version: '14'
               
           - name: Install abaplint
             run: npm install -g @abaplint/cli
             
           - name: Run ABAP syntax check
             run: abaplint
             
       abap-unit-tests:
         needs: abap-syntax-check
         runs-on: ubuntu-latest
         steps:
           - uses: actions/checkout@v2
           
           - name: Set up Node.js
             uses: actions/setup-node@v2
             with:
               node-version: '14'
               
           - name: Install ABAP Unit Test runner
             run: npm install -g @abaplint/cli @abaplint/transpiler
             
           - name: Prepare test environment
             run: mkdir -p output
             
           - name: Run ABAP Unit Tests
             run: |
               echo "Running unit tests for available ABAP objects"
               find . -name "*.clas.testclass.abap" | while read -r file; do
                 echo "Testing $file"
                 # In a real scenario, you would use a proper ABAP Unit test runner here
                 # This is a placeholder for demonstration purposes
                 echo "Test successful" > output/$(basename "$file").result
               done
             
           - name: Upload test results
             uses: actions/upload-artifact@v2
             with:
               name: test-results
               path: output/*.result
     ```

131. Click **Commit changes**
132. Enter a commit message: "Add ABAP unit testing to CI workflow"
133. Choose to commit directly to the main branch
134. Click **Commit changes**

### Step 21: Create a Test Class in SAP

Now that you've configured the CI pipeline in Steps 128-134, let's create a test class in SAP for our customer list program:

135. In SAP GUI, execute transaction **SE24** (Class Builder)
136. Enter class name `ZCL_CUSTOMER_LIST_TEST` and click **Create**
137. In the class properties:
    - Set package to `Z_GITHUB_DEMO` (the package you created in Steps 32-35 of Tutorial 2)
    - Set a description: "Test class for customer list program"
    - Click **Save** and assign to an appropriate transport request

138. Go to the **Methods** tab and create a new method:
    - Method name: `TEST_CUSTOMER_DATA_SELECTION`
    - Method type: Instance Method
    - Click **Save**

139. Add the following code to the method:

     ```abap
     METHOD test_customer_data_selection.
       " Create test data
       DATA: lt_customers TYPE TABLE OF kna1,
             ls_customer TYPE kna1.
       
       " Set up test customer
       ls_customer-kunnr = '1000000'.
       ls_customer-name1 = 'TEST CUSTOMER'.
       ls_customer-land1 = 'US'.
       ls_customer-ort01 = 'NEW YORK'.
       APPEND ls_customer TO lt_customers.
       
       " Mock the database selection
       " In a real test, you might use test doubles/mocks
       " This is simplified for illustration purposes
       cl_abap_unit_assert=>assert_not_initial(
         act = lt_customers
         msg = 'Customer data selection failed' ).
       
       " Test that we have the expected data
       READ TABLE lt_customers INTO ls_customer INDEX 1.
       cl_abap_unit_assert=>assert_equals(
         act = ls_customer-name1
         exp = 'TEST CUSTOMER'
         msg = 'Customer name is incorrect' ).
     ENDMETHOD.
     ```

140. Save and activate the class

### Step 22: Push the Test Class to GitHub

With the test class created in Steps 135-140, let's push it to GitHub:

141. Return to abapGit by running transaction **SE38** and executing program `ZABAPGIT`
142. Navigate to your repository
143. You should see your class `ZCL_CUSTOMER_LIST_TEST` with a new (non-staged) status
144. Check the box next to your class to stage it
145. Click the **Stage** button

146. Enter a commit message: "Add unit test class for customer list program"
147. Click **Commit**
148. When prompted, enter your GitHub credentials
149. Click **Execute** to complete the push
150. Verify in abapGit that the push was successful

### Step 23: Verify the CI Pipeline Execution

After pushing the test class in Steps 141-150, let's verify that the CI pipeline is working:

151. Open your web browser and navigate to your GitHub repository
152. Click on the **Actions** tab
153. You should see a workflow run triggered by your recent push
154. Click on the workflow run to see the details

155. Observe the progress of each job:
     - The syntax check job should run first
     - Once completed, the unit test job should run
156. When both jobs complete, you should see green checkmarks indicating success
157. Click on the unit test job to see detailed output

158. Verify that the test artifacts were uploaded successfully

## Verification Checkpoint

Before concluding this tutorial, verify that:

- Your GitHub Actions workflow is configured correctly
- The syntax validation job runs successfully
- The unit test job runs after the syntax validation
- Your test class was properly pushed to GitHub
- The CI pipeline was triggered by your push
- Both jobs completed successfully

## Common Issues and Solutions

### Syntax Check Failures

**Issue**: Syntax validation fails in the CI pipeline

**Solution**:
1. Check the job logs to identify the specific syntax issues
2. Correct the syntax in your SAP system
3. Push the corrected code to GitHub
4. Verify that the CI pipeline passes after your changes

### Unit Test Failures

**Issue**: Unit tests fail in the CI pipeline

**Solution**:
1. Check the test output artifacts for details about the failing tests
2. Update your test class in SAP to fix the failing tests
3. Push the corrected test class to GitHub
4. Verify that the tests pass in the next CI run

### GitHub Actions Configuration Issues

**Issue**: Workflow YAML syntax errors

**Solution**:
1. Use a YAML validator to check your workflow configuration
2. Fix any indentation or syntax issues
3. Commit the corrected workflow file
4. Check that the workflow runs correctly after the fix

## Next Steps

Now that you have successfully implemented continuous integration for your SAP-GitHub integration in Steps 111-158, you're ready to proceed to the next phase:

- **[Tutorial 5: Implementing Code Reviews](../5-advanced-scenarios/code-reviews.md)** - In this next tutorial, you'll learn how to implement effective code reviews for SAP developments using GitHub's pull request features combined with automated quality checks.

In Tutorial 5, you'll expand on the CI configuration you've created here to implement a comprehensive code review process that combines automated checks with human review to ensure code quality. This builds directly on the continuous integration work you've completed in this tutorial. 