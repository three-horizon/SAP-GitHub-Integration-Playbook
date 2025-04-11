# Tutorial 5: Implementing Code Reviews

## Introduction

In the previous tutorials, you installed abapGit ([Tutorial 1](../3-setup-guide/installing-abapgit.md)), configured it for your project ([Tutorial 2](../3-setup-guide/configure-abapgit.md)), implemented a development workflow ([Tutorial 3](../4-workflow-tutorials/development-workflow.md)), and set up continuous integration ([Tutorial 4](../4-workflow-tutorials/continuous-integration.md)). Now that you have a functioning CI pipeline (Steps 111-158), it's time to implement a structured code review process to further improve code quality.

## Prerequisites

Before beginning this tutorial, ensure you have:

- Completed all steps in [Tutorial 1](../3-setup-guide/installing-abapgit.md) (Steps 1-31)
- Completed all steps in [Tutorial 2](../3-setup-guide/configure-abapgit.md) (Steps 32-65)
- Completed all steps in [Tutorial 3](../4-workflow-tutorials/development-workflow.md) (Steps 66-110)
- Completed all steps in [Tutorial 4](../4-workflow-tutorials/continuous-integration.md) (Steps 111-158)
- Admin access to your GitHub repository
- Team members who will participate in the code review process

## Code Review Overview

[DIAGRAM: SAP-GitHub code review process showing code submission, automated checks, manual review, and approval workflow]

This tutorial covers the implementation of a structured code review process for your SAP developments using GitHub's pull request features. By the end of this tutorial, you will have:

1. Configured branch protection rules
2. Set up pull request templates
3. Implemented code review assignments
4. Established code review guidelines
5. Created review automation with checks

## Step-by-Step Implementation

### Step 24: Configure Branch Protection Rules

Continuing from Tutorial 4 where you completed setting up the CI pipeline in Steps 111-158, you'll now set up branch protection rules:

159. In your web browser, navigate to your GitHub repository
160. Click on the **Settings** tab
161. In the left sidebar, click on **Branches**
162. In the "Branch protection rules" section, click **Add rule**

     [SCREENSHOT: GitHub branch protection rule creation screen]

163. In the "Branch name pattern" field, enter `main`
164. Select the following options:
     - ✓ Require pull request reviews before merging
     - ✓ Require status checks to pass before merging
     - ✓ Require branches to be up to date before merging
     - ✓ Include administrators
     
165. Under "Require status checks to pass before merging", search for and select:
     - ✓ abap-syntax-check
     - ✓ abap-unit-tests
     
166. Under "Require pull request reviews before merging", set:
     - Required approving reviews: 1 (adjust based on team size)
     - ✓ Dismiss stale pull request approvals when new commits are pushed
     - ✓ Require review from Code Owners
     
167. Click **Create** to save the branch protection rule

     [SCREENSHOT: Completed branch protection rule configuration]

### Step 25: Create a CODEOWNERS File

With branch protection rules in place from Steps 159-167, let's create a CODEOWNERS file to specify who needs to review which parts of the codebase:

168. In your GitHub repository, click on the **Add file** button and select **Create new file**
169. Name the file `.github/CODEOWNERS` (create the `.github` directory if it doesn't exist)
170. Add the following content, adjusting usernames as needed:

     ```
     # Default owners for everything in the repo
     * @your-username
     
     # Owners for specific directories
     /src/z_customer_* @sap-customer-lead-username
     
     # Owners for specific file types
     *.clas.abap @abap-developer-username
     *.prog.abap @abap-developer-username
     *.fugr.abap @abap-developer-username
     
     # Configuration files
     abaplint.json @abap-admin-username
     .github/workflows/* @devops-username
     ```

171. Click **Commit new file**
172. Enter a commit message: "Add CODEOWNERS file for code review assignments"
173. Choose to commit directly to the main branch
174. Click **Commit new file**

     [SCREENSHOT: GitHub CODEOWNERS file editor with commit dialog]

### Step 26: Create a Pull Request Template

After configuring code owners in Steps 168-174, let's create a pull request template to standardize review information:

175. In your GitHub repository, create a new file at `.github/PULL_REQUEST_TEMPLATE.md`
176. Add the following content:

     ```markdown
     ## Description
     
     [Describe the changes implemented in this pull request]
     
     ## Related Issue
     
     [Reference any related GitHub issues, e.g., "Fixes #123"]
     
     ## Type of Change
     
     - [ ] Bug fix (non-breaking change that fixes an issue)
     - [ ] New feature (non-breaking change that adds functionality)
     - [ ] Breaking change (fix or feature that would cause existing functionality to change)
     - [ ] This change requires a documentation update
     
     ## SAP Impact Analysis
     
     - [ ] Affects existing SAP customizations
     - [ ] Requires transport to test/production systems
     - [ ] Impacts interfaces with other systems
     
     ## Test/Validation Performed
     
     - [ ] Unit tests added/passed
     - [ ] Integration tests added/passed
     - [ ] Manual testing performed in development system
     
     ## Review Checklist
     
     - [ ] Code follows ABAP style guidelines
     - [ ] Documentation/comments updated if needed
     - [ ] Security implications considered
     - [ ] Performance implications considered
     ```

177. Click **Commit new file**
178. Enter a commit message: "Add pull request template"
179. Choose to commit directly to the main branch
180. Click **Commit new file**

     [SCREENSHOT: GitHub pull request template editor with commit dialog]

### Step 27: Create Code Review Guidelines

With the pull request template in place from Steps 175-180, let's create code review guidelines:

181. In your GitHub repository, create a new file at `docs/CODE_REVIEW_GUIDELINES.md`
182. Add the following content:

     ```markdown
     # SAP Code Review Guidelines
     
     ## Purpose
     
     This document outlines the code review process and standards for SAP developments in our GitHub integration.
     
     ## Review Process
     
     1. **Pull Request Creation**
        - Create a feature branch from main
        - Implement and test your changes
        - Create a pull request using the template
        - Assign reviewers based on CODEOWNERS
     
     2. **Automated Validation**
        - Wait for CI pipeline to complete
        - Address any automated check failures
     
     3. **Manual Review**
        - Reviewers should examine code within 2 business days
        - Use GitHub's review feature to provide feedback
        - Focus on the review standards below
     
     4. **Addressing Feedback**
        - Respond to all review comments
        - Make necessary changes and push to the same branch
        - Request re-review when ready
     
     5. **Approval and Merge**
        - Pull request can be merged once approved and all checks pass
        - Use "Squash and merge" option to keep history clean
     
     ## Review Standards
     
     ### Functionality
     
     - [ ] Code correctly implements the intended functionality
     - [ ] Edge cases and error conditions are handled
     - [ ] SAP-specific behaviors are properly addressed
     
     ### Code Quality
     
     - [ ] Follows ABAP programming guidelines
     - [ ] No hardcoded values (use constants or parameters)
     - [ ] SQL statements are efficient and secure
     - [ ] Error handling is appropriate
     
     ### Security
     
     - [ ] No sensitive data is exposed
     - [ ] Authorization checks are implemented where needed
     - [ ] Proper input validation is performed
     
     ### Performance
     
     - [ ] Database calls are optimized
     - [ ] No unnecessary loops or operations
     - [ ] Efficient use of internal tables and data structures
     
     ### Documentation
     
     - [ ] Code includes appropriate comments
     - [ ] Complex logic is explained
     - [ ] API changes are documented
     
     ## Example Review Comments
     
     Good review comments are specific, constructive, and actionable. For example:
     
     - "Consider using a WHERE clause in this SELECT to limit the data retrieved instead of filtering after selection."
     - "This method is quite large (150+ lines). Could it be broken down into smaller, more focused methods?"
     - "Missing authorization check before accessing this sensitive data. Consider adding AUTHORITY-CHECK here."
     ```

183. Click **Commit new file**
184. Enter a commit message: "Add code review guidelines"
185. Choose to commit directly to the main branch
186. Click **Commit new file**

     [SCREENSHOT: GitHub code review guidelines editor with commit dialog]

### Step 28: Test the Code Review Process

With the code review infrastructure set up in Steps 159-186, let's test the process:

187. Create a new feature branch in abapGit:
     - In SAP GUI, execute transaction **SE38** and run program `ZABAPGIT`
     - Navigate to your repository
     - Click on the branch icon and create a new branch named `feature/update-customer-list`
     
188. Modify the customer list program:
     - In SAP GUI, execute transaction **SE38**
     - Open program `Z_CUSTOMER_LIST` created in Tutorial 3
     - Add a new feature to sort the customer list:

     ```abap
     REPORT z_customer_list.
     
     TABLES: kna1. " Customer master data table
     
     PARAMETERS: p_limit TYPE i DEFAULT 10,
                p_sort  TYPE c AS CHECKBOX DEFAULT 'X'.
     
     START-OF-SELECTION.
       " Select customer data
       SELECT kunnr name1 land1 ort01
         FROM kna1
         INTO TABLE @DATA(lt_customers)
         UP TO @p_limit ROWS.
       
       " Sort data if requested
       IF p_sort = 'X'.
         SORT lt_customers BY name1.
       ENDIF.
     
       " Display results
       WRITE: / 'Customer List', / '============'.
       WRITE: / 'Sorted by name:', p_sort, / .
       LOOP AT lt_customers INTO DATA(ls_customer).
         WRITE: / 'ID:', ls_customer-kunnr, 
                'Name:', ls_customer-name1,
                'Country:', ls_customer-land1,
                'City:', ls_customer-ort01.
       ENDLOOP.
     ```

189. Save and activate the program
190. Push the changes to GitHub:
     - Return to abapGit
     - Stage the modified program
     - Enter a commit message: "Add sorting feature to customer list"
     - Push the changes
     
191. Create a pull request:
     - Open your web browser and navigate to your GitHub repository
     - GitHub should show a notification about your recently pushed branch
     - Click on **Compare & pull request**
     - Notice that the pull request template is automatically loaded
     - Fill out the template with appropriate information
     - Click **Create pull request**
     
     [SCREENSHOT: GitHub pull request creation with template]

192. Observe the automated checks:
     - The CI workflow should automatically start
     - Wait for the syntax check and unit tests to complete
     
193. Perform a code review:
     - In the pull request, go to the "Files changed" tab
     - Review the code changes
     - Click the "+" icon next to a line to add a comment
     - Add a review comment: "Consider adding field selection in the sort parameter to allow sorting by different fields"
     - Click **Start a review**
     - Add another comment if needed
     - Click **Review changes**
     - Select **Request changes**
     - Submit the review
     
     [SCREENSHOT: GitHub code review interface with comments]

194. Update the code based on the review:
     - Return to SAP GUI and update the program with field selection:

     ```abap
     REPORT z_customer_list.
     
     TABLES: kna1. " Customer master data table
     
     PARAMETERS: p_limit TYPE i DEFAULT 10,
                p_sort  TYPE c AS CHECKBOX DEFAULT 'X',
                p_field TYPE c DEFAULT 'N' RADIOBUTTON GROUP fld,
                p_cntry TYPE c DEFAULT ' ' RADIOBUTTON GROUP fld,
                p_city  TYPE c DEFAULT ' ' RADIOBUTTON GROUP fld.
     
     START-OF-SELECTION.
       " Select customer data
       SELECT kunnr name1 land1 ort01
         FROM kna1
         INTO TABLE @DATA(lt_customers)
         UP TO @p_limit ROWS.
       
       " Sort data if requested
       IF p_sort = 'X'.
         CASE 'X'.
           WHEN p_field.
             SORT lt_customers BY name1.
           WHEN p_cntry.
             SORT lt_customers BY land1.
           WHEN p_city.
             SORT lt_customers BY ort01.
           WHEN OTHERS.
             SORT lt_customers BY name1.
         ENDCASE.
       ENDIF.
     
       " Display results
       WRITE: / 'Customer List', / '============'.
       WRITE: / 'Sorted:', p_sort, / .
       LOOP AT lt_customers INTO DATA(ls_customer).
         WRITE: / 'ID:', ls_customer-kunnr, 
                'Name:', ls_customer-name1,
                'Country:', ls_customer-land1,
                'City:', ls_customer-ort01.
       ENDLOOP.
     ```

195. Save, activate, and push the updated program
196. Return to the pull request in GitHub
197. Note that the pull request shows the new commit
198. The reviewer can now approve the changes
199. Once approved and all checks pass, the pull request can be merged

## Verification Checkpoint

Before concluding this tutorial, verify that:

- Branch protection rules enforce code reviews and status checks
- Pull request template provides structured information
- CODEOWNERS file correctly assigns reviewers
- Code review guidelines are clear and accessible
- The entire review process works from branch creation to merge

## Common Issues and Solutions

### Review Assignment Issues

**Issue**: Pull requests aren't automatically assigned to the correct reviewers

**Solution**:
1. Verify that the CODEOWNERS file is in the correct location (`.github/CODEOWNERS`)
2. Check that the file patterns in CODEOWNERS match your project structure
3. Ensure team members have the proper repository access levels

### CI Integration Problems

**Issue**: Status checks aren't showing up in pull requests

**Solution**:
1. Verify that your CI workflow is correctly configured
2. Check that the branch protection rule references the correct status check names
3. Ensure the CI workflow is triggered on pull requests to the main branch

### Review Workflow Confusion

**Issue**: Team members are unsure of the review process

**Solution**:
1. Conduct a training session on the code review process
2. Make the CODE_REVIEW_GUIDELINES.md document easily accessible
3. Create example pull requests to demonstrate the process

## Next Steps

Now that you have successfully implemented a code review process in Steps 159-199, you're ready to proceed to the next phase of the integration:

- **[Tutorial 6: Security Hardening](./security-hardening.md)** - In this next tutorial, you'll implement security best practices for your SAP-GitHub integration to protect your code and systems.

In Tutorial 6, you'll learn how to secure your integration with proper authentication, access controls, and sensitive data management, building on the solid foundation you've established through the first five tutorials. 