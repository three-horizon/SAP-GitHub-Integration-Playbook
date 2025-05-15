# Tutorial 6: Security Hardening

## Introduction

In the previous tutorials, you installed abapGit ([Tutorial 1](../3-setup-guide/installing-abapgit.md)), configured it for your project ([Tutorial 2](../3-setup-guide/configure-abapgit.md)), implemented a development workflow ([Tutorial 3](../4-workflow-tutorials/development-workflow.md)), set up continuous integration ([Tutorial 4](../4-workflow-tutorials/continuous-integration.md)), and implemented code reviews ([Tutorial 5](./code-reviews.md)). Now that you have a functioning integration with proper workflows (Steps 1-199), it's time to strengthen the security of your SAP-GitHub integration to protect your code and systems.

## Prerequisites

Before beginning this tutorial, ensure you have:

- Completed all steps in [Tutorial 1](../3-setup-guide/installing-abapgit.md) (Steps 1-31)
- Completed all steps in [Tutorial 2](../3-setup-guide/configure-abapgit.md) (Steps 32-65)
- Completed all steps in [Tutorial 3](../4-workflow-tutorials/development-workflow.md) (Steps 66-110)
- Completed all steps in [Tutorial 4](../4-workflow-tutorials/continuous-integration.md) (Steps 111-158)
- Completed all steps in [Tutorial 5](./code-reviews.md) (Steps 159-199)
- Admin access to your GitHub repository
- SAP administrator privileges for authorization configuration

## Security Hardening Overview

[DIAGRAM: Security integration diagram showing SAP security, abapGit security, GitHub security, and network security layers]

This tutorial covers the implementation of security best practices for your SAP-GitHub integration. By the end of this tutorial, you will have:

1. Secured authentication between SAP and GitHub
2. Implemented proper authorization controls
3. Protected sensitive data
4. Enabled security scanning
5. Configured audit logging
6. Implemented secure development practices

## Step-by-Step Implementation

### Step 29: Implement Secure Authentication

Continuing from Tutorial 5 where you completed setting up the code review process in Steps 159-199, you'll now enhance authentication security:

200. Create a dedicated GitHub service account:
     - In GitHub, create a new user account specifically for SAP integration
     - Give it a descriptive name like `sap-integration-youcompany`
     - Use a company email address for this account
     - Secure it with a strong password and two-factor authentication

201. Configure GitHub personal access token (PAT) with limited scope:
     - Log in to the service account
     - Navigate to Settings > Developer settings > Personal access tokens
     - Click **Generate new token**
     - Provide a descriptive name: "SAP abapGit Integration"
     - Select only the required scopes:
       - ✓ repo (to access repositories)
       - ✓ read:org (if working with organization repositories)
     - Do NOT select unnecessary scopes like delete_repo, admin, etc.
     - Click **Generate token**

202. Create a secure token storage in SAP:
     - Execute transaction **SE38**
     - Create a new program `Z_GITHUB_TOKEN_SECURE`
     - Add the following code:

     ```abap
     REPORT z_github_token_secure.
     
     CONSTANTS: c_token_id TYPE string VALUE 'GITHUB_INTEGRATION_TOKEN'.
     
     PARAMETERS: p_action RADIOBUTTON GROUP act DEFAULT 'X',
                p_store  RADIOBUTTON GROUP act,
                p_test   RADIOBUTTON GROUP act,
                p_token  TYPE string LOWER CASE VISIBLE LENGTH 40.
     
     START-OF-SELECTION.
       CASE 'X'.
         WHEN p_action.
           " Display current token (masked)
           DATA(lv_token) = get_stored_token( ).
           IF lv_token IS INITIAL.
             WRITE: / 'No token currently stored.'.
           ELSE
             WRITE: / 'Token is stored (first 4 chars):', lv_token(4), '****************'.
           ENDIF.
         WHEN p_store.
           " Store new token
           IF p_token IS INITIAL.
             WRITE: / 'Error: Token cannot be empty.'.
             RETURN.
           ENDIF.
           store_token( p_token ).
           WRITE: / 'Token stored successfully.'.
         WHEN p_test.
           " Test token with GitHub API
           DATA(lv_test_token) = get_stored_token( ).
           IF lv_test_token IS INITIAL.
             WRITE: / 'Error: No token stored.'.
             RETURN.
           ENDIF.
           test_github_connection( lv_test_token ).
       ENDCASE.
     
     FORM store_token USING iv_token TYPE string.
       " In a real scenario, this would use secure storage like:
       " - SSF (Secure Store and Forward)
       " - CPS (Credential Protection Service)
       " - SECSTORE (Secure Storage in the Database)
       
       " Sample implementation using SECSTORE
       TRY.
           cl_secstore=>create_with_key(
             EXPORTING
               iv_key    = c_token_id
               iv_value  = iv_token
           ).
         CATCH cx_root INTO DATA(lx_error).
           WRITE: / 'Error storing token:', lx_error->get_text( ).
       ENDTRY.
     ENDFORM.
     
     FORM get_stored_token RETURNING rv_token TYPE string.
       " Sample implementation using SECSTORE
       TRY.
           cl_secstore=>read_with_key(
             EXPORTING
               iv_key    = c_token_id
             IMPORTING
               ev_value  = rv_token
           ).
         CATCH cx_root INTO DATA(lx_error).
           CLEAR rv_token.
       ENDTRY.
     ENDFORM.
     
     FORM test_github_connection USING iv_token TYPE string.
       " Test connection to GitHub API using the token
       TRY.
           " Create HTTP client
           DATA(lo_http_client) = cl_http_client=>create_by_url( 'https://api.github.com/user' ).
           
           " Set request headers
           lo_http_client->request->set_header_field(
             name  = 'Authorization'
             value = |token { iv_token }|
           ).
           lo_http_client->request->set_header_field(
             name  = 'User-Agent'
             value = 'SAP-abapGit-Integration'
           ).
           
           " Send request
           lo_http_client->send( ).
           lo_http_client->receive( ).
           
           " Check response
           DATA(lv_status) = lo_http_client->response->get_status( ).
           IF lv_status = 200.
             WRITE: / 'Connection successful! Token is valid.'.
           ELSE
             WRITE: / 'Connection failed with status:', lv_status.
             WRITE: / 'Response:', lo_http_client->response->get_cdata( ).
           ENDIF.
           
           " Close connection
           lo_http_client->close( ).
         CATCH cx_root INTO DATA(lx_error).
           WRITE: / 'Error testing connection:', lx_error->get_text( ).
       ENDTRY.
     ENDFORM.
     ```

203. Save and activate the program
204. Execute the program and store your GitHub token:
     - Run transaction **SE38** and execute `Z_GITHUB_TOKEN_SECURE`
     - Select "Store token" radio button
     - Enter the personal access token created in Step 201
     - Execute the program
     - Verify successful storage
     - Test the token connection using the "Test connection" option

### Step 30: Configure SAP Authorizations

With secure authentication set up in Steps 200-204, let's implement proper authorization controls:

205. Create a dedicated authorization object for abapGit:
     - Execute transaction **SE11**
     - Select "Authorization Object" and click **Create**
     - Enter object name `ZABAPGIT`
     - Enter description "abapGit Integration Authorization"
     - Add authorization fields:
       - ACTVT (Activity)
       - DEVCLASS (Development Class)
     - Save and activate the object

206. Create an authorization profile:
     - Execute transaction **PFCG**
     - Create a new role `Z_ABAPGIT_DEVELOPER`
     - Enter description "abapGit Developer Role"
     - Go to the "Authorizations" tab
     - Add the `ZABAPGIT` authorization object with:
       - ACTVT = 01 (Create), 02 (Change), 03 (Display)
       - DEVCLASS = Z* (or specific package names)
     - Add standard development authorizations:
       - S_DEVELOP with appropriate values
       - S_TRANSPRT for transport management
     - Save and generate the profile

207. Assign users to the role:
     - Still in transaction **PFCG**
     - Go to the "Users" tab
     - Assign developer users who need to use abapGit
     - Save the assignments

### Step 31: Protect Sensitive Data

After configuring authorization controls in Steps 205-207, let's ensure sensitive data is properly protected:

208. Create a .gitignore file for sensitive content:
     - In your GitHub repository, create a new file `.gitignore`
     - Add patterns to exclude sensitive files:

     ```
     # Sensitive SAP data
     *PASSWORD*
     *CREDENTIAL*
     *.pse
     *.cer
     *.key
     
     # Configuration with sensitive data
     **/z_config_sensitive*
     
     # Temporary and local files
     .DS_Store
     Thumbs.db
     desktop.ini
     ```

209. Create a program to check for sensitive data:
     - Execute transaction **SE38**
     - Create a new program `Z_CHECK_SENSITIVE_DATA`
     - Add the following code to scan for potential sensitive data in ABAP objects

### Step 32: Implement Security Scanning

With sensitive data protection in place from Steps 208-209, let's implement automated security scanning:

212. Add a security scanning workflow to GitHub:
     - In your GitHub repository, create a new file `.github/workflows/security-scan.yml`
     - Add the following content:

     ```yaml
     name: Security Scan
     
     on:
       push:
         branches: [ main ]
       pull_request:
         branches: [ main ]
       schedule:
         - cron: '0 0 * * 0'  # Run weekly on Sundays
     
     jobs:
       abap-security-scan:
         runs-on: ubuntu-latest
         steps:
           - uses: actions/checkout@v2
           
           - name: Set up Node.js
             uses: actions/setup-node@v2
             with:
               node-version: '14'
               
           - name: Install abaplint
             run: npm install -g @abaplint/cli
             
           - name: Install security plugins
             run: npm install @abaplint/core
             
           - name: Create security configuration
             run: |
               cat > abaplint-security.json << 'EOF'
               {
                 "global": {
                   "files": "/src/**/*.*"
                 },
                 "rules": {
                   "sql_injection": true,
                   "cross_site_scripting": true,
                   "hardcoded_credentials": true,
                   "insecure_encryption": true,
                   "auth_check_missing": true
                 }
               }
               EOF
             
           - name: Run security scan
             run: abaplint --config abaplint-security.json
             
           - name: Check for secrets in code
             uses: zricethezav/gitleaks-action@master
             
           - name: Run dependency checks
             uses: snyk/actions/node@master
             env:
               SNYK_TOKEN: ${{ secrets.SNYK_TOKEN }}
             with:
               args: --all-projects
     ```

213. Commit the security scanning workflow:
     - Click **Commit new file**
     - Enter a commit message: "Add security scanning workflow"
     - Choose to commit directly to the main branch
     - Click **Commit new file**

214. Set up automated vulnerability scanning in abapGit:
     - Create a pull request template specifically for security-sensitive changes
     - Create a new file `.github/SECURITY_PR_TEMPLATE.md` with the following content:

     ```markdown
     ## Security Change Request
     
     ### Security Impact Assessment
     
     - [ ] This change modifies authentication or authorization code
     - [ ] This change handles sensitive data
     - [ ] This change affects network communication
     - [ ] This change modifies user inputs or outputs
     
     ### Security Controls Implemented
     
     - [ ] Input validation for all user-controlled data
     - [ ] Proper authentication checks
     - [ ] Secure data handling (no sensitive data in logs or GitHub)
     - [ ] Protection against common ABAP security issues
     
     ### Security Testing Performed
     
     - [ ] Code reviewed for security issues by security team member
     - [ ] Security scan completed with no high/critical issues
     - [ ] Penetration testing performed (if applicable)
     
     ### Additional Security Notes
     
     [Add any security-related notes or context here]
     ```

215. Commit the security PR template:
     - Click **Commit new file**
     - Enter a commit message: "Add security pull request template"
     - Choose to commit directly to the main branch
     - Click **Commit new file**

### Step 33: Configure Audit Logging

With security scanning in place from Steps 212-215, let's implement audit logging:

216. Create an audit logging program in SAP:
     - Execute transaction **SE38**
     - Create a new program `Z_ABAPGIT_AUDIT_LOG`
     - Add the following code:

     ```abap
     REPORT z_abapgit_audit_log.
     
     CONSTANTS: c_log_object TYPE balobj_d VALUE 'ZABAPGIT',
                c_log_subobj TYPE balsubobj VALUE 'AUDIT'.
     
     PARAMETERS: p_action RADIOBUTTON GROUP act DEFAULT 'X',
                p_view   RADIOBUTTON GROUP act,
                p_user   TYPE syuname DEFAULT sy-uname,
                p_days   TYPE i DEFAULT 7,
                p_action TYPE string,
                p_object TYPE string.
     
     START-OF-SELECTION.
       CASE 'X'.
         WHEN p_action.
           " Log an action
           IF p_action IS INITIAL OR p_object IS INITIAL.
             WRITE: / 'Error: Action and Object are required for logging.'.
             RETURN.
           ENDIF.
           log_abapgit_action(
             iv_action = p_action
             iv_object = p_object
           ).
           WRITE: / 'Action logged successfully.'.
           
         WHEN p_view.
           " View logs
           display_logs(
             iv_username = p_user
             iv_days     = p_days
           ).
       ENDCASE.
     
     FORM log_abapgit_action USING iv_action TYPE string
                                  iv_object TYPE string.
       " Create application log
       DATA: ls_log     TYPE bal_s_log,
             ls_msg     TYPE bal_s_msg,
             lv_log_handle TYPE balloghndl.
       
       " Initialize log object
       ls_log-object    = c_log_object.
       ls_log-subobject = c_log_subobj.
       ls_log-aldate    = sy-datum.
       ls_log-altime    = sy-uzeit.
       ls_log-aluser    = sy-uname.
       ls_log-alprog    = sy-repid.
       
       " Create log handle
       CALL FUNCTION 'BAL_LOG_CREATE'
         EXPORTING
           i_s_log      = ls_log
         IMPORTING
           e_log_handle = lv_log_handle
         EXCEPTIONS
           OTHERS       = 1.
       
       IF sy-subrc <> 0.
         WRITE: / 'Error creating application log.'.
         RETURN.
       ENDIF.
       
       " Add message to log
       ls_msg-msgty = 'I'.
       ls_msg-msgid = 'ZABAPGIT'. " Replace with your message class
       ls_msg-msgno = '001'.       " Replace with your message number
       ls_msg-msgv1 = iv_action.
       ls_msg-msgv2 = iv_object.
       ls_msg-msgv3 = sy-uname.
       ls_msg-msgv4 = |{ sy-datum DATE = ISO } { sy-uzeit TIME = ISO }|.
       
       " If no message class exists, use this alternative
       ls_msg-msgty = 'I'.
       ls_msg-msgid = '00'.
       ls_msg-msgno = '001'.
       ls_msg-msgv1 = |abapGit action: { iv_action }|.
       ls_msg-msgv2 = |Object: { iv_object }|.
       ls_msg-msgv3 = |User: { sy-uname }|.
       ls_msg-msgv4 = |Time: { sy-datum DATE = ISO } { sy-uzeit TIME = ISO }|.
       
       CALL FUNCTION 'BAL_LOG_MSG_ADD'
         EXPORTING
           i_log_handle = lv_log_handle
           i_s_msg      = ls_msg
         EXCEPTIONS
           OTHERS       = 1.
       
       IF sy-subrc <> 0.
         WRITE: / 'Error adding message to log.'.
         RETURN.
       ENDIF.
       
       " Save log to database
       CALL FUNCTION 'BAL_DB_SAVE'
         EXPORTING
           i_save_all = 'X'
         EXCEPTIONS
           OTHERS     = 1.
       
       IF sy-subrc <> 0.
         WRITE: / 'Error saving log to database.'.
         RETURN.
       ENDIF.
     ENDFORM.
     
     FORM display_logs USING iv_username TYPE syuname
                            iv_days     TYPE i.
       DATA: lt_log_handle TYPE bal_t_logh,
             lt_log_header TYPE bal_t_logh,
             ls_log_filter TYPE bal_s_lfil,
             ls_date       TYPE bal_s_date.
       
       " Set filter for username
       ls_log_filter-object    = c_log_object.
       ls_log_filter-subobject = c_log_subobj.
       ls_log_filter-aluser    = iv_username.
       
       " Set date range
       ls_date-sign   = 'I'.
       ls_date-option = 'BT'.
       ls_date-low    = sy-datum - iv_days.
       ls_date-high   = sy-datum.
       APPEND ls_date TO ls_log_filter-date.
       
       " Get log handles
       CALL FUNCTION 'BAL_DB_SEARCH'
         EXPORTING
           i_s_log_filter = ls_log_filter
         IMPORTING
           e_t_log_handle = lt_log_handle
         EXCEPTIONS
           OTHERS         = 1.
       
       IF sy-subrc <> 0.
         WRITE: / 'Error searching logs.'.
         RETURN.
       ENDIF.
       
       IF lt_log_handle IS INITIAL.
         WRITE: / 'No logs found for the specified criteria.'.
         RETURN.
       ENDIF.
       
       " Display logs
       CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
         EXPORTING
           i_t_log_handle = lt_log_handle
         EXCEPTIONS
           OTHERS         = 1.
       
       IF sy-subrc <> 0.
         WRITE: / 'Error displaying logs.'.
         RETURN.
       ENDIF.
     ENDFORM.
     ```

217. Save and activate the program
218. Create a hook in abapGit to log actions:
     - Create a class `ZCL_ABAPGIT_AUDIT_HOOK` in transaction **SE24**
     - Implement the class as follows:

     ```abap
     CLASS zcl_abapgit_audit_hook DEFINITION
       PUBLIC
       FINAL
       CREATE PUBLIC .
     
       PUBLIC SECTION.
         CLASS-METHODS:
           log_action
             IMPORTING
               iv_action TYPE string
               iv_object TYPE string,
           init.
     ENDCLASS.
     
     CLASS zcl_abapgit_audit_hook IMPLEMENTATION.
       METHOD log_action.
         SUBMIT z_abapgit_audit_log
           WITH p_action = 'X'
           WITH p_action = iv_action
           WITH p_object = iv_object
           AND RETURN.
       ENDMETHOD.
     
       METHOD init.
         " Hook initialization code
         " In a real implementation, this would be called from abapGit
         " through its user exit functionality
       ENDMETHOD.
     ENDCLASS.
     ```

219. Save and activate the class
220. Test the audit logging:
     - Execute transaction **SE38** and run program `Z_ABAPGIT_AUDIT_LOG`
     - Select "Log an action" radio button
     - Enter "PUSH" as action and "Z_GITHUB_DEMO" as object
     - Execute the program
     - Verify the action was logged successfully
     - Run the program again with "View logs" selected
     - Verify you can see the logged actions

### Step 34: Implement Secure Development Practices

With audit logging in place from Steps 216-220, let's implement secure development practices:

221. Create security guidelines for developers:
     - In your GitHub repository, create a new file `docs/SECURITY_GUIDELINES.md`
     - Add the following content:

     ```markdown
     # Security Guidelines for SAP-GitHub Integration
     
     ## Purpose
     
     This document provides security guidelines for developers working with the SAP-GitHub integration. Following these guidelines helps maintain a secure development environment and protects both SAP and GitHub systems from security risks.
     
     ## Authentication and Access Control
     
     ### GitHub Authentication
     
     - Always use personal access tokens (PATs) with limited scope
     - Never share tokens between users
     - Store tokens securely in SAP using the `Z_GITHUB_TOKEN_SECURE` program
     - Rotate tokens every 90 days or when team members leave
     
     ### SAP Authorization
     
     - Only assign the `Z_ABAPGIT_DEVELOPER` role to authorized developers
     - Follow the principle of least privilege for all authorizations
     - Regularly review and audit user access
     
     ## Sensitive Data Protection
     
     ### Code Content
     
     - Never include hardcoded credentials in ABAP code
     - Use the `Z_CHECK_SENSITIVE_DATA` program before committing code
     - Exclude sensitive files using .gitignore patterns
     
     ### GitHub Content
     
     - Never store configuration files with credentials in GitHub
     - Use GitHub secrets for CI/CD variables
     - Be careful with error messages that might expose system details
     
     ## Secure Coding Practices
     
     ### SQL Injection Prevention
     
     - Always use parameterized queries with bind parameters
     - Example: 
       ```abap
       SELECT * FROM ztable INTO TABLE @DATA(lt_data)
         WHERE field = @lv_param.  " Good practice
       ```
     - Never concatenate SQL statements with user input
     - Bad example:
       ```abap
       DATA(lv_query) = |SELECT * FROM ztable WHERE field = '{ lv_user_input }'|.
       EXECUTE lv_query.  " Bad practice - SQL injection risk
       ```
     
     ### Authorization Checks
     
     - Implement authorization checks in all sensitive operations
     - Example:
       ```abap
       AUTHORITY-CHECK OBJECT 'ZABAPGIT'
         ID 'ACTVT' FIELD '02'
         ID 'DEVCLASS' FIELD lv_package.
       
       IF sy-subrc <> 0.
         RAISE EXCEPTION TYPE zcx_abapgit_auth_error.
       ENDIF.
       ```
     
     ### Input Validation
     
     - Validate all user inputs before processing
     - Sanitize data before displaying it to prevent XSS
     - Use allowlists rather than denylists for input validation
     
     ## Security Incident Response
     
     ### Reporting Security Issues
     
     - Report any security issues immediately to the security team
     - Document security incidents in the audit log
     - Do not share security vulnerabilities in public channels
     
     ### Emergency Response
     
     - In case of a security breach, immediately revoke affected tokens
     - Follow the organization's incident response procedure
     - Conduct a post-incident review to prevent future issues
     
     ## Security Testing
     
     ### Regular Testing
     
     - Run the security scan workflow before major releases
     - Periodically review security logs
     - Conduct security reviews of critical code changes
     
     ### Vulnerability Management
     
     - Address all high and critical security findings immediately
     - Include security fixes in regular development cycles
     - Keep dependencies updated to prevent known vulnerabilities
     ```

222. Commit the security guidelines:
     - Click **Commit new file**
     - Enter a commit message: "Add security guidelines for developers"
     - Choose to commit directly to the main branch
     - Click **Commit new file**

223. Implement a security checklist for pull requests:
     - Create a GitHub Issue template for security reviews
     - Create a new file `.github/ISSUE_TEMPLATE/security-review.md`
     - Add the following content:

     ```markdown
     ---
     name: Security Review
     about: Request a security review for code changes
     title: 'Security Review: [Brief Description]'
     labels: security, review
     assignees: ''
     ---
     
     ## Security Review Request
     
     ### Pull Request Reference
     
     [Link to the pull request that needs security review]
     
     ### Changes Description
     
     [Brief description of the changes that require security review]
     
     ### Security Concerns
     
     - [ ] Authentication/Authorization changes
     - [ ] Handling sensitive data
     - [ ] Database operations
     - [ ] User input processing
     - [ ] API integration
     - [ ] Other (please specify)
     
     ### Security Review Checklist
     
     - [ ] Proper authentication and authorization checks
     - [ ] No sensitive data exposure
     - [ ] Input validation implemented
     - [ ] SQL injection protection
     - [ ] Secure error handling
     - [ ] No hardcoded credentials
     - [ ] Proper logging (no sensitive data in logs)
     
     ### Additional Context
     
     [Any additional information that would help with the security review]
     ```

224. Commit the security review template:
     - Click **Commit new file**
     - Enter a commit message: "Add security review issue template"
     - Choose to commit directly to the main branch
     - Click **Commit new file**

## Verification Checkpoint

Before concluding this tutorial, verify that:

- GitHub authentication uses a service account with limited token scope
- SAP authorization controls are properly implemented
- Sensitive data protection mechanisms are in place
- Security scanning is configured in the CI/CD pipeline
- Audit logging is implemented and functional
- Security guidelines and processes are documented

## Common Issues and Solutions

### Authentication Issues

**Issue**: Personal access token expires or is revoked

**Solution**:
1. Generate a new token in GitHub with appropriate scopes
2. Update the token in the SAP secure storage program
3. Verify the connection works with the new token

### Security Scanning Failures

**Issue**: Security scan workflow fails with false positives

**Solution**:
1. Review the scan results carefully to determine if they are genuine issues
2. For confirmed false positives, update the security scanning configuration
3. Document any exceptions with justification

### Audit Log Management

**Issue**: Audit logs growing too large

**Solution**:
1. Implement a log rotation and archiving strategy
2. Consider filtering less critical actions from being logged
3. Archive older logs to maintain system performance

## Next Steps

Now that you have successfully implemented security hardening for your SAP-GitHub integration in Steps 200-224, you have completed the core tutorials for establishing a secure, efficient integration between SAP and GitHub.

To continue enhancing your implementation, consider:

- Implementing automated deployment workflows
- Setting up monitoring and alerting for the integration
- Creating custom reports for integration activities
- Expanding to additional SAP systems or projects

The security hardening steps in this tutorial have established a solid foundation for protecting your code and systems while enabling the productivity benefits of the SAP-GitHub integration. 