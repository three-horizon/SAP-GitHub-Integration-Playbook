---
layout: default
title: OAuth Configuration for SAP-GitHub Integration
---

# OAuth Configuration for SAP-GitHub Integration

This document provides a comprehensive guide for implementing OAuth 2.0 authentication between SAP systems and GitHub, ensuring secure and authorized access for integration scenarios.

## Overview

OAuth 2.0 is the industry-standard protocol for authorization that enables third-party applications to obtain limited access to a service on behalf of a resource owner. This example demonstrates how to configure OAuth 2.0 authentication for SAP-GitHub integration scenarios.

## Prerequisites

- SAP system with Internet connectivity
- GitHub Enterprise or GitHub.com organization
- Administrative access to both platforms
- SSL certificates for secure communication
- Network connectivity between systems

## Implementation Steps

### 1. Register OAuth Application in GitHub

1. Login to GitHub as an organization administrator
2. Navigate to **Organization Settings** > **Developer Settings** > **OAuth Apps**
3. Click **New OAuth App**
4. Fill in the required details:
   ```
   Application name: SAP Integration
   Homepage URL: https://your-sap-system.example.com
   Application description: SAP system integration with GitHub
   Authorization callback URL: https://your-sap-system.example.com/oauth/callback
   ```
5. Click **Register application**
6. Note the generated **Client ID**
7. Click **Generate a new client secret** and securely store the generated secret

### 2. Configure OAuth in SAP System

#### For SAP NetWeaver

1. Run transaction **SOAUTH2**
2. Select **Create** button
3. Enter the following details:
   ```
   Profile Name: GITHUB_OAUTH
   OAuth Provider Description: GitHub OAuth Provider
   Authorization Endpoint: https://github.com/login/oauth/authorize
   Token Endpoint: https://github.com/login/oauth/access_token
   Client ID: <Client ID from GitHub>
   Client Secret: <Client Secret from GitHub>
   Scope: repo,admin:repo_hook,user
   ```
4. Save the configuration

#### For SAP BTP

1. Navigate to the SAP BTP Cockpit
2. Access your subaccount
3. Go to **Security** > **Destinations**
4. Create a new destination with the following configuration:
   ```
   Name: GITHUB_OAUTH
   Type: HTTP
   URL: https://api.github.com
   Proxy Type: Internet
   Authentication: OAuth2ClientCredentials
   Client ID: <Client ID from GitHub>
   Client Secret: <Client Secret from GitHub>
   Token Service URL: https://github.com/login/oauth/access_token
   Token Service User: <Client ID from GitHub>
   Token Service Password: <Client Secret from GitHub>
   ```
5. Add additional properties:
   ```
   OAuth2.Scope: repo,admin:repo_hook,user
   GitHub-Accept: application/vnd.github.v3+json
   ```
6. Save the destination

### 3. Create OAuth Callback Handler

#### For SAP NetWeaver (ABAP)

```abap
CLASS zcl_github_oauth_handler DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES:
      BEGIN OF ty_token_response,
        access_token TYPE string,
        token_type   TYPE string,
        scope        TYPE string,
      END OF ty_token_response.

    METHODS:
      constructor,
      handle_callback
        IMPORTING
          iv_code          TYPE string
        RETURNING
          VALUE(rs_result) TYPE ty_token_response
        RAISING
          cx_static_check,
      get_authorization_url
        RETURNING
          VALUE(rv_url) TYPE string.

  PRIVATE SECTION.
    DATA:
      mv_client_id     TYPE string,
      mv_client_secret TYPE string,
      mv_redirect_uri  TYPE string,
      mv_scope         TYPE string.
ENDCLASS.

CLASS zcl_github_oauth_handler IMPLEMENTATION.
  METHOD constructor.
    " Read configuration from secured storage
    " This is just an example - implement secure storage according to your standards
    mv_client_id     = 'your_github_client_id'.
    mv_client_secret = 'your_github_client_secret'.
    mv_redirect_uri  = 'https://your-sap-system.example.com/oauth/callback'.
    mv_scope         = 'repo,admin:repo_hook,user'.
  ENDMETHOD.

  METHOD get_authorization_url.
    rv_url = |https://github.com/login/oauth/authorize?| &&
             |client_id={ mv_client_id }| &&
             |&redirect_uri={ mv_redirect_uri }| &&
             |&scope={ mv_scope }| &&
             |&state={ cl_abap_random=>create_random_hex( 8 ) }|.
  ENDMETHOD.

  METHOD handle_callback.
    DATA: lo_http_client TYPE REF TO if_http_client,
          lv_url         TYPE string,
          lv_response    TYPE string.

    " Prepare token request URL
    lv_url = 'https://github.com/login/oauth/access_token'.

    " Create HTTP client
    cl_http_client=>create_by_url(
      EXPORTING
        url                = lv_url
      IMPORTING
        client             = lo_http_client
      EXCEPTIONS
        argument_not_found = 1
        plugin_not_active  = 2
        internal_error     = 3
        OTHERS             = 4 ).

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_static_check.
    ENDIF.

    " Set request method and headers
    lo_http_client->request->set_method( 'POST' ).
    lo_http_client->request->set_header_field(
        name  = 'Content-Type'
        value = 'application/x-www-form-urlencoded' ).
    lo_http_client->request->set_header_field(
        name  = 'Accept'
        value = 'application/json' ).

    " Set request body
    DATA(lv_body) = |client_id={ mv_client_id }| &&
                    |&client_secret={ mv_client_secret }| &&
                    |&code={ iv_code }| &&
                    |&redirect_uri={ mv_redirect_uri }|.

    lo_http_client->request->set_cdata( lv_body ).

    " Send request
    lo_http_client->send(
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        OTHERS                     = 4 ).

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_static_check.
    ENDIF.

    " Receive response
    lo_http_client->receive(
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        OTHERS                     = 4 ).

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_static_check.
    ENDIF.

    " Get response
    lv_response = lo_http_client->response->get_cdata( ).

    " Parse JSON response
    DATA: lo_json_parser TYPE REF TO /ui2/cl_json.
    CREATE OBJECT lo_json_parser.
    
    rs_result = lo_json_parser->deserialize(
      json = lv_response
      pretty_name = /ui2/cl_json=>pretty_mode-none ).

    " Store the token securely for future use
    " Implement secure storage according to your requirements

    " Close HTTP connection
    lo_http_client->close( ).
  ENDMETHOD.
ENDCLASS.
```

## Security Considerations

### Token Security

1. **Secure Storage**
   - Always encrypt tokens before storing them
   - Use platform-specific secure storage (SSF in SAP NetWeaver, CredStore in SAP BTP)
   - Implement proper key management

2. **Token Scope**
   - Use the principle of least privilege
   - Request only necessary permissions (scopes)
   - Regularly audit and review token permissions

3. **Token Lifecycle**
   - Implement token expiry and refresh mechanisms
   - Revoke tokens when no longer needed
   - Implement user off-boarding processes to clean up tokens

### Communication Security

1. **Transport Security**
   - Use TLS 1.2 or higher for all communication
   - Implement certificate validation
   - Regularly update cryptographic libraries

2. **CSRF Protection**
   - Use state parameter in OAuth flows
   - Validate state parameter on callback
   - Implement CSRF tokens for form submissions

3. **Error Handling**
   - Implement secure error handling
   - Avoid leaking sensitive information in error messages
   - Log security-related errors for monitoring

## Implementation Checklist

- [ ] Register OAuth application in GitHub
- [ ] Configure OAuth client in SAP system
- [ ] Implement OAuth callback handler
- [ ] Set up secure token storage
- [ ] Implement token refresh logic
- [ ] Configure secure communication (TLS/SSL)
- [ ] Test authentication flow end-to-end
- [ ] Set up token lifecycle management
- [ ] Implement logging and monitoring
- [ ] Document the implementation

## Best Practices

1. **Minimize Token Usage**
   - Where possible, use SSH keys for Git operations instead of OAuth tokens
   - Consider using GitHub Apps with fine-grained permissions for specific use cases

2. **Token Rotation**
   - Implement regular token rotation
   - Automate token rotation processes
   - Monitor token usage patterns

3. **Monitoring and Alerting**
   - Monitor failed authentication attempts
   - Set up alerts for suspicious token usage
   - Implement audit logging for all token operations

## Troubleshooting

### Common Issues and Solutions

1. **OAuth Authorization Failures**
   - **Issue**: "Invalid client_id" error
   - **Solution**: Verify client ID matches the one registered in GitHub

2. **Token Exchange Failures**
   - **Issue**: Unable to exchange authorization code for token
   - **Solution**: Check that the client secret is correct and validate the code hasn't expired

3. **Access Token Expiration**
   - **Issue**: API calls failing with 401 Unauthorized
   - **Solution**: Implement token refresh logic and ensure tokens are being refreshed properly

4. **Scope Issues**
   - **Issue**: "Insufficient scope" error when accessing GitHub API
   - **Solution**: Review and update OAuth scopes to include all required permissions

## Next Steps

- Implement [GitHub Actions Workflows](../github-actions/ui5-build-pipeline.md) to automate your development processes
- Set up [API Management](../api-management/sap-to-github.md) for comprehensive API governance
- Configure [System Connectivity](../xml-config/system-connectivity.md) for robust system integration

---

**Document Metadata:**
- Last Updated: 2023-11-15
- Version: 1.0.0
- Status: Published 
