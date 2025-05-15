*&---------------------------------------------------------------------*
*& Title: GitHub Integration API Implementation
*& Description: API Class for GitHub Integration with abapGit
*& Author: SAP-GitHub Integration Team
*& Created: 2023-01-01
*&---------------------------------------------------------------------*
CLASS zcl_github_integration_api DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES:
      BEGIN OF ty_transport_request,
        id          TYPE trkorr,
        description TYPE as4text,
        owner       TYPE tr_as4user,
        target      TYPE tr_target,
        status      TYPE trstatus,
        created_at  TYPE timestamp,
        objects     TYPE i,
      END OF ty_transport_request,

      BEGIN OF ty_abapgit_status,
        package     TYPE devclass,
        branch      TYPE string,
        url         TYPE string,
        objects     TYPE i,
        status      TYPE string,
        last_sync   TYPE timestamp,
        is_dirty    TYPE abap_bool,
        user        TYPE sy-uname,
        transport   TYPE trkorr,
      END OF ty_abapgit_status,

      BEGIN OF ty_api_response,
        status_code  TYPE i,
        message      TYPE string,
        correlation_id TYPE string,
        timestamp    TYPE timestamp,
        payload      TYPE REF TO data,
      END OF ty_api_response.

    METHODS:
      constructor
        IMPORTING
          iv_api_key      TYPE string
          iv_api_endpoint TYPE string
          iv_client_id    TYPE string OPTIONAL,

      get_transport_info
        IMPORTING
          iv_transport_id TYPE trkorr
        RETURNING
          VALUE(rs_response) TYPE ty_api_response
        RAISING
          cx_static_check,

      create_transport
        IMPORTING
          iv_description TYPE as4text
          iv_target      TYPE tr_target
          iv_owner       TYPE tr_as4user OPTIONAL
        RETURNING
          VALUE(rs_response) TYPE ty_api_response
        RAISING
          cx_static_check,

      release_transport
        IMPORTING
          iv_transport_id TYPE trkorr
          iv_target       TYPE tr_target
        RETURNING
          VALUE(rs_response) TYPE ty_api_response
        RAISING
          cx_static_check,

      abapgit_pull
        IMPORTING
          iv_package        TYPE devclass
          iv_branch         TYPE string OPTIONAL
          iv_transport      TYPE trkorr OPTIONAL
          iv_commit_message TYPE string OPTIONAL
        RETURNING
          VALUE(rs_response) TYPE ty_api_response
        RAISING
          cx_static_check,

      abapgit_push
        IMPORTING
          iv_package        TYPE devclass
          iv_branch         TYPE string OPTIONAL
          iv_transport      TYPE trkorr
          iv_commit_message TYPE string
        RETURNING
          VALUE(rs_response) TYPE ty_api_response
        RAISING
          cx_static_check,

      abapgit_status
        IMPORTING
          iv_package TYPE devclass
        RETURNING
          VALUE(rs_response) TYPE ty_api_response
        RAISING
          cx_static_check,

      set_github_token
        IMPORTING
          iv_token TYPE string
        RETURNING
          VALUE(rs_response) TYPE ty_api_response
        RAISING
          cx_static_check.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA:
      mv_api_key      TYPE string,
      mv_api_endpoint TYPE string,
      mv_client_id    TYPE string,
      mo_http_client  TYPE REF TO if_http_client,
      mo_logger       TYPE REF TO /goog/cl_logging.

    METHODS:
      initialize_http_client
        RAISING
          cx_static_check,

      add_auth_headers
        IMPORTING
          io_client TYPE REF TO if_http_client
        RAISING
          cx_static_check,

      convert_abap_to_json
        IMPORTING
          iv_data        TYPE any
        RETURNING
          VALUE(rv_json) TYPE string
        RAISING
          cx_static_check,

      convert_json_to_abap
        IMPORTING
          iv_json       TYPE string
          er_data       TYPE REF TO data
        RAISING
          cx_static_check,

      call_api
        IMPORTING
          iv_method      TYPE string
          iv_path        TYPE string
          iv_payload     TYPE string OPTIONAL
        RETURNING
          VALUE(rs_response) TYPE ty_api_response
        RAISING
          cx_static_check,

      handle_error
        IMPORTING
          iv_message     TYPE string
          iv_status_code TYPE i DEFAULT 500
        RETURNING
          VALUE(rs_response) TYPE ty_api_response,

      log_api_call
        IMPORTING
          iv_method      TYPE string
          iv_path        TYPE string
          iv_payload     TYPE string OPTIONAL
          iv_status_code TYPE i
          iv_response    TYPE string OPTIONAL,

      perform_security_validation
        IMPORTING
          iv_data      TYPE any
        RETURNING
          VALUE(rv_valid) TYPE abap_bool,

      generate_correlation_id
        RETURNING
          VALUE(rv_correlation_id) TYPE string.

ENDCLASS.

CLASS zcl_github_integration_api IMPLEMENTATION.

  METHOD constructor.
    mv_api_key = iv_api_key.
    mv_api_endpoint = iv_api_endpoint.
    mv_client_id = iv_client_id.

    " Initialize logging
    mo_logger = /goog/cl_logging=>get_instance( iv_log_object = 'ZGITHUB'
                                                iv_log_subobject = 'INTEGRATION' ).
    
    " Set up HTTP client
    TRY.
        initialize_http_client( ).
      CATCH cx_static_check INTO DATA(lx_error).
        mo_logger->error( lx_error->get_text( ) ).
    ENDTRY.

    mo_logger->info( |GitHub Integration API initialized for endpoint { mv_api_endpoint }| ).
  ENDMETHOD.

  METHOD initialize_http_client.
    " Create HTTP client factory
    DATA(lo_http_factory) = cl_http_client_factory=>create_by_url( mv_api_endpoint ).
    mo_http_client = lo_http_factory->create_by_url( mv_api_endpoint ).
    
    " Set timeout 
    mo_http_client->propertytype_set_timeout( 60 ).
    
    " Set SSL session parameters if necessary
    DATA(lo_ssl) = NEW cl_http_ssl_config( ).
    lo_ssl->set_ssl_version( ssl_version = cl_http_ssl_config=>sslversion_tls1_2 ).
    mo_http_client->ssl_config_set( lo_ssl ).
  ENDMETHOD.

  METHOD add_auth_headers.
    " Add security headers
    io_client->request->set_header_field( 
        name = 'Ocp-Apim-Subscription-Key'
        value = mv_api_key ).
        
    " Add correlation ID for request tracing
    DATA(lv_correlation_id) = generate_correlation_id( ).
    io_client->request->set_header_field(
        name = 'X-Correlation-ID'
        value = lv_correlation_id ).
        
    " Add client ID if available
    IF mv_client_id IS NOT INITIAL.
      io_client->request->set_header_field(
          name = 'X-Client-ID'
          value = mv_client_id ).
    ENDIF.
    
    " Add content type
    io_client->request->set_header_field(
        name = 'Content-Type'
        value = 'application/json' ).
    
    " Add authorization token if applicable
    " In a real implementation, this would retrieve the token from secure storage
    DATA(lv_auth_token) = get_auth_token_from_secure_storage( ).
    IF lv_auth_token IS NOT INITIAL.
      io_client->request->set_header_field(
          name = 'Authorization'
          value = |Bearer { lv_auth_token }| ).
    ENDIF.
  ENDMETHOD.

  METHOD get_auth_token_from_secure_storage.
    " This is a placeholder method
    " In a real implementation, retrieve the token from secure storage
    " For example, from SSF secure storage
    RETURN.
  ENDMETHOD.

  METHOD convert_abap_to_json.
    " Use ABAP2JSON transformation to convert ABAP structure/table to JSON
    DATA:
      lo_writer TYPE REF TO cl_sxml_string_writer.
      
    lo_writer = cl_sxml_string_writer=>create( type = 'JSON' ).
    CALL TRANSFORMATION id SOURCE data = iv_data 
                           RESULT XML lo_writer.
    rv_json = lo_writer->get_output( ).
    
    " Security: Check for potential security issues in JSON
    IF NOT perform_security_validation( iv_data ).
      RAISE EXCEPTION TYPE cx_static_check
        EXPORTING
          textid = '001'
          text   = 'Security validation failed for JSON conversion'.
    ENDIF.
  ENDMETHOD.

  METHOD convert_json_to_abap.
    " Use JSON2ABAP transformation to convert JSON to ABAP structure/table
    DATA:
      lo_reader TYPE REF TO cl_sxml_string_reader.
      
    lo_reader = cl_sxml_string_reader=>create( cl_abap_codepage=>convert_to( iv_json ) ).
    CALL TRANSFORMATION id SOURCE XML lo_reader
                           RESULT data = er_data.
  ENDMETHOD.

  METHOD call_api.
    DATA:
      lv_http_status TYPE i,
      lv_response    TYPE string.

    TRY.
        " Create HTTP client for the specific path
        DATA(lo_http_client) = cl_http_client_factory=>create_by_url( 
            |{ mv_api_endpoint }{ iv_path }| ).
        
        " Set HTTP method
        lo_http_client->request->set_method( iv_method ).
        
        " Add authentication headers
        add_auth_headers( lo_http_client ).
        
        " Add payload if provided
        IF iv_payload IS NOT INITIAL.
          lo_http_client->request->set_cdata( iv_payload ).
        ENDIF.
        
        " Execute the request
        lo_http_client->send( ).
        lo_http_client->receive( ).
        
        " Get HTTP status
        lv_http_status = lo_http_client->response->get_status( ).
        
        " Get response
        lv_response = lo_http_client->response->get_cdata( ).
        
        " Log the API call
        log_api_call( iv_method      = iv_method
                      iv_path        = iv_path
                      iv_payload     = iv_payload
                      iv_status_code = lv_http_status
                      iv_response    = lv_response ).
        
        " Parse response
        IF lv_http_status >= 200 AND lv_http_status < 300.
          " Success case
          DATA: lr_data TYPE REF TO data.
          CREATE DATA lr_data TYPE string.
          
          TRY.
              convert_json_to_abap( EXPORTING iv_json = lv_response
                                    IMPORTING er_data = lr_data ).
                                    
              rs_response = VALUE #( status_code    = lv_http_status
                                    message        = 'Success'
                                    correlation_id = lo_http_client->request->get_header_field( 'X-Correlation-ID' )
                                    timestamp      = utclong_current( )
                                    payload        = lr_data ).
          CATCH cx_root INTO DATA(lx_parse).
            rs_response = handle_error( iv_message = |Error parsing response: { lx_parse->get_text( ) }|
                                        iv_status_code = 500 ).
          ENDTRY.
        ELSE.
          " Error case
          rs_response = handle_error( iv_message = |API returned error: { lv_response }|
                                      iv_status_code = lv_http_status ).
        ENDIF.
        
        " Close HTTP connection
        lo_http_client->close( ).
        
    CATCH cx_root INTO DATA(lx_error).
        rs_response = handle_error( iv_message = |HTTP client error: { lx_error->get_text( ) }| ).
    ENDTRY.
  ENDMETHOD.

  METHOD handle_error.
    " Log error
    mo_logger->error( iv_message ).
    
    " Create error response
    rs_response = VALUE #( status_code    = iv_status_code
                          message        = iv_message
                          correlation_id = generate_correlation_id( )
                          timestamp      = utclong_current( ) ).
  ENDMETHOD.

  METHOD log_api_call.
    " Sanitize payload and response for logging
    DATA(lv_sanitized_payload) = sanitize_for_logging( iv_payload ).
    DATA(lv_sanitized_response) = sanitize_for_logging( iv_response ).
    
    " Log API call
    mo_logger->info( |API Call: { iv_method } { iv_path } | &&
                     |Status: { iv_status_code } | &&
                     |Payload: { lv_sanitized_payload }| ).
                     
    " Log detailed info for debugging if needed
    IF mo_logger->get_log_level( ) = /goog/cl_logging=>c_log_level-debug.
      mo_logger->debug( |Response: { lv_sanitized_response }| ).
    ENDIF.
  ENDMETHOD.

  METHOD sanitize_for_logging.
    " This method sanitizes sensitive information before logging
    " Important for security and compliance
    
    " This is a simplified example - a real implementation would be more comprehensive
    DATA(lv_result) = iv_data.
    
    " Remove sensitive fields
    REPLACE ALL OCCURRENCES OF REGEX '"password":\s*"[^"]*"' IN lv_result WITH '"password":"***"'.
    REPLACE ALL OCCURRENCES OF REGEX '"api_key":\s*"[^"]*"' IN lv_result WITH '"api_key":"***"'.
    REPLACE ALL OCCURRENCES OF REGEX '"token":\s*"[^"]*"' IN lv_result WITH '"token":"***"'.
    
    " Truncate long content for readability
    IF strlen( lv_result ) > 500.
      lv_result = substring( val = lv_result len = 500 ) && '... [truncated]'.
    ENDIF.
    
    RETURN lv_result.
  ENDMETHOD.

  METHOD perform_security_validation.
    " Placeholder for security validation logic
    " This would include checks for:
    " - SQL injection attempts
    " - XSS payloads
    " - Command injection
    " - etc.
    
    " For this example, we'll just return true
    rv_valid = abap_true.
  ENDMETHOD.

  METHOD generate_correlation_id.
    " Generate a unique correlation ID for request tracing
    DATA: lv_guid TYPE guid_32.
    
    TRY.
        lv_guid = cl_system_uuid=>create_uuid_c32_static( ).
        rv_correlation_id = lv_guid.
    CATCH cx_uuid_error.
        " Fallback if UUID generation fails
        GET TIME STAMP FIELD DATA(lv_timestamp).
        rv_correlation_id = |{ sy-uname }-{ lv_timestamp }|.
    ENDTRY.
  ENDMETHOD.

  METHOD get_transport_info.
    " Validate input
    IF iv_transport_id IS INITIAL.
      rs_response = handle_error( 'Transport ID cannot be empty' ).
      RETURN.
    ENDIF.
    
    " Call transport API
    rs_response = call_api( iv_method = 'GET'
                           iv_path   = |/transport/{ iv_transport_id }| ).
  ENDMETHOD.

  METHOD create_transport.
    " Validate input
    IF iv_description IS INITIAL OR iv_target IS INITIAL.
      rs_response = handle_error( 'Description and target are required fields' ).
      RETURN.
    ENDIF.
    
    " Prepare payload
    DATA: ls_payload TYPE string.
    DATA: ls_transport TYPE ty_transport_request.
    
    ls_transport-description = iv_description.
    ls_transport-target = iv_target.
    ls_transport-owner = COND #( WHEN iv_owner IS INITIAL THEN sy-uname
                                ELSE iv_owner ).
    
    TRY.
        ls_payload = convert_abap_to_json( ls_transport ).
    CATCH cx_static_check INTO DATA(lx_convert).
        rs_response = handle_error( |JSON conversion error: { lx_convert->get_text( ) }| ).
        RETURN.
    ENDTRY.
    
    " Call transport API
    rs_response = call_api( iv_method  = 'POST'
                           iv_path    = '/transport/create'
                           iv_payload = ls_payload ).
  ENDMETHOD.

  METHOD release_transport.
    " Validate input
    IF iv_transport_id IS INITIAL OR iv_target IS INITIAL.
      rs_response = handle_error( 'Transport ID and target are required fields' ).
      RETURN.
    ENDIF.
    
    " Prepare payload
    DATA: ls_payload TYPE string.
    DATA: ls_release TYPE string.
    
    ls_release = |{ "transportId": "{ iv_transport_id }", | &&
                 |"targetSystem": "{ iv_target }" }|.
    
    " Call transport API
    rs_response = call_api( iv_method  = 'POST'
                           iv_path    = '/transport/release'
                           iv_payload = ls_release ).
  ENDMETHOD.

  METHOD abapgit_pull.
    " Validate input
    IF iv_package IS INITIAL.
      rs_response = handle_error( 'Package is a required field' ).
      RETURN.
    ENDIF.
    
    " Prepare payload
    DATA: ls_payload TYPE string.
    DATA: ls_pull TYPE string.
    
    ls_pull = |{ "package": "{ iv_package }", | &&
              |"branch": "{ COND #( WHEN iv_branch IS INITIAL THEN 'main' ELSE iv_branch ) }", | &&
              |"transportRequest": "{ iv_transport }", | &&
              |"commitMessage": "{ iv_commit_message }" }|.
    
    " Call abapGit API
    rs_response = call_api( iv_method  = 'POST'
                           iv_path    = '/abapgit/pull'
                           iv_payload = ls_pull ).
  ENDMETHOD.

  METHOD abapgit_push.
    " Validate input
    IF iv_package IS INITIAL OR iv_transport IS INITIAL OR iv_commit_message IS INITIAL.
      rs_response = handle_error( 'Package, transport, and commit message are required fields' ).
      RETURN.
    ENDIF.
    
    " Prepare payload
    DATA: ls_payload TYPE string.
    DATA: ls_push TYPE string.
    
    ls_push = |{ "package": "{ iv_package }", | &&
             |"branch": "{ COND #( WHEN iv_branch IS INITIAL THEN 'main' ELSE iv_branch ) }", | &&
             |"transportRequest": "{ iv_transport }", | &&
             |"commitMessage": "{ iv_commit_message }" }|.
    
    " Call abapGit API
    rs_response = call_api( iv_method  = 'POST'
                           iv_path    = '/abapgit/push'
                           iv_payload = ls_push ).
  ENDMETHOD.

  METHOD abapgit_status.
    " Validate input
    IF iv_package IS INITIAL.
      rs_response = handle_error( 'Package is a required field' ).
      RETURN.
    ENDIF.
    
    " Call abapGit status API
    rs_response = call_api( iv_method = 'GET'
                           iv_path   = |/abapgit/status?package={ iv_package }| ).
  ENDMETHOD.

  METHOD set_github_token.
    " Validate input
    IF iv_token IS INITIAL.
      rs_response = handle_error( 'Token cannot be empty' ).
      RETURN.
    ENDIF.
    
    " Prepare payload
    DATA: ls_payload TYPE string.
    DATA: ls_token TYPE string.
    
    ls_token = |{ "token": "{ iv_token }" }|.
    
    " Call token API
    rs_response = call_api( iv_method  = 'POST'
                           iv_path    = '/github/token'
                           iv_payload = ls_token ).
                           
    " In a real implementation, the token would be securely stored
    " rather than just passed to the API
  ENDMETHOD.

ENDCLASS.
