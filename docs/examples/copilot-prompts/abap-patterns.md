# ABAP Prompting Patterns for GitHub Copilot

This document provides effective prompting patterns for SAP ABAP development with GitHub Copilot. These patterns leverage context awareness to produce higher quality code suggestions that align with SAP development standards and best practices.

## General ABAP Prompting Guidelines

### 1. Provide Clear Context

Always include relevant context in your prompts to get more precise suggestions:

- ABAP version (7.40, 7.50, 7.52, etc.)
- SAP system type (ECC, S/4HANA)
- Development object type (Class, Function Module, Report, etc.)
- Relevant dependencies and interfaces
- Transport ID if applicable

**Example:**
```
Write an ABAP class to validate customer numbers using ABAP 7.52 syntax for S/4HANA.
The class should implement ZIF_CUSTOMER_VALIDATOR interface and be transport-ready for TR#K901234.
```

### 2. Specify Coding Standards

Include your organization's coding standards or reference SAP's clean ABAP guidelines:

**Example:**
```
Generate an ABAP method to process sales orders following clean ABAP guidelines.
Use camelCase for variables, descriptive naming, and proper error handling.
Method should validate input parameters and raise exceptions for invalid inputs.
```

### 3. Include Technical Requirements

Specify performance, security, or other technical requirements:

**Example:**
```
Create an efficient ABAP function module to read large tables with millions of records.
Optimize for memory usage and consider using package size processing.
Add proper authority checks for S_TABU_DIS authorization object.
```

## Transport-Aware Prompts

### 1. Create Transport-Ready Code

When working with SAP transports, specify the transport details in your prompt:

**Example:**
```
Generate ABAP code for transport K902345 that implements a data extraction report.
Include transport references in the code documentation and consider that this will be
transported from DEV to QAS and eventually to PRD systems. Include necessary package assignments.
```

### 2. Transport Analysis

Ask Copilot to analyze transport contents:

**Example:**
```
Analyze the following transport objects for K903456 and suggest potential issues 
or improvements:

Objects:
- Class ZCL_SALES_PROCESSING
- Function group ZSALES
- DDIC table ZSALES_HEADER
- DDIC structure ZSALES_ITEM
```

## Context-Aware Development Prompts

### 1. Code with Context References

Reference existing context when requesting new code:

**Example:**
```
Based on the SAP transport K904567 containing ZCL_CUSTOMER_MASTER, write a test class
that thoroughly tests the customer validation functionality. Use ABAP Unit and include
test cases for all edge cases mentioned in the requirements ER-2345.
```

### 2. SAP-Specific Context

Include SAP-specific concepts in your prompts:

**Example:**
```
Write an ABAP class to handle BAPI_CUSTOMER_GETLIST with proper error handling.
Include RFC destination parameter and handle the BAPI return structure.
The class should follow Factory pattern and be mockable for unit tests.
```

## Common ABAP Development Scenarios

### 1. Database Access

```
Create an ABAP class to access the VBAK and VBAP tables using modern ABAP 7.40+ SQL.
Implement efficient filtering and handle large result sets. Follow SELECT * FROM
VBAK WHERE VBELN IN @lt_vbeln INTO TABLE @DATA(lt_vbak) pattern.
```

### 2. ALV Grid Display

```
Implement an ABAP class to display sales data in ALV Grid using SALV_CL_* classes.
Include features for sorting, filtering, and column customization. Implement event
handling for user clicks on list entries.
```

### 3. OData Service Implementation

```
Generate ABAP code for implementing an OData service for customer data retrieval.
Use the SEGW model and implement the ZCL_Z_CUSTOMER_DPC_EXT provider class.
Include entity sets for Customers and Sales, with navigation between them.
```

### 4. Background Job Scheduling

```
Write ABAP code to create and schedule a background job that processes pending
orders daily at 2:00 AM. Include proper error handling and job log monitoring.
```

### 5. BADI Implementation

```
Create an implementation of BAdI BADI_SD_SALESDOCUMENT to extend the standard
sales document creation process. Add custom validation for payment terms based
on customer credit rating.
```

## Integration with MCP Context

### 1. Context-Enhanced Code Generation

Reference context IDs from the Context Store:

**Example:**
```
Using context ID CTX-12345 from the transport K905678, generate ABAP code to
implement the missing parts of the ZCL_INVOICE_PROCESSOR class. Consider the
related GitHub PR #456 mentioned in the context.
```

### 2. Cross-System Development

Reference both SAP and GitHub contexts:

**Example:**
```
Review the changes in GitHub PR #789 (context ID CTX-23456) and generate
the corresponding ABAP class implementations that would need to be transported
in SAP. Consider dependencies on existing function modules ZFUN_GET_CUSTOMER
and ZFUN_VALIDATE_ORDERS.
```

## Security-Focused Prompts

### 1. Code Security Analysis

```
Analyze this ABAP code for security vulnerabilities, particularly focusing on
SQL injection, authority checks, and sensitive data handling:

```abap
REPORT z_process_customers.
DATA: lv_customer_id TYPE kunnr.
SELECT-OPTIONS: s_kunnr FOR lv_customer_id.

START-OF-SELECTION.
  DATA(lv_where_clause) = |KUNNR IN | && cl_abap_dyn_prg=>format_sql_for_where_clause( s_kunnr ).
  
  SELECT * FROM kna1 
    WHERE (lv_where_clause)
    INTO TABLE @DATA(lt_customers).
    
  LOOP AT lt_customers ASSIGNING FIELD-SYMBOL(<fs_customer>).
    WRITE: / <fs_customer>-kunnr, <fs_customer>-name1.
  ENDLOOP.
```

### 2. Authority Check Implementation

```
Generate ABAP code with proper authority checks for a transaction that allows
users to modify sensitive customer credit data. Include checks for S_TCODE,
S_TABU_DIS, and any custom authorization objects needed.
```

## Performance Optimization Prompts

### 1. Code Performance Analysis

```
Analyze this ABAP code for performance issues and suggest optimizations:

```abap
METHOD process_sales_documents.
  DATA: lt_vbak    TYPE TABLE OF vbak,
        lt_vbap    TYPE TABLE OF vbap,
        ls_result  TYPE ty_result.
        
  SELECT * FROM vbak INTO TABLE lt_vbak WHERE erdat GE iv_date.
  
  LOOP AT lt_vbak ASSIGNING FIELD-SYMBOL(<fs_vbak>).
    SELECT * FROM vbap INTO TABLE lt_vbap WHERE vbeln = <fs_vbak>-vbeln.
    
    LOOP AT lt_vbap ASSIGNING FIELD-SYMBOL(<fs_vbap>).
      APPEND VALUE ty_result( vbeln = <fs_vbak>-vbeln
                             posnr = <fs_vbap>-posnr
                             matnr = <fs_vbap>-matnr
                             amount = <fs_vbap>-netwr ) TO rt_results.
    ENDLOOP.
  ENDLOOP.
ENDMETHOD.
```

### 2. Memory Optimization

```
Refactor this ABAP code to use less memory when processing large datasets:

```abap
METHOD read_material_data.
  SELECT * FROM mara INTO TABLE @DATA(lt_mara).
  SELECT * FROM marc INTO TABLE @DATA(lt_marc).
  SELECT * FROM mard INTO TABLE @DATA(lt_mard).
  
  LOOP AT lt_mara ASSIGNING FIELD-SYMBOL(<fs_mara>).
    READ TABLE lt_marc ASSIGNING FIELD-SYMBOL(<fs_marc>)
      WITH KEY matnr = <fs_mara>-matnr.
    IF sy-subrc = 0.
      LOOP AT lt_mard ASSIGNING FIELD-SYMBOL(<fs_mard>)
        WHERE matnr = <fs_mara>-matnr.
        " Process data
      ENDLOOP.
    ENDIF.
  ENDLOOP.
ENDMETHOD.
```

## Best Practices for ABAP Prompting

1. **Iterative Refinement**: Start with a general prompt, then refine with more specifics
2. **Reference SAP Documentation**: Mention specific SAP Notes or documentation
3. **Include Business Context**: Explain the business purpose of the code
4. **Specify Error Handling**: Clarify how errors should be handled
5. **Request Comments**: Ask for explanatory comments in complex code
6. **Test Cases**: Request example test cases for the generated code

## Related Documentation

- [GitHub Copilot Setup](../../docs/implementation/github-copilot-setup.md)
- [Context-Aware Development](../../docs/development/context-aware-development.md)
- [SAP Integration Security](../../docs/security/context-aware-security.md)

## External References

1. [SAP ABAP Development Style Guide](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md)
2. [SAP Help Portal: ABAP Programming](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm)
3. [GitHub Copilot Documentation](https://docs.github.com/en/copilot) 
