---
title: abapGit Configuration Guide
description: Detailed configuration instructions for abapGit 1.128.0+ in SAP systems
author: SAP-GitHub Integration Team
date: 2024-05-15
version: 1.0.0
---

# abapGit Configuration Guide

## Table of Contents

- [Overview](#overview)
- [Basic Configuration](#basic-configuration)
  - [User Settings](#user-settings)
  - [Repository Settings](#repository-settings)
- [Advanced Configuration](#advanced-configuration)
  - [HTTP/1.1 Protocol Configuration](#http11-protocol-configuration)
  - [SSL/TLS Settings](#ssltls-settings)
  - [Proxy Configuration](#proxy-configuration)
  - [Authentication](#authentication)
- [Working with Different Object Types](#working-with-different-object-types)
  - [Standard ABAP Objects](#standard-abap-objects)
  - [Event Consumption Models (EEEC)](#event-consumption-models-eeec)
  - [Knowledge Transfer Documents (SKTD)](#knowledge-transfer-documents-sktd)
  - [API Release State Objects (APIS)](#api-release-state-objects-apis)
- [UI Configuration](#ui-configuration)
  - [Theme Selection](#theme-selection)
  - [View Customization](#view-customization)
  - [Keyboard Shortcuts](#keyboard-shortcuts)
- [Performance Optimization](#performance-optimization)
  - [Folder Logic Configuration](#folder-logic-configuration)
  - [Memory Management](#memory-management)
  - [Serialization Options](#serialization-options)
- [Troubleshooting Configuration Issues](#troubleshooting-configuration-issues)
- [Next Steps](#next-steps)
- [Reference](#reference)

## Overview

This guide covers comprehensive configuration options for abapGit version 1.128.0 and later, focusing on optimal setup for SAP-GitHub integration. Proper configuration ensures stable performance, secure connections, and efficient object handling.

## Basic Configuration

After [installing abapGit](abapgit-installation.md), configure these essential settings before connecting to GitHub repositories.

### User Settings

Configure your user settings to ensure proper attribution in git commits:

1. Start abapGit (transaction `ZABAPGIT` or execute program `ZABAPGIT_FULL`)
2. Click the **Settings** icon (⚙️) in the top-right corner
3. Enter your information:
   - **Git user name**: Your full name (e.g., `Jane Doe`)
   - **Git user email**: Your email address (e.g., `jane.doe@example.com`)
4. Optional: Set default settings:
   - **Show standard table as ALV grid**: Improves table display
   - **Show original object name**: Shows original object names for certain objects
   - **Show category icon in repo list**: Helps identify repository types
5. Click **Save**

[SCREENSHOT: abapGit user settings configuration screen]

### Repository Settings

For each repository you create or clone:

1. Navigate to the repository (or create a new one)
2. Click the **Settings** tab
3. Configure these key settings:
   - **Display name**: Friendly name for the repository
   - **Package**: SAP package where objects will be stored
   - **Branch**: Git branch to work with (typically `main` or `master`)
   - **Transport request**: Optional transport for changes
   - **Folder logic**: How objects are organized (PREFIX or FULL)
   - **Ignore files**: Files to exclude from versioning

#### Folder Logic Options

- **PREFIX**: Objects are placed in folders based on object type prefixes
  - Recommended for most scenarios
  - More efficient folder structure
  - Better handling of large repositories
- **FULL**: Uses full package path for folder structure
  - Maintains SAP package hierarchy
  - More complex folder structure
  - Better for visualizing package relationships

[SCREENSHOT: Repository settings screen with folder logic options]

## Advanced Configuration

### HTTP/1.1 Protocol Configuration

Version 1.128.0+ leverages HTTP/1.1 by default, offering significant performance improvements:

1. In repository settings, the "Use HTTP v1.1" option is now enabled by default
2. Benefits include:
   - **Persistent connections**: Reduces connection overhead
   - **Request pipelining**: Allows multiple requests without waiting for responses
   - **Chunked transfers**: Better handling of large files
   - **Header compression**: Reduces network traffic

To optimize HTTP/1.1 performance:

1. Go to repository settings
2. In the "Advanced" section, configure:
   - **Max HTTP requests**: Set to 6-10 for optimal performance (default: 8)
   - **Connection timeout**: Recommended 120-180 seconds (default: 120)
   - **Enable gzip compression**: Recommended for slower networks

```abap
" Typical HTTP/1.1 settings in abapGit background code
lo_settings->set_http_version( 'HTTP/1.1' ).
lo_settings->set_max_connections( 8 ).
lo_settings->set_timeout( 120 ).
lo_settings->set_gzip_compression( abap_true ).
```

### SSL/TLS Settings

For secure connections to GitHub:

1. Go to the **Settings** menu
2. Configure SSL options:
   - **SSL verify**: Enable for production (recommended)
   - **SSL ID**: Enter your system's SSL client identity

If you encounter SSL issues:

1. Use transaction `STRUST` to import necessary certificates
2. Add GitHub's certificate chain to the SSL client (Standard)
3. Configure proxy SSL settings if behind a corporate proxy

### Proxy Configuration

If your SAP system requires a proxy for external connections:

1. Go to **Settings**
2. Configure proxy settings:
   - **Proxy URL**: Your proxy server address (e.g., `http://proxy.example.com:8080`)
   - **Proxy bypass**: Hosts to exclude from proxy (e.g., `localhost,*.internal.example.com`)
   - **Proxy authentication**: User/password if required

For systems with complex proxy requirements, use the SAP profile parameters:

```
icm/HTTP/proxy_host = proxy.example.com
icm/HTTP/proxy_port = 8080
icm/HTTP/proxy_user = [username]
icm/HTTP/proxy_password = [password]
```

### Authentication

Configure authentication for private repositories:

1. In repository settings, go to the **Authentication** section
2. Choose the authentication method:
   - **HTTP(S)**: Username/password or token (recommended)
   - **SSH**: SSH keys (advanced setup)

For token-based authentication (recommended):

1. Create a Personal Access Token in GitHub with `repo` scope
2. Use this token instead of your password in abapGit
3. For enhanced security, use fine-grained tokens with limited repository access

[SCREENSHOT: GitHub token creation screen with required permissions]

## Working with Different Object Types

### Standard ABAP Objects

abapGit handles standard object types automatically. No special configuration is needed for:

- Programs (PROG)
- Classes and Interfaces (CLAS/INTF)
- Function Groups (FUGR)
- Data Dictionary Objects (TABL, DTEL, DOMA, etc.)
- Business Object Processing Framework (BOPF)
- Core Data Services (CDS)

### Event Consumption Models (EEEC)

For Event Consumption Models (new in version 1.128.0+):

1. Ensure your SAP system supports Event Consumption (S/4HANA 1909 or higher)
2. Configure in repository settings:
   - Enable "Include event consumption models" option
   - Set "Event object handling" to "Full serialization"

Event model objects are stored in XML files with detailed metadata:

```xml
<!-- Example EEEC XML structure -->
<eeec:event_consumption_model>
  <eeec:header>
    <eeec:name>ZCM_BUSINESS_PARTNER_CREATED</eeec:name>
    <eeec:description>Business Partner Created</eeec:description>
    <eeec:version>1.0</eeec:version>
  </eeec:header>
  <eeec:event_definition>
    <eeec:event_name>BUSINESSPARTNER.CREATED</eeec:event_name>
    <eeec:namespace>sap.s4.beh</eeec:namespace>
  </eeec:event_definition>
  <!-- Additional elements -->
</eeec:event_consumption_model>
```

To work with Event Consumption Models:

1. Create/modify Event Consumption Models in transaction SEEC
2. Stage and commit changes using abapGit
3. When pulling to target systems, ensure Event Mesh connectivity is configured

### Knowledge Transfer Documents (SKTD)

For Knowledge Transfer Documents:

1. Enable in repository settings:
   - Set "Include documentation objects" to "Yes"
   - Set "Documentation handling" to "Full content" (includes media)

For optimal handling:

1. Store large media files in separate repositories
2. Use relative links when referring to external resources
3. Configure "Media compression" based on document size:
   - "None" for small documents
   - "Standard" for medium documents
   - "Maximum" for large documents with media

### API Release State Objects (APIS)

API Release State Objects require additional configuration:

1. In repository settings under "API Management":
   - Enable "Include API states"
   - Set "API handling method" to "Complete" (tracks all versions)
   - Configure "API dependency tracking" based on needs

API state synchronization options:

- **Standard**: Basic version tracking
- **Complete**: Tracks all version history
- **Reference only**: Stores references without implementation details

## UI Configuration

### Theme Selection

Version 1.128.0+ introduces theme options:

1. Go to **Settings** > **Appearance**
2. Select from available themes:
   - **Light** (default): Standard SAP GUI compatible theme
   - **Dark**: Reduced eye strain for long development sessions
   - **System**: Follows SAP GUI theme settings

[SCREENSHOT: abapGit theme selection with dark mode preview]

### View Customization

Customize abapGit views:

1. Go to **Settings** > **Display Options**
2. Configure:
   - **Show repository description**: Adds context in repository list
   - **Repository list display mode**: Compact/Detailed
   - **Max items per page**: Adjust based on preference (default: 25)
   - **Show category icon**: Visual indicators for repository types
   - **Show repository state**: Quick status indicators

### Keyboard Shortcuts

Enable and customize keyboard shortcuts:

1. Go to **Settings** > **Keyboard**
2. Available shortcuts include:
   - `Alt+P`: Pull changes
   - `Alt+C`: Commit changes
   - `Alt+B`: Switch branch
   - `Alt+S`: Stage changes
   - `Alt+D`: Show diff
   - `Alt+H`: Show help

## Performance Optimization

### Folder Logic Configuration

Optimize folder handling for your workflow:

1. For new repositories, select the appropriate folder logic:
   - **PREFIX**: More efficient for large repositories
   - **FULL**: Better for visualizing package structure
2. For existing repositories, changing folder logic requires:
   - Full export of all objects
   - Repository recreation with new logic
   - Import of all objects

### Memory Management

Optimize memory usage for large repositories:

1. Go to **Settings** > **Performance**
2. Configure:
   - **Parallel processing**: Number of parallel threads (2-4 recommended)
   - **Commit bundle size**: Maximum objects per commit (100-500 recommended)
   - **Memory limit per operation**: Maximum MB per operation (default: 100)

```abap
" Example of optimized memory settings
lo_settings->set_parallel_proc_disabled( abap_false ).
lo_settings->set_parallel_proc_threads( 4 ).
lo_settings->set_commit_bundle_size( 200 ).
```

### Serialization Options

Configure serialization for improved performance:

1. In repository settings under "Serialization":
   - **Object serialization mode**: Standard/Optimized/Compatible
   - **Serialization format**: XML/JSON (XML recommended for compatibility)
   - **Include unchanged objects**: Set to "No" for faster operations

Serialization mode options:

- **Standard**: Default, balanced performance
- **Optimized**: Faster but may exclude some metadata
- **Compatible**: Includes all metadata, slower but most reliable

## Troubleshooting Configuration Issues

Common configuration issues and solutions:

| Issue | Solution |
|-------|----------|
| HTTP/1.1 connection failures | Check proxy settings and firewall rules<br>Verify SSL certificates<br>Try disabling chunked transfers |
| Memory consumption too high | Reduce parallel threads<br>Lower commit bundle size<br>Process larger repositories in smaller chunks |
| SSL handshake failures | Verify certificates in STRUST<br>Check if GitHub certificates are imported<br>Temporarily disable SSL verification for testing |
| Object serialization errors | Use compatible serialization mode<br>Check SAP Note 2937606 for known issues<br>Ensure all prerequisites are installed |
| Dark mode display issues | Update to latest abapGit version<br>Try "System" theme option<br>Ensure SAP GUI theme compatibility |

## Next Steps

After configuring abapGit:

- [Connect to GitHub repositories](../github-setup/repository-connection.md)
- [Set up transport integration](transport-layer.md)
- [Configure automated workflows](../../getting-started/automation.md)
- [Implement development standards](development-standards.md)

## Reference

- [Official abapGit Documentation](https://docs.abapgit.org/user-guide/setup/settings.html)
- [abapGit GitHub Repository](https://github.com/abapGit/abapGit)
- [SAP Note 2937606: abapGit HTTP/1.1 Support](https://launchpad.support.sap.com/#/notes/2937606)
- [SAP Community: abapGit Best Practices](https://blogs.sap.com/tags/73555000100800001164/)

---

*Last Updated: 2024-05-15 for abapGit version 1.128.0*


