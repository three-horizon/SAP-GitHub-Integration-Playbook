# 📄 Prerequisites for SAP-GitHub Integration

## 📋 Table of Contents

- [Overview](#overview)
- [SAP Environment Requirements](#sap-environment-requirements)
  - [Compatible SAP Versions](#compatible-sap-versions)
  - [Required SAP Notes](#required-sap-notes)
  - [Configuration Requirements](#configuration-requirements)
- [GitHub Requirements](#github-requirements)
- [Azure Requirements](#azure-requirements)
- [Network Requirements](#network-requirements)
- [Security Requirements](#security-requirements)
- [User Permissions](#user-permissions)
- [Skill Requirements](#skill-requirements)
- [Verification Checklist](#verification-checklist)

## 📋 Overview

This document outlines the prerequisites for implementing the SAP-GitHub integration. Ensuring these requirements are met before beginning the implementation will help avoid issues and ensure a smooth setup process.

## SAP Environment Requirements

### Compatible SAP Versions

#### SAP NetWeaver

| Version | Support Status | Notes |
|---------|---------------|-------|
| SAP NetWeaver 7.0x | Limited Support | Basic functionality only, some features may not work |
| SAP NetWeaver 7.1x | Limited Support | Basic functionality only, some features may not work |
| SAP NetWeaver 7.3x | Supported | Most features supported |
| SAP NetWeaver 7.4x | Fully Supported | Recommended minimum version |
| SAP NetWeaver 7.5x | Fully Supported | Recommended for optimal functionality |
| SAP NetWeaver 7.6x | Fully Supported | Recommended for optimal functionality |

#### SAP Business Suite / ERP

| Version | Support Status | Notes |
|---------|---------------|-------|
| SAP ECC 6.0 | Supported with EHP | Requires EHP 7 or higher |
| SAP ECC 6.0 EHP 7 | Fully Supported | Minimum recommended EHP version |
| SAP ECC 6.0 EHP 8 | Fully Supported | Recommended for optimal functionality |
| SAP S/4HANA 1511 | Supported | Most features supported |
| SAP S/4HANA 1610 | Fully Supported | |
| SAP S/4HANA 1709 | Fully Supported | |
| SAP S/4HANA 1809 | Fully Supported | Recommended version |
| SAP S/4HANA 1909 | Fully Supported | Recommended version |
| SAP S/4HANA 2020 | Fully Supported | Recommended version |
| SAP S/4HANA 2021 | Fully Supported | Recommended version |
| SAP S/4HANA 2022 | Fully Supported | Recommended version |

#### SAP BTP ABAP Environment

| Version | Support Status | Notes |
|---------|---------------|-------|
| Cloud ABAP | Fully Supported | Requires specific configuration |

### Required SAP Notes

Install the following SAP Notes before beginning the implementation:

| SAP Note | Title | Purpose | Priority |
|----------|-------|---------|----------|
| 2297281 | abapGit | Fixes issues related to abapGit compatibility | Required |
| 2655547 | OAuth authentication for external interfaces | Enables OAuth authentication for API integration | Required |
| 2573788 | Security for external integrations | Enhances security for external system connections | Required |
| 2937996 | Support packages for abapGit | Provides latest support packages for abapGit | Required |
| 2960908 | API Management and OData integration | Improves API connectivity | Recommended |
| 2408073 | Git client integration | Fixes issues with Git client connectivity | Recommended |
| 2454582 | Transport Management System connectivity | Improves transport handling | Recommended |

### Configuration Requirements

- SAP_BASIS 7.40 or higher
- SAP Gateway service activated
- SICF services for API endpoints activated
- Appropriate client configuration
- UTF-8 system recommended
- RFC destinations configured
- Development/modification authorization
- Transport Management System configured

## GitHub Requirements

- GitHub Enterprise Cloud or Server (version 3.0 or higher)
- Administrator access to create repositories and configure settings
- Ability to create and manage GitHub Actions workflows
- Access to configure webhooks
- Ability to create and manage Personal Access Tokens
- Organization-level settings access for:
  - Repository creation
  - Branch protection rules
  - Action workflows
  - Security settings

## Azure Requirements

- Azure subscription with appropriate permissions
- Ability to create and manage Azure resources:
  - Azure API Management
  - Azure Key Vault
  - Azure Monitor
  - Microsoft Sentinel (for security monitoring)
  - Microsoft Defender for Cloud (optional)
- Network connectivity between Azure and SAP environments
- Network connectivity between Azure and GitHub Enterprise

## Network Requirements

- HTTPS connectivity between SAP systems and API Management
- HTTPS connectivity between GitHub and API Management
- Network security group configurations allowing required traffic
- Proxy server configurations (if applicable)
- Firewall rules for:
  - SAP systems to Azure API Management
  - GitHub to Azure API Management
  - SAP systems to GitHub (if direct connection is used)
- DNS resolution for all systems
- If using on-premises systems, consider:
  - Azure ExpressRoute
  - VPN Connectivity
  - Proxy configurations

## 🛡️ Security Requirements

- SSL/TLS certificates for secure communications
- Token-based authentication capabilities
- Security monitoring infrastructure
- Data protection policies compliance
- Identity and access management system
- Secrets management solution (Azure Key Vault recommended)
- Compliance with organization security policies
- Network security controls
- Audit logging capabilities

## User Permissions

### SAP User Permissions

- SAP_ALL profile access or the following specific authorizations:
  - S_DEVELOP - For development objects
  - S_TCODE - For transaction access
  - S_CTS_ADMI - For transport management
  - S_RFC - For RFC calls
  - S_SERVICE - For external services
  - S_USER_ADM - For user administration (if creating technical users)

### GitHub User Permissions

- Organization Owner or Admin role
- Repository Admin permissions
- Ability to create and manage:
  - Repositories
  - Branches
  - Pull requests
  - Webhooks
  - GitHub Actions workflows
  - Secrets

### Azure User Permissions

- Contributor or Owner role for resource group
- Service Administrator for API Management
- Key Vault Administrator for secrets management
- Monitoring Contributor for logging and monitoring

## Skill Requirements

Team members involved in the implementation should have knowledge in:

- ABAP development
- Git/GitHub administration
- Azure cloud services
- API Management
- DevOps practices
- Security principles and implementation
- Network configuration
- CI/CD pipelines

## Verification Checklist

Use this checklist to verify all prerequisites are met before beginning implementation:

- [ ] SAP system meets minimum version requirements
- [ ] All required SAP Notes are installed
- [ ] SAP Gateway is configured and operational
- [ ] Technical users with required authorizations are created
- [ ] GitHub Enterprise environment is available
- [ ] GitHub administrator access is confirmed
- [ ] Azure subscription with required permissions is available
- [ ] Network connectivity between all systems is confirmed
- [ ] Firewall rules are configured to allow required traffic
- [ ] Security requirements are understood and can be implemented
- [ ] Team has the necessary skills to implement and maintain the solution
- [ ] Change management process is in place for the implementation
- [ ] SAP BASIS team is informed and onboard with the implementation
- [ ] Security team has reviewed and approved the architecture

# 📋 Prerequisites

<div align="center">
  <img src="../../assets/images/architecture/high-level-architecture.svg" width="700" alt="Prerequisites">
  
  *Required components and configurations for SAP-GitHub Integration*
</div>


