# Environment-Specific Configurations

This document outlines the specialized configurations required for different deployment scenarios of SAP-GitHub integration, addressing on-premises, cloud-hosted, and hybrid environments along with secure connectivity options.

## Overview

SAP-GitHub integration can be deployed in various environment configurations, each with unique requirements for connectivity, security, and operational considerations. This document provides architectural guidance for implementing integration across these different deployment scenarios.

## Environment Configuration Scenarios

The integration architecture supports the following key deployment scenarios:

1. **On-premises GitHub Enterprise with on-premises SAP** - Both systems deployed within the corporate network
2. **Cloud-hosted GitHub with on-premises SAP** - GitHub in the cloud connecting to on-premises SAP systems
3. **On-premises GitHub Enterprise with SAP BTP** - GitHub Enterprise on-premises connecting to cloud SAP services
4. **Cloud-hosted GitHub with SAP BTP** - Both systems in the cloud with specific connectivity requirements
5. **Hybrid environments** - Mixed deployments with distributed components across on-premises and cloud environments

## On-Premises GitHub Enterprise with On-Premises SAP

### Architecture Overview

```mermaid
flowchart TD
    A[GitHub Enterprise Server] -->|Internal Network| B[Integration Layer]
    B -->|Internal Network| C[SAP Systems]
    D[Developer Workstations] -->|Internal Network| A
    D -->|SAP GUI/Web UI| C
    E[CI/CD Runners] -->|Internal Network| B
    F[Network Firewall] -->|Protects| A
    F -->|Protects| C
    G[Corporate Identity Provider] -->|Authentication| A
    G -->|Authentication| C
```

### Configuration Components

1. **Network Configuration**
   - Internal network connectivity
   - Firewall rules for required ports
   - Network segmentation best practices
   - Load balancing for high availability

2. **GitHub Enterprise Configuration**
   - Hardware/VM sizing recommendations
   - High availability configuration
   - Storage configuration
   - Backup integration

3. **SAP System Connectivity**
   - Direct RFC connections
   - HTTP/HTTPS connections
   - Database connectivity
   - File system access

4. **Authentication Integration**
   - SAML configuration
   - LDAP integration
   - Single Sign-On setup
   - Service account management

### Implementation Requirements

#### Network Requirements

| Component | Ports | Protocol | Direction | Purpose |
|-----------|-------|----------|-----------|---------|
| GitHub Enterprise | 22 | SSH | Inbound | Git operations over SSH |
| GitHub Enterprise | 80/443 | HTTP/HTTPS | Inbound | Web interface, API access |
| GitHub Enterprise | 8080/8443 | HTTP/HTTPS | Inbound | Management console |
| GitHub Enterprise | 122 | SSH | Inbound | Administrative shell |
| GitHub Enterprise | 1194 | UDP | Outbound | GitHub Enterprise clustering |
| GitHub Enterprise | 8080/8443 | HTTP/HTTPS | Outbound | GitHub Enterprise clustering |
| SAP NetWeaver | 3200-3299 | TCP | Inbound | SAP Gateway |
| SAP NetWeaver | 443, 8000-8099 | HTTP/HTTPS | Inbound | ICM HTTP(S) access |
| SAP NetWeaver | 3300-3399 | TCP | Inbound | SAP Message Server |
| SAP HANA | 30013-30015 | TCP | Inbound | SQL/MDX access |
| SAP HANA | 1128-1129 | TCP | Inbound | HANA Host Agent |
| SAP HANA | 43<xx> | TCP | Inbound | HANA Internal Web |

#### GitHub Enterprise Server Specifications

Example sizing for a medium enterprise deployment (500-3000 users):

```
# GitHub Enterprise Server Sizing

## Hardware Requirements
- CPUs: 16 cores
- Memory: 64 GB RAM
- Storage:
  - Root disk: 200 GB SSD
  - Data disk: 750 GB SSD
  
## High Availability Configuration
- Primary instance
- Replica instance
- Network load balancer
- Shared file system for repositories
- Database replication

## Backup Configuration
- Daily snapshot schedule
- Repository backup
- Database backup
- Configuration backup
- Retention period: 30 days
```

#### Example GitHub Enterprise Configuration (config.json)

```json
{
  "mysql": {
    "host": "localhost",
    "innodb-buffer-pool-size": "16GB",
    "innodb-log-file-size": "512MB",
    "max-connections": 500
  },
  "elasticsearch": {
    "enabled": true,
    "heap-size": "8GB"
  },
  "kafka": {
    "retention-period-minutes": 2880
  },
  "memcached": {
    "memory-allocation": "4GB"
  },
  "redis": {
    "maxmemory": "4GB"
  },
  "ghe-auth": {
    "allow-http-basic-auth": false,
    "server": {
      "url": "https://idp.example.com",
      "certificate": "cert-data"
    }
  },
  "ghe-backup": {
    "enabled": true,
    "location": "/data/backup",
    "retention-days": 30
  }
}
```

### Connectivity Implementation

The on-premises scenario typically leverages direct network connectivity:

1. **GitHub Enterprise to SAP**
   - Direct RFC connections for ABAP systems
   - HTTPS for API-based integration
   - Database connections through dedicated interfaces

2. **CI/CD Infrastructure**
   - On-premises build servers in same network
   - Virtual runners on corporate infrastructure
   - Direct connections to both GitHub and SAP

3. **Developer Access**
   - Internal network access to GitHub Enterprise
   - Standard SAP access methods (GUI, Browser)
   - Common identity for both systems

### Security Considerations

On-premises deployments have specific security requirements:

1. **Network Security**
   - Network segmentation for GitHub and SAP systems
   - Firewall rules limiting access to required services
   - Intrusion detection and prevention systems
   - Regular network security audits

2. **System Hardening**
   - Operating system hardening for GitHub Enterprise
   - Minimal required services and ports
   - Regular patching and updates
   - Security monitoring and logging

3. **Data Protection**
   - Encryption at rest for sensitive data
   - Transport encryption for all communications
   - Data classification and handling procedures
   - Access controls based on least privilege

## Cloud-Hosted GitHub with On-Premises SAP

### Architecture Overview

```mermaid
flowchart TD
    A[GitHub.com / GitHub Enterprise Cloud] -->|Secure Tunnel| B[Integration Gateway]
    B -->|Internal Network| C[SAP Systems]
    D[Developer Workstations] -->|Internet| A
    D -->|Internal/VPN| C
    E[Cloud-based CI/CD Runners] -->|Webhooks| A
    E -->|Secure Tunnel| B
    F[Corporate Firewall] -->|Protects| B
    F -->|Protects| C
    G[DMZ] -->|Contains| B
    H[Identity Provider] -->|Authentication| A
    H -->|Authentication| C
```

### Configuration Components

1. **Secure Connectivity Options**
   - VPN connections
   - Reverse proxy solutions
   - Enterprise gateway services
   - DMZ implementation

2. **GitHub Enterprise Cloud Configuration**
   - Organization setup
   - API permissions
   - OAuth applications
   - GitHub Actions configuration

3. **Connectivity Infrastructure**
   - Gateway server specifications
   - DMZ architecture
   - Traffic routing rules
   - High availability design

4. **Authentication Flow**
   - SAML configuration
   - OIDC integration
   - Access token management
   - Credential security

### Implementation Requirements

#### Secure Connectivity Options

Multiple secure connectivity options are available for connecting GitHub.com to on-premises SAP systems:

| Connectivity Option | Pros | Cons | Best For |
|---------------------|------|------|----------|
| Site-to-Site VPN | Secure, established technology, complete network integration | Complex setup, ongoing maintenance, all-or-nothing access | Large enterprises with existing VPN infrastructure |
| Reverse Proxy | Precise access control, DMZ compatibility, minimal footprint | Limited to HTTP/HTTPS, requires careful security configuration | Organizations preferring application-level control |
| GitHub Enterprise Gateway | Purpose-built for GitHub, self-updates, minimal configuration | Limited to GitHub actions and runners | Organizations focused on CI/CD integration |
| SAP Cloud Connector | Native SAP integration, designed for SAP security model | Additional component to maintain, SAP-specific | BTP with on-premises SAP systems |

#### Reverse Proxy Configuration Example (NGINX)

```nginx
# NGINX Reverse Proxy for GitHub Integration

# Rate limiting to prevent abuse
limit_req_zone $binary_remote_addr zone=github_limit:10m rate=10r/s;

server {
    listen 443 ssl;
    server_name github-integration.example.com;

    # SSL configuration
    ssl_certificate     /etc/ssl/certs/example.com.crt;
    ssl_certificate_key /etc/ssl/private/example.com.key;
    ssl_protocols       TLSv1.2 TLSv1.3;
    ssl_ciphers         HIGH:!aNULL:!MD5;

    # Access log with request details but no sensitive data
    access_log /var/log/nginx/github-access.log combined;
    error_log  /var/log/nginx/github-error.log;

    # GitHub webhook endpoint
    location /webhook {
        limit_req zone=github_limit burst=20 nodelay;
        
        # Verify GitHub webhook signature
        access_by_lua_file /etc/nginx/lua/verify_github_signature.lua;
        
        # Proxy to internal integration service
        proxy_pass http://integration-service:8080/github/webhook;
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto $scheme;
    }

    # SAP API Gateway endpoint
    location /sap-api/ {
        limit_req zone=github_limit burst=5 nodelay;
        
        # Authentication check
        auth_request /auth;
        
        # Proxy to SAP Gateway
        proxy_pass http://sap-gateway:8000/;
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto $scheme;
    }

    # Authentication service
    location = /auth {
        internal;
        proxy_pass http://auth-service:8081/validate;
        proxy_pass_request_body off;
        proxy_set_header Content-Length "";
        proxy_set_header X-Original-URI $request_uri;
    }
}
```

#### SAP Cloud Connector Configuration

For scenarios that include SAP BTP components, the SAP Cloud Connector provides secure connectivity:

```yaml
# SAP Cloud Connector Configuration

cloudConnector:
  version: 2.15.0
  location: DMZ
  
  systems:
    - type: ABAP
      name: ERP_SYSTEM
      host: erp-internal.example.com
      port: 44300
      protocol: HTTPS
      description: "Main ERP System"
      allowedPaths:
        - /sap/opu/odata
        - /sap/bc/soap
        - /sap/bc/webdynpro
      
    - type: HANA
      name: HANA_DB
      host: hana-db.example.com
      port: 30015
      protocol: HDBSQL
      description: "Main HANA Database"
      allowedResources:
        - schema: GITHUB_INT
          
  securitySettings:
    allowedClientIPs: 10.0.0.0/16
    systemCertificateCheck: true
    hostHeaderValidation: true
    
  highAvailability:
    enabled: true
    mode: active-active
    heartbeatTimeout: 30
```

### GitHub Actions Self-Hosted Runner Setup

For cloud-hosted GitHub with on-premises SAP, self-hosted runners provide secure execution:

```yaml
# GitHub Actions Self-Hosted Runner Configuration

# Virtual Machine Specifications
vmConfig:
  cpu: 4
  memory: 16GB
  disk: 200GB
  os: Ubuntu 22.04 LTS
  network: Internal DMZ

# Runner Configuration
runnerConfig:
  labels:
    - sap-integration
    - on-premises
    - secure-zone
  groupName: sap-integration-runners
  autoUpdate: true
  runAsService: true
  serviceAccount: github-runner-svc

# Security Configuration
securityConfig:
  firewallRules:
    - allow: outbound, GitHub.com, 443
    - allow: outbound, SAP-Gateway, 44300
    - allow: outbound, SAP-Router, 3299
    - deny: all
  sandboxing: ephemeral-vms
  networkIsolation: enabled
  secretsManager: azure-keyvault
```

### GitHub Actions Workflow for SAP On-Premises Integration

Example GitHub Actions workflow for SAP on-premises integration:

```yaml
name: SAP On-Premises Integration
on:
  push:
    branches: [ main, develop ]
    paths:
      - 'src/**'
  pull_request:
    branches: [ main ]
    
jobs:
  sap-integration-test:
    runs-on: [self-hosted, sap-integration]
    
    steps:
      - name: Checkout code
        uses: actions/checkout@v3
        
      - name: Setup SAP Connection
        uses: sap-actions/setup-connection@v1
        with:
          connection-url: ${{ secrets.SAP_CONNECTION_URL }}
          client: ${{ secrets.SAP_CLIENT }}
          username: ${{ secrets.SAP_USERNAME }}
          password: ${{ secrets.SAP_PASSWORD }}
          
      - name: Run Integration Tests
        uses: sap-actions/run-integration-tests@v1
        with:
          test-config: tests/integration/config.yaml
          
      - name: Prepare Transport Request
        if: github.event_name == 'push' && github.ref == 'refs/heads/main'
        uses: sap-actions/prepare-transport@v1
        with:
          transport-description: "Automated deployment from GitHub"
          
      - name: Deploy to Development
        if: github.event_name == 'push' && github.ref == 'refs/heads/main'
        uses: sap-actions/deploy-to-sap@v1
        with:
          system: DEV
          transport-request: ${{ steps.prepare-transport.outputs.transport-id }}
```

## On-Premises GitHub Enterprise with SAP BTP

### Architecture Overview

```mermaid
flowchart TD
    A[GitHub Enterprise Server] -->|Internet/Private Link| B[SAP BTP]
    C[Developer Workstations] -->|Internal Network| A
    C -->|Internet/VPN| B
    D[On-Premises CI/CD Runners] -->|Internal Network| A
    D -->|Internet/Private Link| B
    E[Corporate Firewall] -->|Protects| A
    E -->|Controls| F[Internet Gateway]
    F -->|Secures| G[Cloud Connection]
    G -->|Connects to| B
    H[Identity Provider] -->|Authentication| A
    H -->|Authentication| B
```

### Configuration Components

1. **Cloud Connectivity Options**
   - Direct internet connections
   - AWS PrivateLink / Azure Private Link
   - Cloud interconnect services
   - VPN configurations

2. **GitHub Enterprise Server Configuration**
   - Outbound connectivity settings
   - API settings for cloud services
   - Webhook configurations
   - OAuth settings

3. **SAP BTP Connectivity**
   - Destination service configuration
   - Cloud connector settings (if hybrid)
   - Connectivity service
   - Authentication configuration

### Implementation Requirements

#### BTP Connectivity Options

For connecting on-premises GitHub Enterprise to SAP BTP, several connectivity options are available:

| Connectivity Option | Pros | Cons | Best For |
|---------------------|------|------|----------|
| Public Internet | Simple setup, standard protocols, widely supported | Security depends on application-level controls, variable performance | Small to medium deployments with good internet connectivity |
| AWS PrivateLink / Azure Private Link | Direct private connectivity, high security, consistent performance | Cloud provider specific, additional cost, complex setup | Large enterprises with significant cloud investment |
| Express Route / Direct Connect | Dedicated connection, guaranteed performance, high security | High cost, complex setup, long provisioning time | Mission-critical enterprise applications |
| SD-WAN | Intelligent routing, optimized performance, centralized management | Additional infrastructure, expertise required, ongoing management | Distributed environments with multiple connection points |

#### BTP Destination Configuration

Example SAP BTP destination configuration for GitHub Enterprise:

```json
{
  "Name": "github-enterprise",
  "Type": "HTTP",
  "URL": "https://github.example.com",
  "ProxyType": "Internet",
  "Authentication": "OAuth2ClientCredentials",
  "tokenServiceURL": "https://github.example.com/api/v3/app/installations/12345/access_tokens",
  "clientId": "Iv1.123456789abcdef",
  "tokenServiceURLType": "Dedicated",
  "Description": "GitHub Enterprise Server Connection",
  "properties": [
    {
      "key": "GitHub-App-ID",
      "value": "12345"
    },
    {
      "key": "TrustAll",
      "value": "false"
    },
    {
      "key": "HTML5.DynamicDestination",
      "value": "true"
    },
    {
      "key": "WebIDEEnabled",
      "value": "true"
    },
    {
      "key": "WebIDEUsage",
      "value": "odata_gen"
    }
  ]
}
```

#### GitHub Enterprise OAuth App Configuration

Example GitHub Enterprise OAuth app configuration for BTP integration:

```yaml
# GitHub Enterprise OAuth App Configuration

oauthApp:
  name: "SAP BTP Integration"
  homepageUrl: "https://btp-integration.example.com"
  description: "Integration between GitHub Enterprise and SAP BTP"
  callbackUrl: "https://btp-integration.example.com/oauth/callback"
  
  permissions:
    contents: read-write
    metadata: read
    pull_requests: read-write
    workflows: read-write
    hooks: read-write
    
  hookConfig:
    url: "https://btp-integration.example.com/webhook"
    contentType: json
    insecureSsl: false
    secret: "${{ secrets.WEBHOOK_SECRET }}"
    events:
      - push
      - pull_request
      - workflow_job
      - workflow_run
```

### GitHub Actions Workflow for BTP Deployment

Example GitHub Actions workflow for SAP BTP deployment:

```yaml
name: SAP BTP Deployment
on:
  push:
    branches: [ main ]
    paths:
      - 'mta/**'
      
jobs:
  build-and-deploy:
    runs-on: [self-hosted, github-enterprise]
    
    steps:
      - name: Checkout code
        uses: actions/checkout@v3
        
      - name: Setup Cloud MTA Build Tool
        uses: SAP/project-piper-action@master
        with:
          piper-version: latest
          
      - name: Build MTA
        run: mbt build -p=cf
        
      - name: Deploy to BTP
        uses: SAP/cf-cli-action@master
        with:
          cf_api: ${{ secrets.CF_API }}
          cf_username: ${{ secrets.CF_USERNAME }}
          cf_password: ${{ secrets.CF_PASSWORD }}
          cf_org: ${{ secrets.CF_ORG }}
          cf_space: ${{ secrets.CF_SPACE }}
          command: deploy mta_archives/myapp_1.0.0.mtar -f
```

## Cloud-Hosted GitHub with SAP BTP

### Architecture Overview

```mermaid
flowchart TD
    A[GitHub.com] -->|Internet| B[SAP BTP]
    C[Developer Workstations] -->|Internet| A
    C -->|Internet| B
    D[GitHub Actions] -->|Internet| B
    E[BTP CI/CD Service] -->|Internal| B
    F[Identity Provider] -->|Authentication| A
    F -->|Authentication| B
    G[API Gateway] -->|Secures| B
    H[Cloud Security Services] -->|Protects| A
    H -->|Protects| B
```

### Configuration Components

1. **Cloud Security Configuration**
   - API authentication and authorization
   - Service-to-service authentication
   - OAuth configuration
   - IP restrictions and network rules

2. **GitHub Configuration**
   - GitHub Apps and OAuth Apps
   - Actions workflow definitions
   - Repository security settings
   - Organization security policies

3. **BTP Service Configuration**
   - Service instance creation
   - Service binding setup
   - Entitlements management
   - Role collection assignments

### Implementation Requirements

#### GitHub-BTP Integration Security

Cloud-to-cloud integration requires specific security controls:

| Security Control | Implementation | Purpose |
|------------------|----------------|---------|
| OAuth 2.0 | GitHub OAuth App with authorization code flow | User-level authentication for web applications |
| JWT Authentication | Self-signed JWTs with GitHub App private keys | Service-to-service authentication |
| IP Allowlisting | Configure BTP trust configuration with GitHub Actions IP ranges | Restrict API access to GitHub services |
| Webhooks with Secrets | GitHub webhooks with signature verification | Ensure webhook authenticity |
| Secret Management | GitHub encrypted secrets with limited access | Secure credential storage |
| CORS Configuration | BTP application router with strict CORS settings | Prevent cross-site request forgery |

#### BTP Application Security Configuration

Example SAP BTP application security configuration for GitHub integration:

```yaml
# xs-security.json
{
  "xsappname": "github-integration",
  "tenant-mode": "dedicated",
  "scopes": [
    {
      "name": "$XSAPPNAME.Webhook.Receiver",
      "description": "Receive webhooks from GitHub"
    },
    {
      "name": "$XSAPPNAME.Repository.Read",
      "description": "Read repository data"
    },
    {
      "name": "$XSAPPNAME.Repository.Write",
      "description": "Modify repository data"
    },
    {
      "name": "$XSAPPNAME.Deployment.Execute",
      "description": "Execute deployments"
    }
  ],
  "attributes": [
    {
      "name": "RepositoryName",
      "description": "GitHub repository name",
      "valueType": "string"
    },
    {
      "name": "OrganizationName",
      "description": "GitHub organization name",
      "valueType": "string"
    }
  ],
  "role-templates": [
    {
      "name": "GitHubWebhookProcessor",
      "description": "Process webhooks from GitHub",
      "scope-references": [
        "$XSAPPNAME.Webhook.Receiver"
      ],
      "attribute-references": []
    },
    {
      "name": "GitHubIntegrationAdmin",
      "description": "Administer GitHub integration",
      "scope-references": [
        "$XSAPPNAME.Repository.Read",
        "$XSAPPNAME.Repository.Write",
        "$XSAPPNAME.Deployment.Execute"
      ],
      "attribute-references": []
    },
    {
      "name": "RepositoryOwner",
      "description": "Owner of specific repositories",
      "scope-references": [
        "$XSAPPNAME.Repository.Read",
        "$XSAPPNAME.Repository.Write"
      ],
      "attribute-references": [
        {
          "name": "RepositoryName",
          "required": true
        }
      ]
    }
  ],
  "oauth2-configuration": {
    "token-validity": 3600,
    "refresh-token-validity": 2592000,
    "redirect-uris": [
      "https://*.cfapps.eu10.hana.ondemand.com/**",
      "https://*.cfapps.us10.hana.ondemand.com/**"
    ],
    "autoapprove": true
  }
}
```

#### GitHub Actions Workflow for BTP Multi-Region Deployment

Example GitHub Actions workflow for multi-region BTP deployment:

```yaml
name: Multi-Region BTP Deployment
on:
  push:
    branches: [ main ]
    tags: [ 'v*' ]
    
jobs:
  build-mta:
    runs-on: ubuntu-latest
    outputs:
      mtar-name: ${{ steps.build-mta.outputs.mtar-name }}
    
    steps:
      - name: Checkout code
        uses: actions/checkout@v3
        
      - name: Setup Cloud MTA Build Tool
        uses: SAP/project-piper-action@master
        with:
          piper-version: latest
          
      - name: Build MTA
        id: build-mta
        run: |
          mbt build -p=cf
          echo "mtar-name=$(ls mta_archives/*.mtar)" >> $GITHUB_OUTPUT
          
      - name: Upload MTA artifact
        uses: actions/upload-artifact@v3
        with:
          name: mta-archive
          path: mta_archives/*.mtar
          
  deploy-eu10:
    needs: build-mta
    runs-on: ubuntu-latest
    environment: production-eu10
    
    steps:
      - name: Download MTA artifact
        uses: actions/download-artifact@v3
        with:
          name: mta-archive
          path: mta_archives
          
      - name: Deploy to EU10
        uses: SAP/cf-cli-action@master
        with:
          cf_api: https://api.cf.eu10.hana.ondemand.com
          cf_username: ${{ secrets.CF_USERNAME }}
          cf_password: ${{ secrets.CF_PASSWORD }}
          cf_org: ${{ secrets.CF_ORG }}
          cf_space: ${{ secrets.CF_SPACE }}
          command: deploy ${{ needs.build-mta.outputs.mtar-name }} -f
          
  deploy-us10:
    needs: [build-mta, deploy-eu10]
    runs-on: ubuntu-latest
    environment: production-us10
    
    steps:
      - name: Download MTA artifact
        uses: actions/download-artifact@v3
        with:
          name: mta-archive
          path: mta_archives
          
      - name: Deploy to US10
        uses: SAP/cf-cli-action@master
        with:
          cf_api: https://api.cf.us10.hana.ondemand.com
          cf_username: ${{ secrets.CF_USERNAME }}
          cf_password: ${{ secrets.CF_PASSWORD }}
          cf_org: ${{ secrets.CF_ORG }}
          cf_space: ${{ secrets.CF_SPACE }}
          command: deploy ${{ needs.build-mta.outputs.mtar-name }} -f
```

## Hybrid Scenarios with Connectivity Options

### Architecture Overview

```mermaid
flowchart TD
    A[GitHub] -->|Internet| B[SAP BTP]
    A -->|Secure Connection| C[Integration Gateway]
    C -->|Internal Network| D[On-Premises SAP]
    B -->|Cloud Connector| D
    E[Developer Workstations] -->|VPN/Internal| D
    E -->|Internet| A
    E -->|Internet| B
    F[CI/CD Runners] -->|Mixed Deployment| A
    F -->|Mixed Deployment| B
    F -->|VPN/Internal| D
    G[Identity Provider] -->|Authentication| A
    G -->|Authentication| B
    G -->|Authentication| D
```

### Configuration Components

1. **Hybrid Network Architecture**
   - End-to-end connectivity mapping
   - Traffic flow optimization
   - Security boundary definitions
   - Network redundancy planning

2. **Multi-Environment CI/CD**
   - Runner deployment strategy
   - Build artifact handling
   - Environment-specific configuration
   - Pipeline orchestration

3. **Unified Monitoring and Management**
   - Cross-environment visibility
   - Integrated logging
   - Alert correlation
   - Performance monitoring

### Implementation Requirements

#### Hybrid Connectivity Architecture

For hybrid scenarios, a comprehensive connectivity architecture is required:

```yaml
# Hybrid Connectivity Architecture

environments:
  onPremises:
    network: "10.0.0.0/16"
    dmz: "10.1.0.0/24"
    dataCenter: "Primary DC East"
    
  cloudProvider:
    name: "Azure"
    region: "West Europe"
    vnet: "172.16.0.0/16"
    
connections:
  - name: "ExpressRoute"
    type: dedicated
    bandwidth: "1 Gbps"
    encryption: true
    endpoints:
      - "On-premises Edge Router"
      - "Azure ExpressRoute Circuit"
    backupLink: "Site-to-Site VPN"
    
  - name: "SAP Cloud Connector"
    type: application-level
    located: "DMZ"
    allowedSystems:
      - system: "ERP_PRD"
        protocols: ["RFC", "HTTP"]
        ports: [33xx, 443]
      - system: "S4_DEV"
        protocols: ["RFC", "HTTP", "HTTPS"]
        ports: [33xx, 443, 8443]
    
  - name: "GitHub Enterprise Connect"
    type: service-specific
    located: "DMZ"
    purpose: "GitHub Actions Runners"
    allowedTargets:
      - "GitHub Enterprise Cloud"
      - "Integration Services"
```

#### Hybrid CI/CD Runner Strategy

In hybrid environments, a comprehensive runner deployment strategy is essential:

| Environment | Runner Type | Characteristics | Use Cases |
|-------------|-------------|-----------------|-----------|
| GitHub-hosted | Cloud | Managed by GitHub, ephemeral, auto-scaling | Standard builds, tests, general automation |
| Self-hosted (Cloud) | Cloud | Customer-managed cloud VMs, ephemeral or persistent | Cloud deployments, BTP integration, performance tests |
| Self-hosted (On-prem) | On-premises | Customer-managed on-prem servers, persistent | SAP on-prem integration, secured data access, legacy systems |
| Hybrid Pool | Mixed | Combination of runner types with intelligent routing | Complex pipelines spanning multiple environments |

#### GitHub Actions Runner Groups Configuration

GitHub Actions Runner Groups for hybrid environments:

```yaml
# GitHub Actions Runner Groups

runnerGroups:
  - name: "cloud-general"
    runners:
      - type: "github-hosted"
        os: ["ubuntu-latest", "windows-latest"]
    accessPolicy:
      repositories: ["all"]
      
  - name: "cloud-btp-deployment"
    runners:
      - type: "self-hosted"
        location: "azure-westeurope"
        labels: ["btp-deploy", "multi-tenant"]
    accessPolicy:
      repositories: ["org/btp-*", "org/cloud-*"]
      
  - name: "onprem-sap-integration"
    runners:
      - type: "self-hosted"
        location: "datacenter-east"
        labels: ["sap-integration", "secure"]
    accessPolicy:
      repositories: ["org/sap-*", "org/erp-*"]
      protectionRules:
        - requireApproval: true
        - restrictSecrets: true
        
  - name: "hybrid-end-to-end"
    runners:
      - type: "self-hosted"
        location: "azure-westeurope"
        labels: ["hybrid-orchestrator", "e2e-test"]
    accessPolicy:
      repositories: ["org/e2e-*", "org/integration-*"]
```

#### GitHub Actions Workflow for Hybrid SAP/BTP Deployment

Example GitHub Actions workflow for hybrid SAP/BTP deployment:

```yaml
name: Hybrid SAP Deployment

on:
  push:
    branches: [ main ]
    paths:
      - 'src/**'
      - 'mta/**'
      
jobs:
  build:
    runs-on: ubuntu-latest
    outputs:
      sap-package: ${{ steps.package-info.outputs.sap-package }}
      mtar-path: ${{ steps.build-mta.outputs.mtar-path }}
    
    steps:
      - name: Checkout code
        uses: actions/checkout@v3
        
      - name: Get SAP package info
        id: package-info
        run: |
          echo "sap-package=$(cat abap-deployment.yaml | grep package | cut -d':' -f2 | xargs)" >> $GITHUB_OUTPUT
          
      - name: Build MTA
        id: build-mta
        run: |
          mbt build -p=cf
          echo "mtar-path=$(ls mta_archives/*.mtar)" >> $GITHUB_OUTPUT
          
      - name: Upload MTA artifact
        uses: actions/upload-artifact@v3
        with:
          name: mta-archive
          path: ${{ steps.build-mta.outputs.mtar-path }}
          
      - name: Create transport package
        uses: actions/upload-artifact@v3
        with:
          name: abap-transport
          path: src/
          
  deploy-onprem:
    needs: build
    runs-on: [self-hosted, sap-integration]
    
    steps:
      - name: Download transport package
        uses: actions/download-artifact@v3
        with:
          name: abap-transport
          path: ./src
          
      - name: Create transport request
        uses: sap-actions/create-transport@v1
        id: transport
        with:
          sap-package: ${{ needs.build.outputs.sap-package }}
          description: "Hybrid deployment from GitHub"
          
      - name: Deploy to SAP
        uses: sap-actions/deploy-to-sap@v1
        with:
          transport-request: ${{ steps.transport.outputs.transport-id }}
          system: DEV
          client: '100'
          
  deploy-btp:
    needs: build
    runs-on: [self-hosted, btp-deploy]
    
    steps:
      - name: Download MTA artifact
        uses: actions/download-artifact@v3
        with:
          name: mta-archive
          path: ./
          
      - name: Deploy to BTP
        uses: SAP/cf-cli-action@master
        with:
          cf_api: ${{ secrets.CF_API }}
          cf_username: ${{ secrets.CF_USERNAME }}
          cf_password: ${{ secrets.CF_PASSWORD }}
          cf_org: ${{ secrets.CF_ORG }}
          cf_space: ${{ secrets.CF_SPACE }}
          command: deploy ${{ needs.build.outputs.mtar-path }} -f
          
  integration-test:
    needs: [deploy-onprem, deploy-btp]
    runs-on: [self-hosted, hybrid-orchestrator]
    
    steps:
      - name: Checkout test code
        uses: actions/checkout@v3
        with:
          sparse-checkout: |
            tests/
            
      - name: Run end-to-end tests
        uses: sap-actions/run-e2e-tests@v1
        with:
          test-config: tests/e2e/hybrid-config.yaml
          onprem-endpoint: ${{ secrets.ONPREM_ENDPOINT }}
          btp-endpoint: ${{ secrets.BTP_ENDPOINT }}
```

## VPN and Secure Connectivity Patterns

### VPN Architecture Patterns

```mermaid
flowchart TD
    A[Corporate Network] -->|Site-to-Site VPN| B[Cloud Provider Network]
    C[GitHub Enterprise] -->|Deployed in| A
    D[SAP Systems] -->|Deployed in| A
    E[BTP Services] -->|Deployed in| B
    F[Development Workstations] -->|Client VPN| A
    F -->|Client VPN| B
    G[CI/CD Runners] -->|Deployed in| A
    G -->|Deployed in| B
    H[VPN Gateway] -->|Secures| A
    I[Cloud VPN Gateway] -->|Secures| B
```

### VPN Implementation Options

Various VPN implementations can be used to connect different environments:

| VPN Type | Implementation | Best For | Considerations |
|----------|----------------|----------|----------------|
| Site-to-Site | IPsec VPN with AES-256 encryption | Connecting corporate network to cloud provider | Requires compatible VPN gateways, fixed bandwidth |
| Client-to-Site | OpenVPN / WireGuard | Developer access to internal resources | Client configuration management, authentication integration |
| Cloud Provider VPN | AWS VPN / Azure VPN / GCP VPN | Native integration with cloud services | Cloud-specific features, simplified management |
| SD-WAN | Cisco SD-WAN / VMware SD-WAN | Complex multi-site connectivity | Higher cost, advanced features, optimization |

### Example Site-to-Site VPN Configuration

Sample configuration for IPsec Site-to-Site VPN:

```
# IPsec VPN Configuration

## Phase 1 (IKE) Configuration
IKE_Version: IKEv2
Encryption: AES-256-GCM
Authentication: SHA-384
Diffie-Hellman_Group: Group 20 (NIST ECP 384)
Lifetime: 28800 seconds

## Phase 2 (ESP) Configuration
Protocol: ESP
Encryption: AES-256-GCM
Authentication: SHA-384
PFS_Group: Group 20 (NIST ECP 384)
Lifetime: 3600 seconds

## Tunnel Configuration
Local_Network: 10.0.0.0/16 (Corporate Network)
Remote_Network: 172.16.0.0/16 (Cloud Provider VPC)
Local_Gateway: 203.0.113.1 (Corporate VPN Gateway)
Remote_Gateway: 198.51.100.1 (Cloud VPN Gateway)
Mode: Main mode
Startup_Action: Start

## Dead Peer Detection
DPD_Enabled: Yes
DPD_Interval: 10 seconds
DPD_Timeout: 30 seconds
DPD_Action: Restart
```

### Private Service Connect / AWS PrivateLink

For highly secure connections to cloud services, cloud provider private service connections are recommended:

```yaml
# AWS PrivateLink Configuration

vpcEndpoints:
  - service: com.amazonaws.eu-central-1.s3
    vpcId: vpc-1a2b3c4d
    subnetIds:
      - subnet-abcdef12
      - subnet-34567890
    securityGroupIds:
      - sg-abcdef12
    privateDnsEnabled: true
    
  - service: com.amazonaws.eu-central-1.sqs
    vpcId: vpc-1a2b3c4d
    subnetIds:
      - subnet-abcdef12
      - subnet-34567890
    securityGroupIds:
      - sg-abcdef12
    privateDnsEnabled: true
    
peeringSAP:
  vpcPeering:
    vpcId: vpc-1a2b3c4d
    peerVpcId: vpc-5e6f7g8h  # SAP BTP VPC
    peerRoleArn: arn:aws:iam::123456789012:role/btp-peering-role
    accepter:
      region: eu-central-1
      allowDnsResolution: true
    routes:
      - destinationCidrBlock: 10.0.0.0/16
        vpcEndpointId: vpce-11223344
```

### SAP HANA Cloud Secure Connection Patterns

For connecting SAP HANA Cloud securely in hybrid environments:

```yaml
# SAP HANA Cloud Connectivity

hanaCloudConnections:
  - name: "On-Premises SAP S/4HANA to HANA Cloud"
    sourceSystem: "S4HANA_PRD"
    type: "SSC_Forwarding"  # SAP HANA Smart Data Access Connection
    port: 443
    encryption: "TLS 1.3"
    authentication: "X.509 Certificate"
    certificateDetails:
      issuer: "Corporate PKI"
      validityPeriod: "1 year"
      keyStrength: "4096 bits"
      
  - name: "HANA Cloud to GitHub Enterprise"
    sourceSystem: "HANA_CLOUD"
    targetSystem: "GitHub Enterprise"
    type: "HTTPS REST API"
    port: 443
    encryption: "TLS 1.3"
    authentication: "OAuth 2.0 + JWT"
    apiRateLimits:
      requests: 5000
      period: "hour"
```

## Implementation Recommendations

Based on real-world implementations, the following recommendations apply to environment-specific configurations for SAP-GitHub integration:

1. **Security-First Approach**
   - Implement defense-in-depth security
   - Secure connectivity between all components
   - Use encryption for data in transit and at rest
   - Regular security assessments and updates

2. **Infrastructure as Code**
   - Define all environment configurations as code
   - Version control environment definitions
   - Automated environment provisioning
   - Consistent deployments across environments

3. **Performance Optimization**
   - Minimize latency between components
   - Optimize traffic patterns
   - Implement caching strategies
   - Monitor and tune performance

4. **Operational Excellence**
   - Comprehensive monitoring across environments
   - Centralized logging and alerting
   - Automated recovery procedures
   - Regular disaster recovery testing

## Related Documentation

- [Governance Model](./governance-model.md)
- [Disaster Recovery Architecture](./disaster-recovery-architecture.md)
- [Integration Architecture](./integration-architecture.md)
- [System Architecture](./system-architecture.md)

## References

1. SAP. (2025). "SAP BTP Connectivity Guide." SAP Help Portal.
2. GitHub. (2025). "Enterprise Server Security Hardening."
3. AWS. (2025). "AWS to SAP Connectivity Best Practices."
4. Microsoft. (2025). "Azure Private Link for SAP Services."
5. NIST. (2025). "Security Guidelines for Enterprise Software Configuration."