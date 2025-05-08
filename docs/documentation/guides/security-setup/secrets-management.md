---
title: Comprehensive Secrets Management
description: Secure secrets management architecture for SAP-GitHub integration including vault solutions, implementation patterns, and rotation strategies
author: SAP-GitHub Integration Team
date: 2024-05-15
version: 1.0.0
---

# Comprehensive Secrets Management

## Table of Contents

- [Overview](#overview)
- [Secret Management Requirements](#secret-management-requirements)
- [Solution Comparison](#solution-comparison)
  - [HashiCorp Vault](#hashicorp-vault)
  - [AWS Secrets Manager](#aws-secrets-manager)
  - [SAP Credential Store](#sap-credential-store)
  - [Azure Key Vault](#azure-key-vault)
  - [Kubernetes Secrets](#kubernetes-secrets)
- [Implementation Patterns](#implementation-patterns)
  - [Centralized Vault Architecture](#centralized-vault-architecture)
  - [Distributed Secret Management](#distributed-secret-management)
  - [Hybrid Approach](#hybrid-approach)
- [Secret Types and Handling](#secret-types-and-handling)
  - [Authentication Credentials](#authentication-credentials)
  - [API Keys](#api-keys)
  - [Certificates and Keys](#certificates-and-keys)
  - [Connection Strings](#connection-strings)
- [Rotation Strategies](#rotation-strategies)
  - [Time-Based Rotation](#time-based-rotation)
  - [Event-Based Rotation](#event-based-rotation)
  - [Automated Rotation](#automated-rotation)
- [Integration with CI/CD](#integration-with-cicd)
  - [GitHub Actions Secrets](#github-actions-secrets)
  - [Dynamic Credential Injection](#dynamic-credential-injection)
- [Access Control and Audit](#access-control-and-audit)
  - [Least Privilege Model](#least-privilege-model)
  - [Secret Access Auditing](#secret-access-auditing)
- [Monitoring and Alerting](#monitoring-and-alerting)
- [Disaster Recovery](#disaster-recovery)
- [Implementation Examples](#implementation-examples)
- [Best Practices](#best-practices)
- [References](#references)

## Overview

Effective secrets management is critical for secure SAP-GitHub integration. This guide outlines a comprehensive architecture for managing credentials, API keys, certificates, and other sensitive information across environments.

A robust secrets management solution provides:
- Secure storage with encryption at rest and in transit
- Granular access controls and authentication
- Automated lifecycle management including rotation
- Comprehensive audit trails
- Integration with development and operational workflows

## Secret Management Requirements

For SAP-GitHub integration, a secrets management solution must satisfy the following requirements:

1. **Security**:
   - AES-256 (or stronger) encryption
   - Hardware Security Module (HSM) support
   - Multi-factor authentication for high-privilege operations
   - Network isolation and secure transport

2. **Functionality**:
   - Support for multiple secret types (text, binary, certificates)
   - Version history and rollback capabilities
   - Secret sharing across applications/services
   - Dynamic secrets generation
   - API and CLI access

3. **Integration**:
   - Support for SAP landscape components
   - GitHub Actions/Workflows integration
   - CI/CD pipeline integration
   - Container/Kubernetes compatibility
   - Cloud provider compatibility

4. **Governance**:
   - Comprehensive access logging
   - Approval workflows for sensitive operations
   - Compliance reporting
   - Secret usage tracking

## Solution Comparison

### HashiCorp Vault

[HashiCorp Vault](https://www.vaultproject.io/) is an open-source solution for securely accessing secrets.

**Strengths**:
- Mature, widely adopted solution
- Highly extensible with plugins
- Dynamic secret generation
- Advanced access control with policies
- Multiple authentication methods
- Open-source core with enterprise features available

**Considerations**:
- Requires infrastructure setup and maintenance
- Higher complexity for initial setup
- Enterprise features require licensing

**Integration Example**:
```java
// Java example using Vault Java Driver
VaultConfig config = new VaultConfig()
    .address("https://vault.example.com:8200")
    .token("s.token")
    .build();

Vault vault = new Vault(config);
String sapApiKey = vault.logical()
    .read("secret/sap/api-keys/production")
    .getData()
    .get("api-key");
```

### AWS Secrets Manager

[AWS Secrets Manager](https://aws.amazon.com/secrets-manager/) is a fully managed service for protecting secrets.

**Strengths**:
- Fully managed with no infrastructure to maintain
- Native AWS integration (IAM, CloudTrail, etc.)
- Automated rotation of AWS RDS credentials
- Region-specific storage for compliance
- Pay-per-use pricing model

**Considerations**:
- AWS ecosystem lock-in
- Higher cost for large numbers of secrets
- Limited features compared to specialized vault solutions

**Integration Example**:
```java
// Java example using AWS SDK
AWSSecretsManager secretsManager = AWSSecretsManagerClientBuilder.standard()
    .withRegion(Regions.EU_CENTRAL_1)
    .build();

GetSecretValueRequest request = new GetSecretValueRequest()
    .withSecretId("sap-github-integration/github-app-key");

GetSecretValueResult result = secretsManager.getSecretValue(request);
String privateKey = result.getSecretString();
```

### SAP Credential Store

[SAP Credential Store](https://help.sap.com/docs/CREDENTIAL_STORE) is part of the SAP Cloud Platform for securely storing credentials.

**Strengths**:
- Native integration with SAP landscape
- SAP BTP integrated security model
- Simplified setup for SAP customers
- Compliance with SAP security standards
- Managed service with SLA

**Considerations**:
- Limited to SAP ecosystem
- Fewer features than specialized vault solutions
- Limited extensibility

**Integration Example**:
```java
// Java example using SAP Credential Store SDK
CredentialStore credStore = CredentialStoreFactory.getInstance().getCredentialStore();
Credentials credentials = credStore.getCredential("GITHUB_APP_CREDENTIALS");

String appId = credentials.getUsername();
String privateKey = credentials.getPassword();
```

### Azure Key Vault

[Azure Key Vault](https://azure.microsoft.com/en-us/services/key-vault/) is Microsoft's cloud service for securing keys and secrets.

**Strengths**:
- Fully managed service
- Strong HSM backing options
- Native Azure AD integration
- Comprehensive key management features
- Good compliance certifications

**Considerations**:
- Azure ecosystem lock-in
- Complex RBAC model
- Cross-cloud access limitations

**Integration Example**:
```java
// Java example using Azure SDK
SecretClient secretClient = new SecretClientBuilder()
    .vaultUrl("https://sap-integration-vault.vault.azure.net/")
    .credential(new DefaultAzureCredentialBuilder().build())
    .buildClient();

KeyVaultSecret githubSecret = secretClient.getSecret("github-app-private-key");
String privateKey = githubSecret.getValue();
```

### Kubernetes Secrets

[Kubernetes Secrets](https://kubernetes.io/docs/concepts/configuration/secret/) provide native secret management for Kubernetes-based deployments.

**Strengths**:
- Native integration with Kubernetes
- Simple setup for existing K8s users
- Works with any Kubernetes distribution
- Can be enhanced with tools like Sealed Secrets or Vault integration

**Considerations**:
- Basic functionality only
- Limited rotation/lifecycle features
- Cluster-scoped
- Requires additional encryption configuration

**Integration Example**:
```yaml
# Kubernetes manifest example
apiVersion: v1
kind: Secret
metadata:
  name: github-app-credentials
  namespace: sap-github-integration
type: Opaque
data:
  app-id: BASE64_ENCODED_APP_ID
  private-key: BASE64_ENCODED_PRIVATE_KEY
```

## Implementation Patterns

### Centralized Vault Architecture

In a centralized architecture, a single vault service manages all secrets across environments.

**Components**:
1. **Primary Vault**: Central high-availability vault cluster
2. **Authentication Services**: Identity providers and authentication gateways
3. **Vault Agents**: Local cache and authentication proxies
4. **Admin UI/API**: Management interfaces
5. **Audit Log Storage**: Consolidated audit trail

**Advantages**:
- Consistent policy enforcement
- Simplified management and oversight
- Centralized audit trail
- Reduced infrastructure footprint

**Disadvantages**:
- Single point of failure risk
- Potential performance bottlenecks
- Cross-environment access complexity

[DIAGRAM: Centralized vault architecture showing components and connections]

### Distributed Secret Management

In a distributed pattern, separate vault instances serve different environments or workloads.

**Components**:
1. **Environment-Specific Vaults**: Dedicated vault for each environment
2. **Local Authentication**: Environment-specific auth services
3. **Federated Identity**: Cross-vault identity management
4. **Replication**: Optional synchronization between vaults
5. **Central Reporting**: Aggregated compliance and audit

**Advantages**:
- Environment isolation
- Performance optimization
- Flexible scaling
- Reduced blast radius for incidents

**Disadvantages**:
- Management overhead
- Policy consistency challenges
- Complex audit consolidation

[DIAGRAM: Distributed vault architecture showing environment-specific instances]

### Hybrid Approach

The recommended approach for SAP-GitHub integration combines elements of centralized and distributed models.

**Implementation**:
1. **Core Vault**: Central vault for organization-wide secrets
2. **Environment Vaults**: Environment-specific vaults (Dev, QA, Prod)
3. **Service-Specific Stores**: Native secret stores for specific services
4. **Vault Federation**: Cross-vault authentication and management
5. **Central Governance**: Unified policies and auditing

This approach provides:
- Appropriate isolation between environments
- Service-specific optimization
- Consistent security posture
- Optimized operational overhead

[DIAGRAM: Hybrid architecture showing core and environment-specific vaults with service integrations]

## Secret Types and Handling

Different types of secrets require specific handling approaches:

### Authentication Credentials

1. **User Credentials**:
   - Store hashed passwords only, never plaintext
   - Implement individual credentials, not shared accounts
   - Enforce strong password policies

2. **Service Accounts**:
   - Implement programmatic rotation
   - Restrict to required permissions only
   - Monitor for unusual access patterns

3. **Application Identities**:
   - Store credentials with application-level encryption
   - Implement secure retrieval with memory protections
   - Avoid embedding in configuration files

### API Keys

1. **Storage Requirements**:
   - Encrypt at rest with strong encryption
   - Store with associated metadata (owner, purpose, expiration)
   - Implement version control for rotation

2. **Access Controls**:
   - Restrict access to specific applications/services
   - Log all access attempts
   - Implement rate limiting on retrieval

3. **Lifecycle Management**:
   - Set expiration dates on all API keys
   - Implement automated rotation procedures
   - Track key usage for deprecated key detection

### Certificates and Keys

1. **Key Security**:
   - Store private keys in dedicated HSM when possible
   - Implement strict access controls
   - Never expose private keys in logs or outputs

2. **Certificate Management**:
   - Track expiration dates and implement alerting
   - Store complete certificate chains
   - Implement automated renewal processes

3. **Implementation Example**:
   ```java
   // Example of secure private key handling
   public class SecureKeyManager {
       private final VaultClient vaultClient;
       
       public PrivateKey getPrivateKey(String keyId) {
           // Retrieve from vault
           String pemKey = vaultClient.getSecret("certificates/" + keyId).getData().get("private_key");
           
           // Convert to PrivateKey object
           try (PEMParser pemParser = new PEMParser(new StringReader(pemKey))) {
               PEMKeyPair keyPair = (PEMKeyPair) pemParser.readObject();
               KeyFactory keyFactory = KeyFactory.getInstance("RSA");
               PKCS8EncodedKeySpec privateKeySpec = new PKCS8EncodedKeySpec(
                   keyPair.getPrivateKeyInfo().getEncoded());
               
               // Key in memory only for usage duration
               return keyFactory.generatePrivate(privateKeySpec);
           } catch (Exception e) {
               throw new SecurityException("Failed to process private key", e);
           }
       }
   }
   ```

### Connection Strings

1. **Database Connections**:
   - Store connection components separately (host, user, password)
   - Implement environment-specific connection details
   - Use temporary, least-privilege credentials when possible

2. **Service Endpoints**:
   - Separate endpoint URLs from authentication
   - Implement secure endpoint validation
   - Consider endpoint routing through secure proxies

## Rotation Strategies

### Time-Based Rotation

Implement regular credential rotation based on time intervals:

1. **Scheduling**:
   - Define rotation periods based on credential sensitivity
   - Stagger rotations to reduce operational impact
   - Implement pre-expiration warnings

2. **Implementation**:
   ```java
   // Example scheduled rotation service
   @Scheduled(cron = "0 0 1 * * ?") // Daily at 1 AM
   public void rotateCredentials() {
       List<CredentialRecord> expiringCredentials = credentialRepository
           .findExpiringCredentials(LocalDate.now().plusDays(7));
           
       for (CredentialRecord credential : expiringCredentials) {
           try {
               credentialRotator.rotateCredential(credential.getId());
               auditLogger.recordRotation(credential.getId(), "Scheduled rotation");
           } catch (RotationException e) {
               alertService.sendAlert("Failed to rotate credential: " + credential.getId());
               log.error("Rotation failed", e);
           }
       }
   }
   ```

### Event-Based Rotation

Trigger credential rotation based on specific events:

1. **Triggering Events**:
   - Personnel changes (employee departures)
   - Security incidents
   - Compliance findings
   - System compromises

2. **Implementation Process**:
   - Define event detection mechanisms
   - Implement rotation workflows
   - Validate post-rotation functionality
   - Document rotation reason

### Automated Rotation

Implement fully automated rotation for programmatic credentials:

1. **GitHub App Private Keys**:
   ```java
   public void rotateGitHubAppKey(Long appId) {
       // 1. Generate new key in GitHub
       GitHubAppKey newKey = githubAdminClient.generateNewAppKey(appId);
       
       // 2. Store in vault with versioning
       vaultClient.putSecret("github/app-keys/" + appId, 
           Map.of("private_key", newKey.getPrivateKey(), 
                  "key_id", newKey.getKeyId()));
       
       // 3. Update references and test
       boolean testSuccessful = testNewKey(appId, newKey.getKeyId());
       
       // 4. Delete old key if test successful
       if (testSuccessful) {
           githubAdminClient.deleteAppKey(appId, currentKeyId);
           currentKeyService.updateCurrentKeyId(appId, newKey.getKeyId());
       } else {
           // Rollback if test failed
           vaultClient.revertSecretToVersion("github/app-keys/" + appId, previousVersion);
           alertService.sendAlert("GitHub App key rotation failed for app " + appId);
       }
   }
   ```

2. **SAP API Credentials**:
   - Implement SAP-specific rotation procedures
   - Coordinate with SAP user management
   - Test connectivity after rotation
   - Maintain fallback credentials during transition

## Integration with CI/CD

### GitHub Actions Secrets

1. **Repository Secrets**:
   - Store environment-specific secrets at repository level
   - Limit access to protected branches
   - Implement secret masking in logs

2. **Organization Secrets**:
   - Use for shared credentials across repositories
   - Implement repository filtering for targeted access
   - Regular audit of organization-level secrets

3. **Environment Secrets**:
   - Configure per-environment secrets
   - Implement approval workflows for production
   - Separate development and production credentials

4. **Workflow Example**:
   ```yaml
   # GitHub Actions workflow with secret usage
   name: SAP Deployment
   
   on:
     push:
       branches: [main]
   
   jobs:
     deploy:
       runs-on: ubuntu-latest
       environment: production
       
       steps:
         - uses: actions/checkout@v3
         
         - name: Retrieve secrets from vault
           id: secrets
           uses: hashicorp/vault-action@v2
           with:
             url: ${{ secrets.VAULT_URL }}
             token: ${{ secrets.VAULT_TOKEN }}
             secrets: |
               secret/data/sap/credentials username | SAP_USERNAME
               secret/data/sap/credentials password | SAP_PASSWORD
         
         - name: Deploy to SAP
           uses: SAP/project-piper-action@v1
           with:
             command: cloudFoundryDeploy
             flags: >
               --cf-username=${{ steps.secrets.outputs.SAP_USERNAME }}
               --cf-password=${{ steps.secrets.outputs.SAP_PASSWORD }}
   ```

### Dynamic Credential Injection

For enhanced security, implement dynamic credential retrieval:

1. **Vault Integration**:
   - Deploy vault agent sidecar in container environments
   - Use vault agent for just-in-time credential retrieval
   - Implement token authentication for vault access

2. **Runtime Secrets**:
   - Retrieve secrets at runtime, not build time
   - Implement memory protections for secret usage
   - Clear secrets from memory after use

3. **Implementation Example**:
   ```java
   // Dynamic credential retrieval during application startup
   @Configuration
   public class DynamicCredentialConfig {
       @Bean
       public SapClient sapClient(VaultClient vaultClient) {
           // Get credentials only when needed, not stored in configuration
           Credentials credentials = vaultClient.getCredentials("sap/api");
           
           return SapClientBuilder.create()
               .withEndpoint(applicationProperties.getSapApiEndpoint())
               .withAuthentication(credentials)
               .withSecureTransport()
               .build();
       }
   }
   ```

## Access Control and Audit

### Least Privilege Model

Implement least privilege access to secrets:

1. **Role-Based Access Control**:
   - Define specific roles for secret access
   - Implement fine-grained policies
   - Enforce separation of duties

2. **Example HashiCorp Vault Policy**:
   ```hcl
   # SAP Integration Developer policy
   path "secret/data/sap/dev/*" {
     capabilities = ["read", "list"]
   }
   
   path "secret/data/github/dev/*" {
     capabilities = ["read", "list"]
   }
   
   # Deny production secrets
   path "secret/data/sap/prod/*" {
     capabilities = ["deny"]
   }
   
   path "secret/data/github/prod/*" {
     capabilities = ["deny"]
   }
   ```

### Secret Access Auditing

Implement comprehensive auditing:

1. **Audit Requirements**:
   - Record all access attempts (successful and failed)
   - Log metadata (who, what, when, from where)
   - Implement secure, immutable audit logs
   - Set appropriate retention periods

2. **Integration with SIEM**:
   - Forward audit logs to central SIEM
   - Implement correlation rules
   - Configure alerting for suspicious patterns
   - Generate compliance reports

## Monitoring and Alerting

Implement proactive monitoring:

1. **Metrics to Monitor**:
   - Failed access attempts
   - Unusual access patterns
   - Certificate/credential expiration
   - Secret engine health
   - Authentication backend status

2. **Alerting Strategy**:
   - Immediate alerts for security events
   - Scheduled reports for compliance
   - Graduated alert levels based on severity
   - On-call rotation for critical alerts

3. **Dashboard Example**:
   [SCREENSHOT: Secret management dashboard showing expiration timelines, access metrics, and health indicators]

## Disaster Recovery

Prepare for secret management system failures:

1. **Backup Strategy**:
   - Regular encrypted backups of vault data
   - Secure offline storage of recovery keys
   - Test restoration procedures regularly

2. **Recovery Procedures**:
   - Document step-by-step recovery process
   - Implement break-glass procedures
   - Train operations teams on recovery
   - Test recovery annually

3. **Business Continuity**:
   - Implement high-availability vault configurations
   - Consider geographical distribution
   - Define RPO/RTO objectives for secret management

## Implementation Examples

### HashiCorp Vault with GitHub Actions

```yaml
# .github/workflows/sap-deployment.yml
name: SAP Deployment

on:
  push:
    branches: [main]

jobs:
  deploy:
    runs-on: ubuntu-latest
    
    steps:
      - uses: actions/checkout@v3
      
      - name: Retrieve secrets from HashiCorp Vault
        id: secrets
        uses: hashicorp/vault-action@v2
        with:
          url: ${{ secrets.VAULT_ADDR }}
          method: approle
          roleId: ${{ secrets.VAULT_ROLE_ID }}
          secretId: ${{ secrets.VAULT_SECRET_ID }}
          secrets: |
            secret/data/sap/btp/credentials username | SAP_USERNAME
            secret/data/sap/btp/credentials password | SAP_PASSWORD
            secret/data/github/apps/integration-app private_key | GITHUB_PRIVATE_KEY
            secret/data/github/apps/integration-app app_id | GITHUB_APP_ID
      
      - name: Setup Piper
        uses: SAP/project-piper-action@v1
        with:
          command: setupCommonPipelineEnvironment
      
      - name: Deploy to SAP BTP
        uses: SAP/project-piper-action@v1
        with:
          command: cloudFoundryDeploy
          flags: >
            --cf-api=${{ secrets.CF_API }}
            --cf-org=${{ secrets.CF_ORG }}
            --cf-space=${{ secrets.CF_SPACE }}
            --cf-username=${{ steps.secrets.outputs.SAP_USERNAME }}
            --cf-password=${{ steps.secrets.outputs.SAP_PASSWORD }}
```

### AWS Secrets Manager Integration

```java
// AWSSecretsRotator.java
public class AWSSecretsRotator implements CredentialRotator {
    private final AWSSecretsManager secretsManager;
    
    @Override
    public void rotateSecret(String secretId) {
        // 1. Generate new credential
        Credentials newCredentials = credentialGenerator.generateCredentials();
        
        // 2. Update target system
        boolean updated = targetSystemClient.updateCredentials(
            secretId, newCredentials.getUsername(), newCredentials.getPassword());
            
        if (!updated) {
            throw new RotationException("Failed to update credentials in target system");
        }
        
        // 3. Update AWS Secrets Manager
        UpdateSecretRequest updateRequest = new UpdateSecretRequest()
            .withSecretId(secretId)
            .withSecretString(toJson(newCredentials));
            
        secretsManager.updateSecret(updateRequest);
        
        // 4. Log rotation event
        auditLogger.logRotation(secretId, "Scheduled rotation completed");
    }
}
```

## Best Practices

1. **Never Hard-Code Secrets**:
   - Remove all hard-coded credentials from code
   - Use vault references instead of embedding secrets
   - Implement secret detection in CI/CD pipelines

2. **Implement Defense in Depth**:
   - Protect vault with multiple security layers
   - Implement network segmentation
   - Use TLS for all vault communication
   - Enable encryption at rest

3. **Automate Secret Lifecycle**:
   - Define clear secret lifecycle policies
   - Implement automated rotation
   - Track and enforce expiration
   - Remove unused secrets

4. **Regular Security Reviews**:
   - Conduct periodic access reviews
   - Test vault security with penetration testing
   - Review audit logs for anomalies
   - Update security practices

5. **Comprehensive Documentation**:
   - Document all secret management procedures
   - Maintain accurate inventory of secrets
   - Define clear ownership of secrets
   - Create emergency access procedures

## References

- [HashiCorp Vault Documentation](https://www.vaultproject.io/docs)
- [AWS Secrets Manager Documentation](https://docs.aws.amazon.com/secretsmanager/)
- [SAP Credential Store Documentation](https://help.sap.com/docs/CREDENTIAL_STORE)
- [NIST SP 800-57: Recommendation for Key Management](https://nvlpubs.nist.gov/nistpubs/SpecialPublications/NIST.SP.800-57pt1r5.pdf)
- [OWASP Secret Management Cheat Sheet](https://cheatsheetseries.owasp.org/cheatsheets/Secrets_Management_Cheat_Sheet.html)

---

*Last Updated: 2024-05-15* 