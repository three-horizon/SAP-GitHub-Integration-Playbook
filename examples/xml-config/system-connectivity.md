---
layout: default
title: System Connectivity Configuration
---

# System Connectivity Configuration

This document provides XML configuration templates for establishing connectivity between SAP systems and GitHub.

## Overview

System connectivity configurations define how SAP systems connect to GitHub APIs and repositories. These XML-based configurations can be used in SAP PI/PO, SAP Cloud Integration, or other middleware components to establish secure and reliable connections.

## Prerequisites

- Access to SAP middleware components (PI/PO, CPI, etc.)
- Network connectivity between SAP and GitHub
- Authentication credentials for GitHub
- Appropriate authorizations in SAP systems

## SAP PI/PO Configuration

### Sender Communication Channel

```xml
<?xml version="1.0" encoding="UTF-8"?>
<xi:include xmlns:xi="http://www.w3.org/2001/XInclude" href="http://integration.sap.com/xi/XI/System/REST">
  <SenderCommunicationChannel>
    <name>GITHUB_API_SENDER</name>
    <description>GitHub API Sender Channel</description>
    <transport>HTTP</transport>
    <adapter>REST</adapter>
    <adapterConfig>
      <adapterSpecific>
        <property name="TransportProtocolVersion">HTTP/1.1</property>
        <property name="Compression">none</property>
        <property name="MessageProtocol">REST</property>
        <property name="Authentication">OAuth2ClientCredentials</property>
        <property name="MaxConnections">10</property>
        <property name="Timeout">300000</property>
        <property name="MaxRetries">3</property>
      </adapterSpecific>
      <security>
        <authentication>
          <property name="OAuth2ClientId">YOUR_GITHUB_CLIENT_ID</property>
          <property name="OAuth2ClientSecret">YOUR_GITHUB_CLIENT_SECRET</property>
          <property name="OAuth2TokenEndpoint">https://github.com/login/oauth/access_token</property>
          <property name="OAuth2AuthEndpoint">https://github.com/login/oauth/authorize</property>
          <property name="OAuth2Scope">repo,admin:repo_hook</property>
        </authentication>
      </security>
    </adapterConfig>
    <transportConfig>
      <URL>https://api.github.com</URL>
    </transportConfig>
  </SenderCommunicationChannel>
</xi:include>
```

### Receiver Communication Channel

```xml
<?xml version="1.0" encoding="UTF-8"?>
<xi:include xmlns:xi="http://www.w3.org/2001/XInclude" href="http://integration.sap.com/xi/XI/System/REST">
  <ReceiverCommunicationChannel>
    <name>SAP_SYSTEM_RECEIVER</name>
    <description>SAP System Receiver Channel</description>
    <transport>HTTP</transport>
    <adapter>REST</adapter>
    <adapterConfig>
      <adapterSpecific>
        <property name="TransportProtocolVersion">HTTP/1.1</property>
        <property name="Compression">none</property>
        <property name="MessageProtocol">REST</property>
        <property name="Authentication">BasicAuthentication</property>
        <property name="MaxConnections">10</property>
        <property name="Timeout">300000</property>
      </adapterSpecific>
      <security>
        <authentication>
          <property name="User">SAP_INTEGRATION_USER</property>
          <property name="Password">SAP_INTEGRATION_PASSWORD</property>
        </authentication>
      </security>
    </adapterConfig>
    <transportConfig>
      <URL>https://your-sap-system.example.com/odata/api</URL>
    </transportConfig>
  </ReceiverCommunicationChannel>
</xi:include>
```

## SAP Cloud Integration Configuration

### GitHub API Connection (iflow.xml)

```xml
<?xml version="1.0" encoding="UTF-8"?>
<bpmn2:definitions xmlns:bpmn2="http://www.omg.org/spec/BPMN/20100524/MODEL" 
                   xmlns:ifl="http://www.sap.com/xi/XI/Integration/Message">
  <bpmn2:process id="GitHubIntegration">
    <bpmn2:startEvent id="StartEvent">
      <bpmn2:outgoing>SequenceFlow_1</bpmn2:outgoing>
    </bpmn2:startEvent>
    
    <bpmn2:sequenceFlow id="SequenceFlow_1" sourceRef="StartEvent" targetRef="HTTPRequestToGitHub"/>
    
    <bpmn2:serviceTask id="HTTPRequestToGitHub">
      <bpmn2:incoming>SequenceFlow_1</bpmn2:incoming>
      <bpmn2:outgoing>SequenceFlow_2</bpmn2:outgoing>
      <bpmn2:extensionElements>
        <ifl:property name="ComponentType" value="HTTPAdapter"/>
        <ifl:property name="Method" value="GET"/>
        <ifl:property name="Address" value="https://api.github.com/repos/${property.organization}/${property.repository}/commits"/>
        <ifl:property name="Authentication" value="OAuth2ClientCredentials"/>
        <ifl:property name="ProxyType" value="Internet"/>
        <ifl:property name="OAuth2ClientId" value="${property.github.client_id}"/>
        <ifl:property name="OAuth2ClientSecret" value="${property.github.client_secret}"/>
        <ifl:property name="OAuth2TokenEndpoint" value="https://github.com/login/oauth/access_token"/>
        <ifl:property name="OAuth2Scope" value="repo,admin:repo_hook"/>
        <ifl:property name="RequestHeaders">
          <entry key="Accept" value="application/vnd.github.v3+json"/>
          <entry key="User-Agent" value="SAP-GitHub-Integration"/>
        </ifl:property>
      </bpmn2:extensionElements>
    </bpmn2:serviceTask>
    
    <bpmn2:sequenceFlow id="SequenceFlow_2" sourceRef="HTTPRequestToGitHub" targetRef="ContentModifier"/>
    
    <bpmn2:serviceTask id="ContentModifier">
      <bpmn2:incoming>SequenceFlow_2</bpmn2:incoming>
      <bpmn2:outgoing>SequenceFlow_3</bpmn2:outgoing>
      <bpmn2:extensionElements>
        <ifl:property name="ComponentType" value="ContentModifier"/>
        <ifl:property name="MessageProtocol" value="JSON"/>
        <ifl:property name="OperationType" value="Transform"/>
        <ifl:property name="Script"><![CDATA[
          import com.sap.gateway.ip.core.customdev.util.Message;
          import groovy.json.JsonSlurper;
          import groovy.json.JsonBuilder;
          
          def Message processData(Message message) {
              def body = message.getBody(String);
              def jsonSlurper = new JsonSlurper();
              def commits = jsonSlurper.parseText(body);
              
              def simplifiedCommits = commits.collect { commit ->
                  [
                      sha: commit.sha,
                      author: commit.commit.author.name,
                      email: commit.commit.author.email,
                      date: commit.commit.author.date,
                      message: commit.commit.message,
                      url: commit.html_url
                  ]
              }
              
              def builder = new JsonBuilder(simplifiedCommits);
              message.setBody(builder.toString());
              return message;
          }
        ]]></ifl:property>
      </bpmn2:extensionElements>
    </bpmn2:serviceTask>
    
    <bpmn2:sequenceFlow id="SequenceFlow_3" sourceRef="ContentModifier" targetRef="HTTPRequestToSAP"/>
    
    <bpmn2:serviceTask id="HTTPRequestToSAP">
      <bpmn2:incoming>SequenceFlow_3</bpmn2:incoming>
      <bpmn2:outgoing>SequenceFlow_4</bpmn2:outgoing>
      <bpmn2:extensionElements>
        <ifl:property name="ComponentType" value="HTTPAdapter"/>
        <ifl:property name="Method" value="POST"/>
        <ifl:property name="Address" value="https://your-sap-system.example.com/odata/api/CommitImport"/>
        <ifl:property name="Authentication" value="BasicAuthentication"/>
        <ifl:property name="ProxyType" value="OnPremise"/>
        <ifl:property name="User" value="${property.sap.user}"/>
        <ifl:property name="Password" value="${property.sap.password}"/>
        <ifl:property name="RequestHeaders">
          <entry key="Content-Type" value="application/json"/>
          <entry key="X-CSRF-Token" value="${header.x-csrf-token}"/>
        </ifl:property>
      </bpmn2:extensionElements>
    </bpmn2:serviceTask>
    
    <bpmn2:sequenceFlow id="SequenceFlow_4" sourceRef="HTTPRequestToSAP" targetRef="EndEvent"/>
    
    <bpmn2:endEvent id="EndEvent">
      <bpmn2:incoming>SequenceFlow_4</bpmn2:incoming>
    </bpmn2:endEvent>
  </bpmn2:process>
</bpmn2:definitions>
```

### Integration Flow Parameter Properties (parameters.prop)

```properties
# GitHub Integration Properties
github.client_id=your_github_client_id
github.client_secret=your_github_client_secret
organization=your_github_organization
repository=your_repository_name

# SAP System Connection
sap.user=SAP_INTEGRATION_USER
sap.password=SAP_INTEGRATION_PASSWORD
sap.url=https://your-sap-system.example.com/odata/api
```

## SAP BTP Destination Configuration

### Destination Service Configuration (destination.json)

```json
{
  "Name": "GitHub-API",
  "Type": "HTTP",
  "URL": "https://api.github.com",
  "Authentication": "OAuth2ClientCredentials",
  "ProxyType": "Internet",
  "Description": "GitHub API Connection",
  "OAuth2ClientId": "${github.client_id}",
  "OAuth2ClientSecret": "${github.client_secret}",
  "OAuth2TokenServiceURL": "https://github.com/login/oauth/access_token",
  "OAuth2TokenServiceUser": "${github.client_id}",
  "OAuth2TokenServicePassword": "${github.client_secret}",
  "Additional Properties": [
    {
      "Key": "GitHub-Accept",
      "Value": "application/vnd.github.v3+json"
    },
    {
      "Key": "User-Agent",
      "Value": "SAP-GitHub-Integration"
    },
    {
      "Key": "OAuth2Scope",
      "Value": "repo,admin:repo_hook"
    }
  ]
}
```

## SAP Cloud Connector Configuration

### Cloud Connector Access Control (access-control.xml)

```xml
<?xml version="1.0" encoding="UTF-8"?>
<AccessControl xmlns="http://sap.com/cloudconnector/accesscontrol">
  <Resource>
    <Type>REST</Type>
    <URI>https://api.github.com</URI>
    <Enabled>true</Enabled>
    <ProxyType>Internet</ProxyType>
    <Authentication>None</Authentication>
    <Description>GitHub API Access</Description>
    <AccessPaths>
      <AccessPath>
        <Path>/</Path>
        <Description>Root Path</Description>
        <Methods>
          <Method>GET</Method>
          <Method>POST</Method>
          <Method>PUT</Method>
          <Method>DELETE</Method>
          <Method>HEAD</Method>
        </Methods>
      </AccessPath>
      <AccessPath>
        <Path>/repos</Path>
        <Description>Repository Access</Description>
        <Methods>
          <Method>GET</Method>
          <Method>POST</Method>
          <Method>PUT</Method>
          <Method>DELETE</Method>
        </Methods>
      </AccessPath>
    </AccessPaths>
  </Resource>
</AccessControl>
```

## Best Practices

### Security Configuration

1. **Credential Protection**
   - Store credentials in secure parameter stores
   - Use alias substitution for sensitive information
   - Implement credential rotation mechanisms

2. **Transport Security**
   - Enforce TLS 1.2 or higher
   - Implement certificate validation
   - Use secure signature algorithms

### Performance Optimization

1. **Connection Pooling**
   - Set appropriate connection pool sizes
   - Configure idle timeout settings
   - Monitor connection usage

2. **Error Handling**
   - Implement retry mechanisms
   - Define clear error handling policies
   - Log connection errors for monitoring

### Monitoring

1. **Health Checks**
   - Implement regular connectivity checks
   - Set up alerts for connection failures
   - Monitor response times

2. **Logging**
   - Log connection establishment and teardown
   - Record authentication events
   - Track API rate limits

## Troubleshooting

### Common Issues and Solutions

1. **Connection Timeout**
   - **Issue**: Connection to GitHub API times out
   - **Solution**: Check network connectivity, proxy settings, and increase timeout values

2. **Authentication Failure**
   - **Issue**: OAuth token exchange fails
   - **Solution**: Verify client ID and secret, check token endpoint URL

3. **SSL/TLS Errors**
   - **Issue**: SSL handshake failure
   - **Solution**: Update certificates, check TLS version compatibility

## Next Steps

- Implement [OAuth Configuration](../security-config/oauth-configuration.md) for authentication
- Set up [API Management Policies](../api-management/sap-to-github.md) for API governance
- Configure [GitHub Actions](../github-actions/ui5-build-pipeline.md) for automated workflows

---

**Document Metadata:**
- Last Updated: 2023-11-15
- Version: 1.0.0
- Status: Published 