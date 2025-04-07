# 📄 Creating Custom GitHub Actions for SAP-Specific Needs

## 📋 Table of Contents

- [📋 Overview](#overview)
- [✅ Prerequisites](#prerequisites)
- [Step 1: Understanding GitHub Actions Architecture](#step-1-understanding-github-actions-architecture)
- [Step 2: Setting Up Your Development Environment](#step-2-setting-up-your-development-environment)
- [Step 3: Creating a Basic SAP-Integrated Action](#step-3-creating-a-basic-sap-integrated-action)
- [Step 4: Building and Testing the Action Locally](#step-4-building-and-testing-the-action-locally)
- [Step 5: Packaging Your Action for Distribution](#step-5-packaging-your-action-for-distribution)
- [Step 6: Publishing Your Action to GitHub](#step-6-publishing-your-action-to-github)
- [Step 7: Creating Advanced SAP Actions with RFC](#step-7-creating-advanced-sap-actions-with-rfc)
- [Step 8: Creating Composite Actions for Common SAP Workflows](#step-8-creating-composite-actions-for-common-sap-workflows)
- [Step 9: Implementing Advanced Security Features](#step-9-implementing-advanced-security-features)
- [Step 10: Implementing Telemetry and Logging](#step-10-implementing-telemetry-and-logging)
- [Best Practices for SAP-Specific Actions](#best-practices-for-sap-specific-actions)
- [Validation](#validation)
- [Troubleshooting](#troubleshooting)
- [➡
️ ️ Next Steps](#next-steps)
- [Additional Resources](#additional-resources)


This tutorial guides you through the process of developing custom GitHub Actions specifically tailored for SAP development workflows, enabling you to automate SAP-specific tasks in your CI/CD pipeline.

## 📋 Overview

Custom GitHub Actions allow you to extend GitHub's workflow capabilities with specialized functionality. For SAP development, custom actions can:

- Interact with SAP systems via RFC, OData, or REST APIs
- Validate SAP transports and objects
- Perform ABAP syntax and code quality checks
- Automate deployment and testing processes
- Connect GitHub workflows to SAP processes and tools

By creating custom actions tailored to your SAP environment, you can significantly improve developer productivity and ensure consistent processes.

## ✅ Prerequisites

Before developing custom GitHub Actions for SAP, ensure you have:

- [ ] A GitHub repository set up for SAP development (see [Setting Up Your First SAP-GitHub Repository](../basic/first-repository.md))
- [ ] GitHub Actions workflows established (see [Setting Up GitHub Actions Workflows](../basic/github-actions-setup.md))
- [ ] Node.js development environment (v16+ recommended)
- [ ] Basic JavaScript knowledge
- [ ] Integration endpoints for SAP communication (RFC, REST APIs, or OData services)
- [ ] A private GitHub organization for storing custom actions

## Step 1: Understanding GitHub Actions Architecture

Before developing custom actions, understand the GitHub Actions architecture:

1. **Types of GitHub Actions**:
   - JavaScript actions (runs directly in the GitHub Actions environment)
   - Docker container actions (runs in a Docker container)
   - Composite actions (combines multiple steps into a single action)

2. **Action Components**:
   - `action.yml` - Defines metadata, inputs, outputs, and runtime
   - Main code (`index.js` or `Dockerfile`)
   - Supporting files and dependencies

3. **Execution Environment**:
   - Actions run on GitHub-hosted or self-hosted runners
   - GitHub-hosted runners provide a clean environment for each job
   - Self-hosted runners can have specialized configurations for SAP connectivity

For SAP integrations, JavaScript actions are typically the most straightforward for API interactions, while Docker actions may be preferred for more complex scenarios requiring specific libraries.

## Step 2: Setting Up Your Development Environment

1. **Create a local development environment**:

   ```sh
   mkdir sap-action-dev
   cd sap-action-dev
   npm init -y
   npm install @actions/core @actions/github axios
   ```

2. **Install additional packages for SAP connectivity** (as needed):

   ```sh
   # For RFC connections
   npm install node-rfc

   # For OData
   npm install @sap/xsenv @sap/xssec @sap-cloud-sdk/http-client
   ```

3. **Configure a testing environment**:
   - Set up environment variables for SAP connectivity
   - Create test scripts that simulate GitHub Actions inputs
   - Configure access to your SAP development system

## Step 3: Creating a Basic SAP-Integrated Action

Let's start with a simple action that checks SAP transport status:

1. **Create the action directory structure**:

   ```
   sap-transport-checker/
   ├── action.yml
   ├── index.js
   ├── package.json
   ├── dist/
   └── src/
       ├── main.js
       └── sap-client.js
   ```

2. **Define the action metadata in `action.yml`**:

   ```yaml
   name: 'SAP Transport Checker'
   description: 'Checks the status of SAP transport requests'
   inputs:
     api-url:
       description: 'API Management endpoint URL'
       required: true
     api-key:
       description: 'API Management API key'
       required: true
     transport-number:
       description: 'SAP transport request number'
       required: true
   outputs:
     status:
       description: 'Status of the transport request'
     description:
       description: 'Description of the transport request'
     object_count:
       description: 'Number of objects in the transport'
   runs:
     using: 'node16'
     main: 'dist/index.js'
   ```

3. **Create an SAP client module in `src/sap-client.js`**:

   ```javascript
   const axios = require('axios');

   class SapClient {
     constructor(apiUrl, apiKey) {
       this.apiUrl = apiUrl;
       this.apiKey = apiKey;
       this.client = axios.create({
         baseURL: apiUrl,
         headers: {
           'Content-Type': 'application/json',
           'Ocp-Apim-Subscription-Key': apiKey
         }
       });
     }

     async getTransportStatus(transportNumber) {
       try {
         const response = await this.client.get(`/transports/${transportNumber}`);
         return {
           success: true,
           data: response.data
         };
       } catch (error) {
         return {
           success: false,
           error: error.response?.data?.message || error.message
         };
       }
     }
   }

   module.exports = SapClient;
   ```

4. **Create the main action code in `src/main.js`**:

   ```javascript
   const core = require('@actions/core');
   const SapClient = require('./sap-client');

   async function run() {
     try {
       // Get inputs
       const apiUrl = core.getInput('api-url', { required: true });
       const apiKey = core.getInput('api-key', { required: true });
       const transportNumber = core.getInput('transport-number', { required: true });

       // Initialize SAP client
       const sapClient = new SapClient(apiUrl, apiKey);

       // Get transport status
       console.log(`Checking transport ${transportNumber}...`);
       const result = await sapClient.getTransportStatus(transportNumber);

       if (!result.success) {
         throw new Error(`Failed to check transport: ${result.error}`);
       }

       const transport = result.data;
       console.log(`Transport ${transportNumber} status: ${transport.status}`);

       // Set outputs
       core.setOutput('status', transport.status);
       core.setOutput('description', transport.description);
       core.setOutput('object_count', transport.objects?.length || 0);

       // Handle different statuses
       if (transport.status === 'Error') {
         core.setFailed(`Transport ${transportNumber} has errors: ${transport.message}`);
       }
     } catch (error) {
       core.setFailed(`Action failed: ${error.message}`);
     }
   }

   module.exports = run;
   ```

5. **Create the entry point at `index.js`**:

   ```javascript
   const run = require('./src/main');

   run();
   ```

## Step 4: Building and Testing the Action Locally

Before publishing, test your action locally:

1. **Create a simple test script in `test/test.js`**:

   ```javascript
   // Mock GitHub Actions core module
   const mockCore = {
     getInput: jest.fn(),
     setOutput: jest.fn(),
     setFailed: jest.fn()
   };

   // Mock Axios for API calls
   jest.mock('axios', () => ({
     create: jest.fn().mockReturnValue({
       get: jest.fn()
     })
   }));

   // Import with mocks
   const axios = require('axios');
   const run = require('../src/main');

   // Setup inputs
   mockCore.getInput.mockImplementation((name) => {
     const inputs = {
       'api-url': 'https://example.com/api',
       'api-key': 'test-api-key',
       'transport-number': 'DEVK900123'
     };
     return inputs[name];
   });

   // Mock successful response
   axios.create().get.mockResolvedValue({
     data: {
       trkorr: 'DEVK900123',
       status: 'Released',
       description: 'Test Transport',
       objects: [{ name: 'ZCL_TEST', type: 'CLAS' }]
     }
   });

   // Run the test
   async function testRun() {
     await run();
     
     // Check outputs
     console.log('setOutput calls:', mockCore.setOutput.mock.calls);
     console.log('setFailed calls:', mockCore.setFailed.mock.calls);
   }

   testRun();
   ```

2. **Run your test script**:

   ```sh
   node test/test.js
   ```

3. **Test with real SAP connection** (optional):
   
   Create a test script that uses your real API endpoints (for development only, never commit credentials):

   ```javascript
   const run = require('./src/main');
   
   // Set environment variables to simulate GitHub Actions
   process.env['INPUT_API-URL'] = 'https://your-api-url.com';
   process.env['INPUT_API-KEY'] = 'your-api-key';
   process.env['INPUT_TRANSPORT-NUMBER'] = 'DEVK900123';
   
   // Register output handlers
   const outputs = {};
   require('@actions/core').setOutput = (name, value) => {
     outputs[name] = value;
     console.log(`Output ${name}: ${value}`);
   };
   
   // Run the action
   run().then(() => {
     console.log('Action completed successfully');
     console.log('Outputs:', outputs);
   }).catch(err => {
     console.error('Action failed:', err);
   });
   ```

## Step 5: Packaging Your Action for Distribution

GitHub Actions need to be packaged correctly for distribution:

1. **Install ncc to create a single file bundle**:

   ```sh
   npm install -g @vercel/ncc
   ```

2. **Build the distribution package**:

   ```sh
   ncc build index.js --license licenses.txt
   ```

   This creates a single file in the `dist` directory containing all dependencies.

3. **Update package.json with proper metadata**:

   ```json
   {
     "name": "sap-transport-checker",
     "version": "1.0.0",
     "description": "GitHub Action to check SAP transport status",
     "main": "dist/index.js",
     "scripts": {
       "build": "ncc build index.js --license licenses.txt",
       "test": "jest"
     },
     "keywords": [
       "github",
       "actions",
       "sap",
       "transport"
     ],
     "author": "Your Name",
     "license": "MIT",
     "dependencies": {
       "@actions/core": "^1.10.0",
       "@actions/github": "^5.1.1",
       "axios": "^1.3.4"
     },
     "devDependencies": {
       "@vercel/ncc": "^0.36.1",
       "jest": "^29.5.0"
     }
   }
   ```

## Step 6: Publishing Your Action to GitHub

Now publish your action to a GitHub repository:

1. **Create a new repository** in your GitHub organization.

2. **Push your action code** to the repository:

   ```sh
   git init
   git add .
   git commit -m "Initial commit of SAP Transport Checker action"
   git remote add origin https://github.com/your-org/sap-transport-checker.git
   git push -u origin main
   ```

3. **Tag a release version**:

   ```sh
   git tag -a v1.0.0 -m "First release"
   git push origin v1.0.0
   ```

4. **Use your action in a workflow**:

   ```yaml
   steps:
     - uses: your-org/sap-transport-checker@v1
       with:
         api-url: ${{ secrets.SAP_API_URL }}
         api-key: ${{ secrets.SAP_API_KEY }}
         transport-number: 'DEVK900123'
   ```

## Step 7: Creating Advanced SAP Actions with RFC

For direct RFC connectivity to SAP (without API Management), create a Docker-based action:

1. **Create the action structure**:

   ```
   sap-rfc-action/
   ├── action.yml
   ├── Dockerfile
   ├── entrypoint.sh
   └── src/
       └── index.js
   ```

2. **Define the action in `action.yml`**:

   ```yaml
   name: 'SAP RFC Action'
   description: 'Executes RFC function modules in SAP systems'
   inputs:
     sap-host:
       description: 'SAP host address'
       required: true
     sap-client:
       description: 'SAP client number'
       required: true
     sap-user:
       description: 'SAP username'
       required: true
     sap-password:
       description: 'SAP password'
       required: true
     function-module:
       description: 'RFC function module to call'
       required: true
     function-params:
       description: 'JSON encoded parameters for the function module'
       required: true
   outputs:
     result:
       description: 'JSON encoded result of the function call'
   runs:
     using: 'docker'
     image: 'Dockerfile'
   ```

3. **Create the Dockerfile**:

   ```dockerfile
   FROM node:16-slim

   # Install SAP Node RFC library dependencies
   RUN apt-get update && apt-get install -y \
       wget \
       unzip \
       libssl-dev \
       build-essential \
       python3 \
       && rm -rf /var/lib/apt/lists/*

   # Download and install SAP NW RFC SDK (you'll need to provide the SDK)
   WORKDIR /tmp
   COPY nwrfcsdk.zip .
   RUN mkdir -p /usr/local/sap/nwrfcsdk && \
       unzip nwrfcsdk.zip -d /usr/local/sap/nwrfcsdk && \
       rm nwrfcsdk.zip
   ENV SAPNWRFC_HOME=/usr/local/sap/nwrfcsdk

   # Set environment variables for Node RFC
   ENV LD_LIBRARY_PATH=$SAPNWRFC_HOME/lib:$LD_LIBRARY_PATH

   # Create app directory
   WORKDIR /app
   COPY package*.json ./
   RUN npm install
   COPY src/ ./src/
   COPY entrypoint.sh /entrypoint.sh
   RUN chmod +x /entrypoint.sh

   ENTRYPOINT ["/entrypoint.sh"]
   ```

4. **Create the entrypoint script**:

   ```bash
   #!/bin/bash
   set -e

   node /app/src/index.js
   ```

5. **Create the action code**:

   ```javascript
   const core = require('@actions/core');
   const noderfc = require('node-rfc');

   async function run() {
     try {
       // Get inputs
       const sapHost = core.getInput('sap-host', { required: true });
       const sapClient = core.getInput('sap-client', { required: true });
       const sapUser = core.getInput('sap-user', { required: true });
       const sapPassword = core.getInput('sap-password', { required: true });
       const functionModule = core.getInput('function-module', { required: true });
       const functionParams = JSON.parse(core.getInput('function-params', { required: true }));

       // Configure RFC client
       const client = new noderfc.Client({
         ashost: sapHost,
         sysnr: '00',  // Adjust as needed
         client: sapClient,
         user: sapUser,
         passwd: sapPassword,
         lang: 'EN'
       });

       // Connect to SAP
       await client.open();
       console.log('Connected to SAP system');

       // Call function module
       console.log(`Calling function module ${functionModule}...`);
       const result = await client.call(functionModule, functionParams);
       console.log('Function call successful');

       // Set output
       core.setOutput('result', JSON.stringify(result));

       // Close connection
       await client.close();
       console.log('Connection closed');
     } catch (error) {
       core.setFailed(`Action failed: ${error.message}`);
     }
   }

   run();
   ```

6. **Package and publish** as described earlier.

## Step 8: Creating Composite Actions for Common SAP Workflows

For reusable sequences of steps, create composite actions:

1. **Create a composite action for transport validation and release**:

   ```yaml
   # action.yml
   name: 'SAP Transport Validation and Release'
   description: 'Validates SAP transport request and releases it if valid'
   inputs:
     api-url:
       description: 'API Management endpoint URL'
       required: true
     api-key:
       description: 'API Management API key'
       required: true
     transport-number:
       description: 'SAP transport request number'
       required: true
     target-system:
       description: 'Target system for transport release'
       required: true
   outputs:
     status:
       description: 'Status of the operation (success/failure)'
     message:
       description: 'Detailed message about the operation'
   runs:
     using: "composite"
     steps:
       - name: Validate transport
         uses: your-org/sap-transport-checker@v1
         id: validate
         with:
           api-url: ${{ inputs.api-url }}
           api-key: ${{ inputs.api-key }}
           transport-number: ${{ inputs.transport-number }}
       
       - name: Check validation result
         shell: bash
         run: |
           if [[ "${{ steps.validate.outputs.status }}" == "Error" ]]; then
             echo "::error::Transport validation failed: ${{ steps.validate.outputs.status }}"
             exit 1
           fi
           echo "Transport validation successful"
       
       - name: Release transport
         if: success()
         uses: your-org/sap-transport-releaser@v1
         id: release
         with:
           api-url: ${{ inputs.api-url }}
           api-key: ${{ inputs.api-key }}
           transport-number: ${{ inputs.transport-number }}
           target-system: ${{ inputs.target-system }}
       
       - name: Set outputs
         shell: bash
         run: |
           echo "status=success" >> $GITHUB_OUTPUT
           echo "message=Transport ${{ inputs.transport-number }} validated and released to ${{ inputs.target-system }}" >> $GITHUB_OUTPUT
         if: success()
       
       - name: Set failure outputs
         shell: bash
         run: |
           echo "status=failure" >> $GITHUB_OUTPUT
           echo "message=Transport operation failed" >> $GITHUB_OUTPUT
         if: failure()
   ```

## Step 9: Implementing Advanced Security Features

For SAP actions, implement these security best practices:

1. **Secure handling of credentials**:

   ```javascript
   // Never log credentials
   const sapPassword = core.getInput('sap-password', { required: true });
   console.log('Password received'); // Don't log the actual password

   // Mask sensitive outputs
   core.setSecret(someOutputThatMightContainSensitiveData);
   ```

2. **Implement retries with exponential backoff**:

   ```javascript
   const axios = require('axios');
   const axiosRetry = require('axios-retry');

   const client = axios.create({
     baseURL: apiUrl,
     headers: { /* ... */ }
   });

   // Configure retry behavior
   axiosRetry(client, {
     retries: 3,
     retryDelay: axiosRetry.exponentialDelay,
     retryCondition: (error) => {
       // Retry on network errors or 5xx responses
       return axiosRetry.isNetworkOrIdempotentRequestError(error) || 
              (error.response && error.response.status >= 500);
     }
   });
   ```

3. **Validate all inputs**:

   ```javascript
   function validateTransportNumber(transport) {
     const transportPattern = /^[A-Z]{4}\d{6}$/;
     if (!transportPattern.test(transport)) {
       throw new Error(`Invalid transport number format: ${transport}`);
     }
     return transport;
   }

   const transportNumber = validateTransportNumber(
     core.getInput('transport-number', { required: true })
   );
   ```

## Step 10: Implementing Telemetry and Logging

For enterprise actions, add telemetry and logging:

1. **Add structured logging**:

   ```javascript
   function log(level, message, context = {}) {
     const timestamp = new Date().toISOString();
     const entry = {
       timestamp,
       level,
       message,
       action: 'sap-transport-checker',
       ...context
     };
     
     // For normal logs
     if (level !== 'error') {
       console.log(JSON.stringify(entry));
     } else {
       // For errors
       console.error(JSON.stringify(entry));
     }
   }

   // Usage
   log('info', 'Checking transport status', { transport: transportNumber });
   try {
     // operation...
   } catch (error) {
     log('error', 'Operation failed', { 
       transport: transportNumber, 
       error: error.message,
       stack: error.stack
     });
   }
   ```

2. **Add usage telemetry** (optional, with clear disclosure):

   ```javascript
   async function sendTelemetry(actionName, status, durationMs) {
     // Only if explicitly enabled
     if (core.getInput('enable-telemetry') !== 'true') return;
     
     try {
       // Send anonymized usage data to your internal endpoint
       await axios.post('https://your-telemetry-endpoint.com/actions', {
         action: actionName,
         status,
         duration: durationMs,
         runner: process.env.RUNNER_OS || 'unknown',
         timestamp: new Date().toISOString()
       });
     } catch (error) {
       // Never fail the action due to telemetry
       console.error('Telemetry error:', error.message);
     }
   }
   ```

## Best Practices for SAP-Specific Actions

1. **Handle SAP-specific exceptions**:
   - RFC connectivity issues
   - Transport lock situations
   - Authorization errors
   - System unavailability

2. **Cache SAP connections** where appropriate:
   - Use a connection pool for multiple RFC calls
   - Implement proper connection cleanup
   - Handle timeouts gracefully

3. **Respect SAP performance implications**:
   - Limit parallel connections to SAP systems
   - Implement rate limiting for high-volume operations
   - Consider background job options for long-running processes

4. **Document extensively**:
   - Document required SAP authorizations
   - Specify supported SAP system versions
   - Detail required RFC function modules and their parameters
   - Provide examples that work with real SAP systems

## Validation

Your custom action is ready for production when:

- It passes all local tests with mock and real SAP connections
- Proper error handling is implemented for all SAP-specific scenarios
- Security measures are in place for credential handling
- Documentation clearly explains inputs, outputs, and SAP prerequisites
- Version is tagged and action is available in your GitHub organization

## Troubleshooting

### Common Issues

| Issue | Solution |
|-------|----------|
| RFC connectivity failures | Check network connectivity, firewall settings, and SAP system availability |
| Authentication errors | Verify service user permissions in SAP and credential passing in workflows |
| Action not found | Ensure the action is properly published and the version tag exists |
| Node-RFC compatibility issues | Build with the correct node-rfc version matching your SAP NW RFC SDK |
| Workflow timeouts | Implement proper timeout handling and consider breaking large operations into smaller steps |

## ➡️ Next Steps

After creating basic SAP custom actions, consider:

- [Implementing Multi-System Deployments](./multi-system.md) for enterprise-scale implementations
- [Integration with SAP Solution Manager](./solman-integration.md) for complete ALM coverage
- [Automated Transport Creation](../transport/automated-creation.md) for end-to-end automation
- [CI/CD Best Practices](../../implementation/workflows/ci-cd-setup.md) for comprehensive pipeline design

## Additional Resources

- [GitHub Actions Documentation](https://docs.github.com/en/actions/creating-actions)
- [Node-RFC Documentation](https://github.com/SAP/node-rfc)
- [SAP Cloud SDK](https://sap.github.io/cloud-sdk/)
- [Docker Actions Documentation](https://docs.github.com/en/actions/creating-actions/creating-a-docker-container-action)

---

*Last Updated: [YYYY-MM-DD] for [GitHub Enterprise X.Y.Z, SAP NetWeaver X.Y.Z]* 
---


