# 📄 Implementing Custom Security Scanning for ABAP

## 📋 Table of Contents

- [📋 Overview](#overview)
- [✅ Prerequisites](#prerequisites)
- [Understanding CodeQL for ABAP](#understanding-codeql-for-abap)
- [Step-by-Step Implementation](#step-by-step-implementation)
- [Best Practices for ABAP Security Scanning](#best-practices-for-abap-security-scanning)
- [Troubleshooting](#troubleshooting)
- [➡
️ ️ Next Steps](#next-steps)


## 📋 Overview

This tutorial guides you through implementing custom security scanning for ABAP code in your GitHub repositories. You'll learn how to create custom CodeQL queries specific to ABAP security vulnerabilities, integrate them into your CI/CD pipeline, and establish automated security analysis for your SAP development.

## ✅ Prerequisites

Before starting this tutorial, ensure you have:

- GitHub Advanced Security enabled on your repository
- GitHub Actions workflows configured for your project
- Basic understanding of CodeQL and security scanning concepts
- Knowledge of common ABAP security vulnerabilities
- Access to modify GitHub Actions workflows

## Understanding CodeQL for ABAP

While GitHub's standard CodeQL doesn't natively support ABAP, this tutorial shows how to:

1. Serialize ABAP code in a format processable by CodeQL
2. Create custom CodeQL queries for specific ABAP vulnerabilities
3. Integrate the results into the GitHub security dashboard

## Step-by-Step Implementation

### 1. Set Up ABAP Code Serialization

First, configure abapGit to serialize ABAP code in a format suitable for static analysis:

1. Create a file named `abaplint.json` in your repository root with the following content:

```json
{
  "global": {
    "files": "/**/*.*",
    "skipGeneratedGatewayClasses": true,
    "skipGeneratedPersistentClasses": true,
    "skipGeneratedFunctionGroups": true
  },
  "syntax": {
    "version": "v755",
    "errorNamespace": "^(Z|Y|LZ|LY|SAPCL_)",
    "globalConstants": [],
    "globalMacros": []
  },
  "dependencies": [
    {
      "url": "https://github.com/abaplint/deps",
      "folder": "/deps",
      "files": "/src/**/*.*"
    }
  ],
  "rules": {
    "security": {
      "exclude": [],
      "severity": "error"
    },
    "sql_escape_host_variables": {
      "exclude": [],
      "severity": "error"
    },
    "injection_suspicious_sql": {
      "exclude": [],
      "severity": "error"
    },
    "cross_site_scripting": {
      "exclude": [],
      "severity": "error"
    }
  }
}
```

2. Create a GitHub Actions workflow to process the ABAP code during CI/CD:

```yaml
# .github/workflows/abap-security-scan.yml
name: ABAP Security Scan

on:
  push:
    branches: [ main, develop ]
  pull_request:
    branches: [ main ]

jobs:
  security-scan:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      
      - name: Setup Node.js
        uses: actions/setup-node@v3
        with:
          node-version: '16'
          
      - name: Install abaplint
        run: npm install -g @abaplint/cli
        
      - name: Run ABAP linting
        run: abaplint --format sarif --outformat sarif --outfile abaplint-results.sarif
        
      - name: Upload SARIF file
        uses: github/codeql-action/upload-sarif@v2
        with:
          sarif_file: abaplint-results.sarif
```

### 2. Create Custom CodeQL Queries for ABAP

Next, create custom CodeQL queries to detect ABAP-specific vulnerabilities:

1. Create a directory structure for your custom queries:

```
.github/
  codeql/
    custom-queries/
      abap/
        security/
          sql-injection.ql
          authorization-checks.ql
          client-handling.ql
          README.md
```

2. Create a query for SQL injection vulnerabilities in ABAP:

```ql
/**
 * @name ABAP SQL Injection
 * @description Detects potential SQL injection vulnerabilities in ABAP code
 * @kind path-problem
 * @id abap/sql-injection
 * @problem.severity error
 * @security-severity 9.0
 * @precision high
 * @tags security
 *       external/cwe/cwe-89
 */

import javascript

/**
 * This is a custom query that looks for patterns in serialized ABAP code
 * that might indicate SQL injection vulnerabilities.
 */
class ABAPSQLOperation extends DataFlow::Node {
  ABAPSQLOperation() {
    exists(StringLiteral s |
      s.getValue().regexpMatch("(?i).*(SELECT|UPDATE|DELETE|INSERT|MODIFY).*") and
      this.asExpr() = s
    )
  }
}

class UserInput extends DataFlow::Node {
  UserInput() {
    exists(StringLiteral s |
      s.getValue().regexpMatch("(?i).*(PARAMETERS|IMPORT|FORM).*") and
      this.asExpr() = s
    )
  }
}

class SQLEscape extends DataFlow::Node {
  SQLEscape() {
    exists(StringLiteral s |
      s.getValue().regexpMatch("(?i).*(cl_abap_dyn_prg|escape).*") and
      this.asExpr() = s
    )
  }
}

from ABAPSQLOperation sql, UserInput input, DataFlow::PathNode source, DataFlow::PathNode sink
where
  source.getNode() = input and
  sink.getNode() = sql and
  DataFlow::PathGraph::edges(source, sink) and
  not exists(SQLEscape escape |
    DataFlow::PathNode mid |
    DataFlow::PathGraph::edges(source, mid) and
    DataFlow::PathGraph::edges(mid, sink) and
    mid.getNode() = escape
  )
select sql, source, sink, "Potential SQL injection vulnerability"
```

3. Create a query for missing authorization checks:

```ql
/**
 * @name ABAP Missing Authorization Check
 * @description Identifies ABAP code that may be missing proper authorization checks
 * @kind problem
 * @id abap/missing-auth-check
 * @problem.severity warning
 * @security-severity 7.0
 * @precision medium
 * @tags security
 *       external/cwe/cwe-285
 */

import javascript

/**
 * This query analyzes serialized ABAP code to find methods or forms that
 * might be missing proper authorization checks.
 */
from StringLiteral methodDecl, StringLiteral reportExec
where
  methodDecl.getValue().regexpMatch("(?i).*METHOD .*PUBLIC.*") and
  reportExec.getValue().regexpMatch("(?i).*(UPDATE|DELETE|INSERT|MODIFY|COMMIT WORK).*") and
  exists(File f | 
    methodDecl.getFile() = f and
    reportExec.getFile() = f
  ) and
  not exists(StringLiteral authCheck |
    authCheck.getValue().regexpMatch("(?i).*(AUTHORITY-CHECK|check_authority|has_authority).*") and
    authCheck.getFile() = methodDecl.getFile()
  )
select reportExec, "Potential missing authorization check for database modification operation"
```

### 3. Configure CodeQL Analysis to Use Custom Queries

Create a CodeQL configuration file to include your custom queries:

```yaml
# .github/codeql/codeql-config.yml
name: "Custom ABAP CodeQL Configuration"

queries:
  - uses: security-and-quality
  - uses: ./.github/codeql/custom-queries/abap/security

paths:
  - src

paths-ignore:
  - deps
  - node_modules
  - vendor
```

### 4. Update GitHub Actions Workflow for CodeQL Analysis

Create or modify the CodeQL analysis workflow to use your custom configuration:

```yaml
# .github/workflows/codeql-analysis.yml
name: "CodeQL Analysis"

on:
  push:
    branches: [ main, develop ]
  pull_request:
    branches: [ main ]
  schedule:
    - cron: '30 5 * * 0'

jobs:
  analyze:
    name: Analyze
    runs-on: ubuntu-latest
    permissions:
      security-events: write
      actions: read
      contents: read

    steps:
      - name: Checkout repository
        uses: actions/checkout@v3

      - name: Initialize CodeQL
        uses: github/codeql-action/init@v2
        with:
          languages: javascript
          config-file: ./.github/codeql/codeql-config.yml

      - name: Perform CodeQL Analysis
        uses: github/codeql-action/analyze@v2
```

### 5. Create Custom Rules for Client-Dependent Access

Add a query to detect improper client handling in ABAP code:

```ql
/**
 * @name ABAP Improper Client Handling
 * @description Detects code that may not properly restrict access by client
 * @kind problem
 * @id abap/client-handling
 * @problem.severity warning
 * @security-severity 6.0
 * @precision medium
 * @tags security
 *       external/cwe/cwe-284
 */

import javascript

from StringLiteral sqlStatement
where
  sqlStatement.getValue().regexpMatch("(?i).*(SELECT|UPDATE|DELETE|INSERT).*") and
  not sqlStatement.getValue().regexpMatch("(?i).*CLIENT SPECIFIED.*") and
  not sqlStatement.getValue().regexpMatch("(?i).*MANDT = sy-mandt.*") and
  not sqlStatement.getValue().regexpMatch("(?i).*client = sy-mandt.*")
select sqlStatement, "Database operation without client restriction"
```

### 6. Set Up ABAP-Specific Code Scanning

Create an additional workflow for more complex ABAP security scanning:

```yaml
# .github/workflows/abap-deep-scan.yml
name: ABAP Deep Security Scan

on:
  schedule:
    - cron: '0 2 * * 1-5'  # Run on weekdays at 2 AM
  workflow_dispatch:

jobs:
  deep-scan:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      
      - name: Setup ABAP Security Tools
        run: |
          npm install -g @abaplint/cli
          npm install -g @abap/abap-security-scanner
          
      - name: Run Deep Security Scan
        run: abap-security-scan --source ./src --output security-findings.json
        
      - name: Convert Findings to SARIF
        run: |
          node .github/scripts/convert-to-sarif.js \
            --input security-findings.json \
            --output security-findings.sarif
            
      - name: Upload SARIF file
        uses: github/codeql-action/upload-sarif@v2
        with:
          sarif_file: security-findings.sarif
          category: abap-security
```

### 7. Implement SARIF Conversion Script

Create a script to convert proprietary security findings to SARIF format:

```javascript
// .github/scripts/convert-to-sarif.js
const fs = require('fs');
const path = require('path');

// Parse command line arguments
const args = process.argv.slice(2);
let inputFile = 'security-findings.json';
let outputFile = 'security-findings.sarif';

for (let i = 0; i < args.length; i++) {
  if (args[i] === '--input' && i + 1 < args.length) {
    inputFile = args[i + 1];
    i++;
  } else if (args[i] === '--output' && i + 1 < args.length) {
    outputFile = args[i + 1];
    i++;
  }
}

// Read input findings
const findings = JSON.parse(fs.readFileSync(inputFile, 'utf8'));

// Create SARIF output
const sarif = {
  $schema: "https://raw.githubusercontent.com/oasis-tcs/sarif-spec/master/Schemata/sarif-schema-2.1.0.json",
  version: "2.1.0",
  runs: [
    {
      tool: {
        driver: {
          name: "ABAP Security Scanner",
          version: "1.0.0",
          rules: []
        }
      },
      results: []
    }
  ]
};

// Collect unique rule IDs
const ruleMap = new Map();

// Convert findings to SARIF format
findings.forEach(finding => {
  // Create rule if not seen before
  if (!ruleMap.has(finding.ruleId)) {
    const rule = {
      id: finding.ruleId,
      shortDescription: {
        text: finding.ruleName
      },
      helpUri: `https://example.com/abap-security/${finding.ruleId}`,
      properties: {
        category: finding.category,
        tags: ["abap", "security"]
      },
      defaultConfiguration: {
        level: finding.severity === "HIGH" ? "error" : 
              finding.severity === "MEDIUM" ? "warning" : "note"
      }
    };
    sarif.runs[0].tool.driver.rules.push(rule);
    ruleMap.set(finding.ruleId, true);
  }
  
  // Add result
  const result = {
    ruleId: finding.ruleId,
    message: {
      text: finding.message
    },
    locations: [
      {
        physicalLocation: {
          artifactLocation: {
            uri: finding.fileName
          },
          region: {
            startLine: finding.lineNumber,
            startColumn: finding.columnNumber || 1
          }
        }
      }
    ]
  };
  sarif.runs[0].results.push(result);
});

// Write SARIF file
fs.writeFileSync(outputFile, JSON.stringify(sarif, null, 2));
console.log(`Converted ${findings.length} findings to SARIF format in ${outputFile}`);
```

### 8. Set Up a Security Dashboard

1. Create a report generation workflow to create security reports:
```yaml
name: Generate Security Reports

on:
  workflow_dispatch:
  schedule:
    - cron: "0 4 * * *"  # Daily at 4 AM

jobs:
  generate-reports:
    runs-on: ubuntu-latest
    steps:
      - name: Checkout code
        uses: actions/checkout@v4
        
      - name: Setup SAP Scanning Environment
        run: |
          echo "Setting up SAP scanning environment"
          # Setup steps here
          
      - name: Run Security Scans
        run: |
          echo "Running comprehensive security scans"
          # Scanning steps here
          
      - name: Generate HTML Reports
        run: |
          echo "Generating security reports"
          mkdir -p security-reports
          # Generate report files
          
      - name: Upload Reports as Artifacts
        uses: actions/upload-artifact@v3
        with:
          name: security-reports
          path: security-reports/
```

4. Access the generated reports through GitHub Actions artifacts

## Best Practices for ABAP Security Scanning

### Common ABAP Vulnerabilities to Scan For

Configure your custom scanning to detect these common ABAP security issues:

1. **SQL Injection**
   - Dynamic SQL statements without proper escaping
   - Direct concatenation of user input into SQL strings

2. **Missing Authorization Checks**
   - Methods or forms that modify data without authority checks
   - Access to sensitive transactions without proper verification

3. **Client Handling Issues**
   - Table access without client restriction
   - Cross-client data access

4. **Secure Storage**
   - Insecure handling of credentials or sensitive data
   - Hardcoded passwords or tokens

5. **Input Validation**
   - Missing or insufficient validation of user inputs
   - Improper handling of special characters

## Troubleshooting

| Issue | Solution |
|-------|----------|
| CodeQL doesn't recognize ABAP syntax | ABAP is analyzed as serialized text; make sure abapGit is configured to produce consistent serialization |
| False positives in SQL injection detection | Refine patterns in your custom queries to recognize valid escaping techniques |
| Security dashboard shows no data | Check if SARIF files are being properly generated and if the dashboard workflow can access them |
| Custom queries aren't running | Verify the CodeQL configuration file path and that it correctly references your custom queries |

## ➡️ Next Steps

After implementing custom security scanning for ABAP, consider these advanced configurations:

- [Setting Up Microsoft Sentinel for SAP Monitoring](./sentinel-setup.md) for comprehensive security monitoring
- [Securing API Management](./secure-apim.md) for securing your API interfaces
- Creating a security gate in your CI/CD pipeline that blocks PR merges with critical findings
- Implementing automated remediation suggestions for common ABAP security issues

---

**Document Metadata:**
- Last Updated: 2023-09-01
- Contributors: Security Team
- Version: 1.0.0
- Status: Published
---

<details>
<summary><strong>📊 Document Metadata</strong></summary>

- **Last Updated:** 2025-04-07
- **Version:** 1.0.0
- **Status:** Published
</details>
