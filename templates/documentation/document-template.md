# [Document Title]

## Overview
<!-- 
Provide a brief introduction to this document:
- What is this document about?
- Why is it important?
- Who is the intended audience?
-->

## Prerequisites
<!-- 
List any requirements, knowledge, or setup needed before using this document:
- Required software
- Access permissions
- Prior knowledge
- Related documents to read first
-->

The following prerequisites are required:

- Item 1
- Item 2
- Item 3

## Detailed Content

### [Section 1]
<!-- Detailed explanation of the first main topic -->

### [Section 2]
<!-- Detailed explanation of the second main topic -->

### [Section 3]
<!-- Detailed explanation of the third main topic -->

## Examples
<!-- 
Provide practical examples demonstrating the concepts discussed:
- Code snippets with explanations
- Configuration examples
- Step-by-step workflows
-->

### Code Example

```python
# Python example with proper syntax highlighting
def connect_to_sap(system_id, client, user, password):
    """
    Establishes connection to SAP system using the provided credentials.
    
    Args:
        system_id (str): SAP system ID
        client (str): SAP client number
        user (str): Username
        password (str): Password
        
    Returns:
        connection: SAP connection object
    """
    try:
        connection = sapnwrfc.Connection(
            ashost=f"{system_id}.example.com",
            sysnr="00",
            client=client,
            user=user,
            passwd=password
        )
        return connection
    except sapnwrfc.RFCError as e:
        logging.error(f"Connection failed: {e}")
        raise
```

### Configuration Example

```yaml
# GitHub Actions workflow example with syntax highlighting
name: SAP-GitHub Sync

on:
  push:
    branches: [ main, dev ]
  pull_request:
    branches: [ main ]

jobs:
  sync_to_sap:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - name: Set up SAP connection
        uses: sap-actions/setup-connection@v1
        with:
          sap-url: ${{ secrets.SAP_URL }}
          sap-client: ${{ secrets.SAP_CLIENT }}
          sap-user: ${{ secrets.SAP_USER }}
          sap-password: ${{ secrets.SAP_PASSWORD }}
```

## Troubleshooting
<!-- 
Common issues and their solutions:
- Problem 1: Solution 1
- Problem 2: Solution 2
-->

## References
<!-- 
List all sources referenced in this document:
- [Reference Name](URL)
- Internal document links
-->

## Additional Resources
<!-- 
Provide links to related documentation, tutorials, or resources:
- [Resource 1](URL)
- [Resource 2](URL)
-->

---

**Document Metadata:**
- Last Updated: YYYY-MM-DD
- Contributors: [List of contributors]
- Version: X.Y.Z
- Status: [Draft/Review/Published]
- Related Documents: [Links to related documents]