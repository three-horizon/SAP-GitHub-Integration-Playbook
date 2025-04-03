#!/bin/bash

# Script to implement the Comprehensive Technical Playbook Template structure
# Usage: ./implement-playbook-template.sh

echo "Starting implementation of Comprehensive Technical Playbook Template..."

# Create main directory structure
echo "Creating directory structure..."

# Create root level files
touch README.md
touch EXECUTIVE-OVERVIEW.md
touch CONTRIBUTING.md
touch LICENSE
touch SECURITY.md

# Create main directories
mkdir -p docs/1-architecture
mkdir -p docs/2-implementation-guide
mkdir -p docs/3-developer-guide
mkdir -p docs/4-operations-guide
mkdir -p docs/5-reference
mkdir -p docs/6-appendices
mkdir -p examples
mkdir -p templates
mkdir -p scripts
mkdir -p assets/images

# Copy existing content if available
echo "Copying existing content to new structure..."

# Root files
if [ -f "docs/1-executive-overview/README.md" ]; then
  cp -v docs/1-executive-overview/README.md EXECUTIVE-OVERVIEW.md
fi

# Architecture content
if [ -d "docs/2-architecture" ]; then
  cp -rv docs/2-architecture/* docs/1-architecture/
fi

# Implementation guide content
if [ -d "docs/3-implementation-guide" ]; then
  cp -rv docs/3-implementation-guide/* docs/2-implementation-guide/
fi

# Developer guide content
if [ -d "docs/4-developer-guide" ]; then
  cp -rv docs/4-developer-guide/* docs/3-developer-guide/
fi

# Operations guide content
if [ -d "docs/5-operations-guide" ]; then
  cp -rv docs/5-operations-guide/* docs/4-operations-guide/
fi

# Reference content
if [ -d "docs/6-reference" ]; then
  cp -rv docs/6-reference/* docs/5-reference/
fi

# Appendices content
if [ -d "docs/7-appendices" ]; then
  cp -rv docs/7-appendices/* docs/6-appendices/
fi

# Create standard documentation files
echo "Creating standard documentation files..."

# Create README.md with template structure
cat > README.md << 'EOL'
# [Product]-[Platform]-Playbook

## Overview

### Purpose

This template provides a standardized, comprehensive approach to creating technical documentation playbooks across various domains and technologies.

### Key Features

- Structured repository layout
- Consistent documentation standards
- Enhanced navigation and accessibility
- Version control best practices
- Cross-platform compatibility

## Getting Started

- [Executive Overview](./EXECUTIVE-OVERVIEW.md) - High-level executive summary
- [Architecture](./docs/1-architecture/README.md) - System architecture details
- [Implementation Guide](./docs/2-implementation-guide/README.md) - Implementation instructions
- [Developer Guide](./docs/3-developer-guide/README.md) - Developer-specific documentation
- [Operations Guide](./docs/4-operations-guide/README.md) - Operational procedures
- [Reference](./docs/5-reference/README.md) - Reference materials
- [Appendices](./docs/6-appendices/README.md) - Supplementary documents

## Examples and Templates

- [Code and Configuration Examples](./examples/README.md)
- [Template Documents](./templates/README.md)

## Contributing

See [CONTRIBUTING.md](./CONTRIBUTING.md) for contribution guidelines.

## License

This project is licensed under the [LICENSE](./LICENSE) specified.

## Security

For security concerns, please see [SECURITY.md](./SECURITY.md).

---

**Document Metadata:**
- Last Updated: $(date +%Y-%m-%d)
- Version: 1.0.0
- Status: Template
EOL

# Create EXECUTIVE-OVERVIEW.md
cat > EXECUTIVE-OVERVIEW.md << 'EOL'
# Executive Overview

## Purpose

This document provides a high-level executive summary of the [Product]-[Platform] integration. It is designed to give business stakeholders and decision-makers an overview of the solution, its benefits, and strategic value.

## Business Value

[Describe the business value of the solution here]

## Key Features

- [Feature 1]
- [Feature 2]
- [Feature 3]
- [Feature 4]

## Strategic Benefits

- [Benefit 1]
- [Benefit 2]
- [Benefit 3]
- [Benefit 4]

## Implementation Overview

[Provide a brief overview of implementation approach and timeline]

## Resource Requirements

[List key resource requirements]

## Return on Investment

[Summarize ROI metrics or KPIs]

---

**Document Metadata:**
- Last Updated: $(date +%Y-%m-%d)
- Version: 1.0.0
- Status: Template
EOL

# Create README files for documentation directories
for dir in docs/1-architecture docs/2-implementation-guide docs/3-developer-guide docs/4-operations-guide docs/5-reference docs/6-appendices examples templates; do
  mkdir -p $dir
  cat > $dir/README.md << EOL
# $(basename $dir | sed 's/^[0-9]-//g' | sed 's/-/ /g' | sed 's/\b\(.\)/\u\1/g')

## Overview

This section contains documentation related to $(basename $dir | sed 's/^[0-9]-//g' | sed 's/-/ /g' | sed 's/\b\(.\)/\u\1/g').

## Contents

[List of documents in this section]

---

**Document Metadata:**
- Last Updated: $(date +%Y-%m-%d)
- Version: 1.0.0
- Status: Template
EOL
done

# Create directory for documentation images
mkdir -p assets/images

echo "Implementation of Comprehensive Technical Playbook Template completed!"
echo "Please check and customize the generated files according to your needs."
echo "To link to these new documents, use the following format:"
echo "  [Document Title](./path/to/document.md)"
echo ""
echo "For example:"
echo "  [Architecture Overview](./docs/1-architecture/overview.md)"
EOL 