#!/bin/bash

# SAP-GitHub Integration Playbook Reorganization Script
# This script reorganizes the repository structure according to the comprehensive technical playbook template

echo "Starting repository reorganization..."

# Create main directory structure if it doesn't exist
mkdir -p docs/1-architecture
mkdir -p docs/2-implementation-guide
mkdir -p docs/3-developer-guide
mkdir -p docs/4-operations-guide
mkdir -p docs/5-reference
mkdir -p docs/6-appendices
mkdir -p examples
mkdir -p scripts
mkdir -p assets/images

# Ensure root files are in place
touch README.md
touch EXECUTIVE-OVERVIEW.md
touch CONTRIBUTING.md
touch LICENSE
touch SECURITY.md

# Move any misplaced architecture documentation to the correct location
if [ -d "architecture" ]; then
  echo "Moving architecture content to docs/1-architecture..."
  cp -R architecture/* docs/1-architecture/
  rm -rf architecture
fi

if [ -d "docs/architecture" ]; then
  echo "Moving docs/architecture content to docs/1-architecture..."
  cp -R docs/architecture/* docs/1-architecture/
  rm -rf docs/architecture
fi

# Move implementation documentation
if [ -d "implementation" ]; then
  echo "Moving implementation content to docs/2-implementation-guide..."
  cp -R implementation/* docs/2-implementation-guide/
  rm -rf implementation
fi

if [ -d "docs/implementation" ]; then
  echo "Moving docs/implementation content to docs/2-implementation-guide..."
  cp -R docs/implementation/* docs/2-implementation-guide/
  rm -rf docs/implementation
fi

# Move developer documentation
if [ -d "docs/development" ]; then
  echo "Moving docs/development content to docs/3-developer-guide..."
  cp -R docs/development/* docs/3-developer-guide/
  rm -rf docs/development
fi

# Move operations documentation
if [ -d "docs/operations" ]; then
  echo "Moving docs/operations content to docs/4-operations-guide..."
  cp -R docs/operations/* docs/4-operations-guide/
  rm -rf docs/operations
fi

if [ -d "docs/maintenance" ]; then
  echo "Moving docs/maintenance content to docs/4-operations-guide/maintenance..."
  mkdir -p docs/4-operations-guide/maintenance
  cp -R docs/maintenance/* docs/4-operations-guide/maintenance/
  rm -rf docs/maintenance
fi

# Move reference documentation
if [ -d "reference" ]; then
  echo "Moving reference content to docs/5-reference..."
  cp -R reference/* docs/5-reference/
  rm -rf reference
fi

if [ -d "docs/reference" ]; then
  echo "Moving docs/reference content to docs/5-reference..."
  cp -R docs/reference/* docs/5-reference/
  rm -rf docs/reference
fi

if [ -d "docs/best-practices" ]; then
  echo "Moving docs/best-practices content to docs/5-reference/best-practices..."
  mkdir -p docs/5-reference/best-practices
  cp -R docs/best-practices/* docs/5-reference/best-practices/
  rm -rf docs/best-practices
fi

# Move appendix content
if [ -d "docs/faq" ]; then
  echo "Moving docs/faq content to docs/6-appendices/faq..."
  mkdir -p docs/6-appendices/faq
  cp -R docs/faq/* docs/6-appendices/faq/
  rm -rf docs/faq
fi

if [ -d "docs/troubleshooting" ]; then
  echo "Moving docs/troubleshooting content to docs/6-appendices/troubleshooting..."
  mkdir -p docs/6-appendices/troubleshooting
  cp -R docs/troubleshooting/* docs/6-appendices/troubleshooting/
  rm -rf docs/troubleshooting
fi

if [ -d "docs/support" ]; then
  echo "Moving docs/support content to docs/6-appendices/support..."
  mkdir -p docs/6-appendices/support
  cp -R docs/support/* docs/6-appendices/support/
  rm -rf docs/support
fi

# Move individual FAQ files if they exist
if [ -f "docs/faq.md" ]; then
  echo "Moving docs/faq.md to docs/6-appendices/faq.md..."
  mkdir -p docs/6-appendices
  mv docs/faq.md docs/6-appendices/
fi

# Move documentation standards
if [ -f "documentation-style-guide.md" ]; then
  echo "Moving documentation-style-guide.md to docs/documentation-standards.md..."
  mv documentation-style-guide.md docs/documentation-standards.md
fi

if [ -f "docs/documentation-style-guide.md" ]; then
  echo "Moving docs/documentation-style-guide.md to docs/documentation-standards.md..."
  mv docs/documentation-style-guide.md docs/documentation-standards.md
fi

# Clean up any remaining non-standard directories from root that should be in docs
for dir in path best-practices templates; do
  if [ -d "$dir" ]; then
    echo "Moving $dir content to docs/$dir..."
    mkdir -p docs/$dir
    cp -R $dir/* docs/$dir/
    rm -rf $dir
  fi
done

# Clean up any directories not in the template structure
for dir in $(find docs -maxdepth 1 -type d | grep -v "docs/1-architecture\|docs/2-implementation-guide\|docs/3-developer-guide\|docs/4-operations-guide\|docs/5-reference\|docs/6-appendices\|docs/templates" | grep -v "^docs$"); do
  echo "Determining appropriate location for $dir..."
  
  # Analyze directory name to place it in the appropriate section
  dirname=$(basename "$dir")
  
  if [[ "$dirname" == *"api"* || "$dirname" == *"github"* ]]; then
    echo "Moving $dir to docs/3-developer-guide/$dirname..."
    mkdir -p "docs/3-developer-guide/$dirname"
    cp -R "$dir"/* "docs/3-developer-guide/$dirname"/
    rm -rf "$dir"
  elif [[ "$dirname" == *"getting-started"* || "$dirname" == *"tutorial"* || "$dirname" == *"training"* ]]; then
    echo "Moving $dir to docs/2-implementation-guide/$dirname..."
    mkdir -p "docs/2-implementation-guide/$dirname"
    cp -R "$dir"/* "docs/2-implementation-guide/$dirname"/
    rm -rf "$dir"
  elif [[ "$dirname" == *"testing"* || "$dirname" == *"security"* ]]; then
    echo "Moving $dir to docs/4-operations-guide/$dirname..."
    mkdir -p "docs/4-operations-guide/$dirname"
    cp -R "$dir"/* "docs/4-operations-guide/$dirname"/
    rm -rf "$dir"
  elif [[ "$dirname" == *"governance"* || "$dirname" == *"benefits"* ]]; then
    echo "Moving $dir to docs/5-reference/$dirname..."
    mkdir -p "docs/5-reference/$dirname"
    cp -R "$dir"/* "docs/5-reference/$dirname"/
    rm -rf "$dir"
  else
    echo "Moving $dir to docs/6-appendices/$dirname..."
    mkdir -p "docs/6-appendices/$dirname"
    cp -R "$dir"/* "docs/6-appendices/$dirname"/
    rm -rf "$dir"
  fi
done

# Create or update README files in each directory to explain its purpose
cat > docs/1-architecture/README.md << 'EOF'
# Architecture Documentation

This section contains architectural details of the SAP-GitHub integration, including:

- System architecture overview
- Component diagrams
- Data flow diagrams
- Integration patterns
- Design decisions and constraints

## Contents

- [System Overview](./system-overview.md)
- [Component Architecture](./component-architecture.md)
- [Integration Patterns](./integration-patterns.md)
- [Security Architecture](./security-architecture.md)
EOF

cat > docs/2-implementation-guide/README.md << 'EOF'
# Implementation Guide

This section provides detailed instructions for implementing the SAP-GitHub integration, including:

- Setup procedures
- Configuration steps
- Installation guides
- Migration processes
- Environment preparation

## Contents

- [Prerequisites](./prerequisites.md)
- [Installation](./installation.md)
- [Configuration](./configuration.md)
- [Migration Guide](./migration-guide.md)
- [Verification Steps](./verification.md)
EOF

cat > docs/3-developer-guide/README.md << 'EOF'
# Developer Guide

This section contains developer-specific documentation for the SAP-GitHub integration, including:

- API references
- SDK usage
- Development workflows
- Coding standards
- Contribution guidelines

## Contents

- [Development Environment Setup](./dev-environment-setup.md)
- [API Documentation](./api-documentation.md)
- [GitHub Workflows](./github-workflows.md)
- [Testing Guidelines](./testing-guidelines.md)
- [Contributing Code](./contributing-code.md)
EOF

cat > docs/4-operations-guide/README.md << 'EOF'
# Operations Guide

This section provides documentation for operating and maintaining the SAP-GitHub integration, including:

- Monitoring procedures
- Backup and recovery
- Performance tuning
- Security operations
- Routine maintenance

## Contents

- [Monitoring](./monitoring.md)
- [Backup & Recovery](./backup-recovery.md)
- [Security Operations](./security-operations.md)
- [Performance Optimization](./performance-optimization.md)
- [Maintenance Procedures](./maintenance-procedures.md)
EOF

cat > docs/5-reference/README.md << 'EOF'
# Reference

This section contains reference materials for the SAP-GitHub integration, including:

- Technical specifications
- Standards compliance
- Best practices
- Glossary of terms
- External resources

## Contents

- [Technical Specifications](./technical-specifications.md)
- [Best Practices](./best-practices.md)
- [Standards Compliance](./standards-compliance.md)
- [Glossary](./glossary.md)
- [External Resources](./external-resources.md)
EOF

cat > docs/6-appendices/README.md << 'EOF'
# Appendices

This section contains supplementary materials for the SAP-GitHub integration, including:

- FAQs
- Troubleshooting guides
- Support resources
- Change logs
- Legacy systems documentation

## Contents

- [Frequently Asked Questions](./faq.md)
- [Troubleshooting Guide](./troubleshooting.md)
- [Support Resources](./support-resources.md)
- [Change Log](./changelog.md)
- [Legacy Systems](./legacy-systems.md)
EOF

echo "Repository reorganization completed successfully!" 