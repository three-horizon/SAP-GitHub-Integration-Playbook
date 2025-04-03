#!/bin/bash

# Script to migrate existing documentation to the new structure
# Usage: ./migrate-documentation.sh

# Create required directories
echo "Creating directory structure..."
mkdir -p docs/1-executive-overview
mkdir -p docs/2-architecture
mkdir -p docs/3-implementation-guide
mkdir -p docs/4-developer-guide
mkdir -p docs/5-operations-guide
mkdir -p docs/6-reference
mkdir -p docs/7-appendices
mkdir -p docs/templates
mkdir -p assets/images/executive
mkdir -p assets/images/implementation

# Migrate existing files with proper mapping

# Implementation Guide files
echo "Migrating implementation guide files..."
cp -v docs/getting-started/prerequisites.md docs/3-implementation-guide/prerequisites.md
cp -v docs/getting-started/implementation-plan.md docs/3-implementation-guide/implementation-plan.md
cp -v docs/implementation/abapgit-setup.md docs/3-implementation-guide/abapgit-setup.md
cp -v docs/github/actions-workflow-setup.md docs/3-implementation-guide/github-actions-setup.md
cp -v docs/api-management/configuration.md docs/3-implementation-guide/api-management-configuration.md

# Create placeholder for files that need to be moved
echo "Creating placeholder files for sections that need content..."

# Executive Overview placeholders
touch docs/1-executive-overview/business-value.md
touch docs/1-executive-overview/transformation-journey.md
touch docs/1-executive-overview/solution-capabilities.md
touch docs/1-executive-overview/executive-dashboard.md

# Architecture placeholders
touch docs/2-architecture/component-architecture.md
touch docs/2-architecture/integration-flows.md
touch docs/2-architecture/security-architecture.md
touch docs/2-architecture/model-context-protocol.md
touch docs/2-architecture/deployment-architecture.md

# Additional Implementation Guide placeholders
touch docs/3-implementation-guide/security-implementation.md
touch docs/3-implementation-guide/testing-validation.md
touch docs/3-implementation-guide/pilot-deployment.md
touch docs/3-implementation-guide/full-rollout.md
touch docs/3-implementation-guide/github-repository-setup.md
touch docs/3-implementation-guide/implementation-checklists.md

# Developer Guide placeholders
touch docs/4-developer-guide/README.md
touch docs/4-developer-guide/development-workflows.md
touch docs/4-developer-guide/abap-patterns.md
touch docs/4-developer-guide/context-aware-development.md
touch docs/4-developer-guide/github-copilot-abap.md
touch docs/4-developer-guide/code-examples.md

# Operations Guide placeholders
touch docs/5-operations-guide/README.md
touch docs/5-operations-guide/monitoring-logging.md
touch docs/5-operations-guide/security-monitoring.md
touch docs/5-operations-guide/troubleshooting.md
touch docs/5-operations-guide/performance-optimization.md
touch docs/5-operations-guide/maintenance-procedures.md

# Reference placeholders
touch docs/6-reference/README.md
touch docs/6-reference/api-reference.md
touch docs/6-reference/configuration-examples.md
touch docs/6-reference/sap-notes.md
touch docs/6-reference/github-actions-reference.md
touch docs/6-reference/security-controls-reference.md

# Appendices placeholders
touch docs/7-appendices/README.md
touch docs/7-appendices/templates.md
touch docs/7-appendices/scripts-utilities.md
touch docs/7-appendices/case-studies.md
touch docs/7-appendices/glossary.md

echo "Migration complete. Please review the new structure and update content as needed."
echo "Note: This script has created placeholder files where content needs to be developed."
echo "Next steps:"
echo "1. Review migrated files for consistency"
echo "2. Update links and references between documents"
echo "3. Apply the new document template to all files"
echo "4. Update placeholder files with content" 