#!/bin/bash

# Add the modified files
git add docs/2-implementation-guide/tutorials/advanced/multi-system.md 
git add docs/2-implementation-guide/github-setup/advanced-security.md 
git add docs/2-implementation-guide/security-setup/cicd-security.md

# Commit the changes with a descriptive message
git commit -m "Fix Jekyll Liquid syntax errors by escaping template variables"

# Push the changes
git push

echo "Changes committed and pushed" 