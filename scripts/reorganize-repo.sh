#!/bin/bash

# Script para reorganizar o repositório conforme a nova estrutura

echo "Criando nova estrutura de diretórios..."

# Criar nova estrutura
mkdir -p docs/getting-started
mkdir -p docs/documentation/architecture
mkdir -p docs/documentation/guides
mkdir -p docs/documentation/developer
mkdir -p docs/documentation/implementation
mkdir -p docs/documentation/operations
mkdir -p docs/documentation/reference
mkdir -p docs/documentation/appendices
mkdir -p docs/examples/dashboards
mkdir -p docs/examples/github-actions
mkdir -p docs/examples/kql-queries
mkdir -p docs/examples/logic-apps
mkdir -p docs/resources

# Mover arquivos existentes para nova estrutura
echo "Movendo arquivos para nova estrutura..."

# Getting Started
cp docs/2-implementation-guide/getting-started/* docs/getting-started/ 2>/dev/null

# Architecture
cp -r docs/1-architecture/* docs/documentation/architecture/ 2>/dev/null

# Guides
cp -r docs/2-implementation-guide/* docs/documentation/guides/ 2>/dev/null
rm -rf docs/documentation/guides/getting-started 2>/dev/null

# Developer
cp -r docs/3-developer-guide/* docs/documentation/developer/ 2>/dev/null

# Implementation
mkdir -p docs/documentation/implementation
# No specific files to move here yet

# Operations
cp -r docs/4-operations-guide/* docs/documentation/operations/ 2>/dev/null

# Reference
cp -r docs/5-reference/* docs/documentation/reference/ 2>/dev/null

# Appendices
cp -r docs/6-appendices/* docs/documentation/appendices/ 2>/dev/null

# Examples
cp -r examples/* docs/examples/ 2>/dev/null

# Resources
cp docs/README.md docs/resources/documentation-overview.md 2>/dev/null
cp docs/executive-overview.md docs/resources/executive-overview.md 2>/dev/null

# Criar arquivos de guia de estilo
echo "Criando guias de recursos..."

cat > docs/resources/markdown-style-guide.md << 'EOL'
# Markdown Style Guide

This guide outlines the markdown formatting standards used in this repository.

## Headers

- Use ATX-style headers with a space after the hash: `# Header`
- Use Title Case for headers
- Maximum of 3 levels in a single document

## Lists

- Use hyphens `-` for unordered lists
- Use `1.` for ordered lists (the actual numbers don't matter)
- Indent with 2 spaces for nested lists

## Code Blocks

- Use triple backticks with language identifier: ```javascript
- Use inline code with single backticks for code references

## Tables

- Use tables sparingly, only when data is tabular
- Include a header row with at least 3 dashes per column
- Align columns for better readability

## Images

- Always include alt text: `![Alt text](image.png)`
- For SVG images, use the full GitHub URL with `?sanitize=true` parameter
- Include captions below images with italics

## Links

- Use descriptive link text: [Useful Article](link) not [click here](link)
- For internal documentation links, use relative paths

## Emphasis

- Use **bold** for emphasis
- Use *italics* for definitions or slight emphasis
- Use ~~strikethrough~~ sparingly

## Quotations

- Use `>` for quotations
- For nested quotes, add additional `>`

## Additional Formatting

- Use horizontal rules (`---`) to separate major sections
- Add a blank line before and after each block element
EOL

cat > docs/resources/multilingual-documentation.md << 'EOL'
# Guide for Multilingual Documentation

This guide provides standards for translating and maintaining multilingual documentation for the SAP-GitHub Integration Playbook.

## Supported Languages

Currently, we support documentation in:
- English (primary)
- German
- Portuguese
- Spanish 
- French

## File Structure

For multilingual documentation:

1. **English Content**: stored in the main documentation path
2. **Translations**: stored in language-specific subfolders

Example:
```
documentation/
├── guide.md                 # English version
├── de/                      # German translations
│   └── guide.md
├── pt/                      # Portuguese translations
│   └── guide.md
```

## Translation Process

1. **Create English content first** - This serves as the source of truth
2. **Keep structure consistent** - Maintain the same structure, headings, and examples
3. **Use glossary** - Refer to the technical glossary for standard translations
4. **Preserve code examples** - Don't translate code (unless in comments)
5. **Review process** - Each translation must be reviewed by at least one native speaker

## Language Switching

Include a language selector at the top of each document:

```markdown
> Also available in: [Deutsch](de/guide.md) | [Português](pt/guide.md) | [Español](es/guide.md) | [Français](fr/guide.md)
```

## Translation Tips

- Focus on technical accuracy over literal translation
- Preserve technical terms in English when standard in the industry
- Include regional variations in parentheses when relevant
- Use tools like terminology databases and translation memory
EOL

cat > docs/resources/using-this-template.md << 'EOL'
# Template Usage Guide

This document explains how to use the SAP-GitHub Integration Playbook as a template for your own documentation.

## Getting Started

To use this template for your own SAP-GitHub integration documentation:

1. **Fork the repository** or download it as a template
2. **Customize the configuration**:
   - Update the repository name and organization
   - Modify the documentation structure if needed
   - Adjust branding elements
3. **Replace content** with your specific implementation details

## Structure Overview

The template follows a logical structure:

- **Getting Started**: Onboarding and initial setup information
- **Documentation**: Detailed technical documentation
  - Architecture: System design and components
  - Guides: Implementation instructions
  - Developer: Developer-focused documentation
  - Implementation: Implementation-specific guidance
  - Operations: Maintenance and monitoring
  - Reference: Technical references
  - Appendices: Supplementary information
- **Examples**: Working code and configuration examples
- **Resources**: Style guides and template information

## Customizing the Template

### Basic Customization

1. **Update README.md** with your project details
2. **Modify documentation/architecture** with your specific architecture
3. **Update all references** to the original SAP-GitHub Integration Playbook

### Advanced Customization

1. **Add or remove sections** based on your needs
2. **Customize the examples** to match your specific SAP landscape
3. **Update image assets** with your own diagrams
4. **Integrate with your tools** (CI/CD, documentation generator, etc.)

## Best Practices

- **Keep documentation close to code** - Follow documentation-as-code principles
- **Use consistent formatting** - Follow the markdown style guide
- **Update regularly** - Documentation should evolve with your implementation
- **Include visuals** - Diagrams help explain complex concepts
- **Obtain feedback** - Regular reviews improve documentation quality

## Support and Maintenance

For your own implementation based on this template:

1. Assign documentation owners for each section
2. Create a regular review schedule
3. Establish a feedback mechanism
4. Track documentation issues using GitHub Issues
EOL

echo "Reorganização concluída!" 