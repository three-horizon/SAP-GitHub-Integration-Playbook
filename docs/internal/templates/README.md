# Documentation Templates

This directory contains templates for creating consistent documentation across the SAP-GitHub Integration Playbook.

## Available Templates

| Template | Purpose | Description |
|----------|---------|-------------|
| [Document Template](document-template.md) | General documentation | Basic template for standard documentation pages |
| [Diagram Document Template](diagram-document-template.md) | Documents with diagrams | Template optimized for documents containing multiple SVG diagrams |
| [Tutorial Template](tutorial-template.md) | Step-by-step guides | Template for creating procedural tutorials |
| [API Documentation Template](api-doc-template.md) | API reference docs | Template for documenting APIs and interfaces |
| [Best Practice Template](best-practice-template.md) | Best practices | Template for documenting recommended practices |
| [Update Template](update-template.md) | Content updates | Template for making structured updates to playbook content |

## Using Templates

1. Copy the appropriate template to your destination folder
2. Rename the file according to the content's purpose
3. Replace placeholder text and images with your content
4. Ensure all diagrams follow the [SVG Implementation Guide](../resources/svg-implementation-guide.md)
5. Submit for review

## Diagram Standards

For documents that include diagrams:

1. Use SVG format for all diagrams
2. Follow color and style guidelines in [VISUAL_ASSETS.md](../../assets/images/VISUAL_ASSETS.md)
3. Always include descriptive alt text for accessibility
4. Use the SVG validation tools:
   ```bash
   npm run validate-svg
   ```

## Creating New Templates

When creating new templates:

1. Follow existing formatting conventions
2. Include clear instructions for placeholders
3. Ensure accessibility standards are met
4. Add the template to this README
5. Submit for review via pull request 