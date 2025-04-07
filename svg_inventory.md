# SVG Image Inventory

This document lists all SVG files available in the repository for use in documentation.

Generated: Mon Apr  7 14:05:09 -03 2025

## Available SVG Files

| Category | SVG File | Path |
|----------|----------|------|
| architecture | abapgit-workflow.svg | assets/images/architecture/abapgit-workflow.svg |
| architecture | api-implementation.svg | assets/images/architecture/api-implementation.svg |
| architecture | high-level-architecture.svg | assets/images/architecture/high-level-architecture.svg |
| architecture | playbook-overview.svg | assets/images/architecture/playbook-overview.svg |
| architecture | sap-abap-github-integration.svg | assets/images/architecture/sap-abap-github-integration.svg |
| architecture | sap-fiori-github-integration.svg | assets/images/architecture/sap-fiori-github-integration.svg |
| architecture | sap-hana-github-integration.svg | assets/images/architecture/sap-hana-github-integration.svg |
| components | sap-components.svg | assets/images/components/sap-components.svg |
| devsecops | devsecops-cycle.svg | assets/images/devsecops/devsecops-cycle.svg |
| devsecops | devsecops-detailed.svg | assets/images/devsecops/devsecops-detailed.svg |
| devsecops | security-components.svg | assets/images/devsecops/security-components.svg |
| flows | development-workflow.svg | assets/images/flows/development-workflow.svg |
| flows | github-to-sap-flow.svg | assets/images/flows/github-to-sap-flow.svg |
| flows | integration-decision-tree.svg | assets/images/flows/integration-decision-tree.svg |
| flows | monitoring-dashboard-template.svg | assets/images/flows/monitoring-dashboard-template.svg |
| flows | sap-github-workflow.svg | assets/images/flows/sap-github-workflow.svg |
| flows | sap-to-github-flow.svg | assets/images/flows/sap-to-github-flow.svg |
| flows | security-monitoring-flow.svg | assets/images/flows/security-monitoring-flow.svg |
| flows | security-workflow.svg | assets/images/flows/security-workflow.svg |
| flows | transport-decision-tree.svg | assets/images/flows/transport-decision-tree.svg |
| icons | sap-github-icon.svg | assets/images/icons/sap-github-icon.svg |
| resources | color-palette.svg | assets/images/resources/color-palette.svg |

## Usage in Markdown

To use these SVG files in your Markdown documentation, use the following HTML format:

```html
<div align="center" class="svg-container">
  <!-- Using both object and img as fallback for maximum compatibility -->
  <object type="image/svg+xml" data="\1" style="width: 700px; max-width: 100%;" aria-label="\1">
    <img src="\1" alt="\1" width="700" />
  </object>
  
  *\1*
</div>
```

Replace:
- `category` with the appropriate category folder name
- `filename.svg` with the SVG file name
- `Description` with a descriptive alt text
- `Caption for the image` with an appropriate caption

## Path Adjustments

Adjust the path as needed for your document's location:

- For root-level documents: `./assets/images/...`
- For docs/ directory: `../assets/images/...`
- For docs/section/ directory: `../../assets/images/...`
