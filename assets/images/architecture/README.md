# Architecture Diagrams

This directory contains the architecture diagrams used throughout the SAP-GitHub Integration Playbook. These diagrams illustrate key architectural concepts, integration patterns, and implementation approaches.

## Included Diagrams

### Reference Architecture
- `high-level-architecture.svg` - Simplified view of the integration architecture
- `sap-abap-github-integration.svg` - SAP ABAP integration architecture
- `sap-fiori-github-integration.svg` - SAP Fiori/UI5 integration architecture
- `sap-hana-github-integration.svg` - SAP HANA integration architecture
- `metrics-framework.svg` - Metrics framework architecture
- `playbook-overview.svg` - Playbook overview diagram
- `btp-architecture.svg` - SAP BTP integration architecture

### Integration Patterns
- `api-interaction-patterns.svg` - API interaction patterns
- `api-extension-framework.svg` - Extension framework architecture
- `abapgit-workflow.svg` - abapGit workflow diagram
- `charm-integration-architecture.svg` - CHARM integration architecture
- `context-store.svg` - Context store architecture

### Process Flows
- `github-to-sap-flow.svg` - GitHub to SAP flow diagram
- `sap-to-github-flow.svg` - SAP to GitHub flow diagram
- `sap-github-workflow.svg` - SAP-GitHub workflow diagram
- `workflow-fundamentals.svg` - Workflow fundamentals diagram
- `cicd-pipeline.svg` - CI/CD pipeline architecture
- `development-workflow.svg` - Development workflow diagram
- `transport-decision-tree.svg` - Transport decision tree
- `integration-decision-tree.svg` - Integration decision tree
- `security-workflow.svg` - Security workflow diagram
- `security-monitoring-flow.svg` - Security monitoring flow diagram
- `monitoring-dashboard-template.svg` - Monitoring dashboard template
- `transport-security-dashboard.svg` - Transport security dashboard
- `governance-model.svg` - Governance model architecture
- `disaster-recovery-architecture.svg` - Disaster recovery architecture

## Design Guidelines

All architecture diagrams follow these design standards:

1. **Consistent Visual Language**
   - Blue components for SAP systems (gradient: #1565C0 to #0D47A1)
   - Purple components for GitHub (gradient: #6F42C1 to #5E35B1)
   - Green components for integration services (gradient: #388E3C to #2E7D32)
   - Orange components for workflows (gradient: #F57F17 to #E65100)
   - Teal components for deployment (gradient: #0097A7 to #00838F)
   - Red components for security services (gradient: #F44336 to #D32F2F)

2. **Flow Direction**
   - Left-to-right for process flows
   - Top-to-bottom for hierarchical relationships
   - Clear directional arrows for all interactions

3. **Component Size**
   - Proportional to significance in the architecture
   - Consistent within diagram sets
   - Sized for readability at standard display resolutions

4. **Labels and Annotations**
   - Concise, clear labels
   - Additional context provided where needed
   - Consistent font and text styling

5. **Accessibility**
   - SVG with proper title and description elements
   - Semantic grouping of elements
   - Text contrast ratio of at least 4.5:1
   - Use of accessible color combinations

## SVG Format Standards

All diagrams should be created using SVG format with the following specifications:

1. **SVG Structure**
   - Include `role="img"` attribute
   - Add `aria-labelledby="title desc"` attribute
   - Include `<title id="title">` element with descriptive title
   - Include `<desc id="desc">` element with detailed description

2. **SVG Design Elements**
   - Use gradient definitions in `<defs>` section
   - Use semantic grouping with `<g>` elements
   - Include proper filter effects for shadows
   - Define arrow markers for connection lines

3. **Example SVG Header**
   ```xml
   <svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" width="800" height="600" viewBox="0 0 800 600" role="img" aria-labelledby="title desc">
     <title id="title">Title of the Diagram</title>
     <desc id="desc">Detailed description of what the diagram represents</desc>
     <!-- Content follows -->
   </svg>
   ```

4. **Referencing SVGs in Markdown**

   When referencing SVG files in Markdown documentation, use the following format:
   
   ```markdown
   <div align="center">

   <img src="/assets/images/architecture/diagram-name.svg" alt="Descriptive Alt Text" width="800">

   *Descriptive Caption for the Diagram*
   </div>
   ```

   This format ensures proper rendering and accessibility of SVG diagrams across different platforms.

## Updating Architecture Diagrams

When updating or adding new diagrams:

1. Follow the established color scheme and visual language
2. Use SVG format for all diagrams
3. Maintain consistent naming conventions
4. Update this README.md when adding new diagrams
5. Update the status in diagram-placeholders.md

## Tools

Recommended tools for creating and editing architecture diagrams:

- [draw.io](https://draw.io) / [diagrams.net](https://diagrams.net) (Free, open-source)
- [Lucidchart](https://lucidchart.com) (Commercial)
- [Microsoft Visio](https://products.office.com/visio) (Commercial)
- [Inkscape](https://inkscape.org) (Free, open-source SVG editor)
- [SVG-Edit](https://github.com/SVG-Edit/svgedit) (Free, web-based)

## Source Files

Source files for diagrams (draw.io, Visio, etc.) should be stored in the `source` subdirectory to enable future editing.

---