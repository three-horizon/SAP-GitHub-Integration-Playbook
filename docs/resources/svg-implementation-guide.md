# SVG Implementation Guide

<div align="center">
<img src="https://github.com/three-horizon/SAP-GitHub-Integration-Playbook/raw/main/assets/images/architecture/playbook-overview.svg?sanitize=true" width="700" alt="SAP-GitHub Integration Playbook Overview">
</div>

## Overview

This guide explains how to create, implement, and maintain SVG (Scalable Vector Graphics) assets throughout the SAP-GitHub Integration Playbook documentation. SVG is the preferred format for all diagrams, flowcharts, architecture illustrations, and other visual assets in this documentation.

## Why SVG?

- **Scalable**: SVGs maintain quality at any size
- **Accessible**: Text elements can be read by screen readers
- **Interactive**: Can be animated and made interactive
- **Searchable**: Text within SVGs is searchable
- **Lightweight**: Often smaller file size than raster images
- **Editable**: Easy to update and maintain

## Creating SVG Diagrams

### Recommended Tools

- **[draw.io](https://draw.io)** (free, web-based and desktop versions)
- **[Lucidchart](https://lucidchart.com)** (subscription-based)
- **[Inkscape](https://inkscape.org)** (free, open-source)
- **[Figma](https://figma.com)** (design tool with SVG export)

### Design Standards

Follow the design standards specified in [VISUAL_ASSETS.md](../../assets/images/VISUAL_ASSETS.md), which include:

- **Color Palette**:
  - SAP Components: SAP Green (#0BAF51)
  - GitHub Components: GitHub Purple (#6E5494)
  - Microsoft/Azure Components: Microsoft Blue (#0078D4)
  - Security Elements: Security Red (#D13438)
  - Integration Components: Neutral Gray (#6B6B6B)
  - Developer/User Components: Orange (#FF8C00)

- **Typography**:
  - Primary Font: Segoe UI or Arial (sans-serif)
  - Clear hierarchy of text sizes

- **Layouts**:
  - Clear directional flow
  - Consistent spacing
  - Properly aligned connections

### SVG Export Best Practices

1. **Include a viewBox attribute**: Essential for proper scaling
2. **Set width and height**: Provides proper dimensions
3. **Define proper aspect ratio**: Prevents distortion
4. **Use descriptive IDs and classes**: Improves accessibility and CSS styling
5. **Minimize file size**: Remove unnecessary elements and metadata
6. **Use proper namespaces**: Include the SVG namespace

## File Organization

SVGs should be organized in the repository following this structure:

```
assets/
  images/
    architecture/       # High-level architecture diagrams
    components/         # Component-level diagrams
    flows/              # Workflow and data flow diagrams
    deployment/         # Deployment diagrams
    devsecops/          # DevSecOps and security diagrams
    resources/          # Resources and templates
    icons/              # Reusable icons and symbols
```

## File Naming Conventions

Use kebab-case (lowercase with hyphens) for all SVG filenames, following this pattern:

1. **Domain-specific prefix** (optional)
2. **Descriptive name** (required)
3. **Version or variant** (optional)

Examples:
- `sap-abap-github-integration.svg`
- `development-workflow.svg`
- `security-monitoring-flow.svg`

## Adding SVGs to Documentation

### Method 1: Direct Image Link (Preferred for Simple Usage)

```markdown
<img src="https://github.com/three-horizon/SAP-GitHub-Integration-Playbook/raw/main/assets/images/architecture/playbook-overview.svg?sanitize=true" width="700" alt="SAP-GitHub Integration Playbook Overview">
```

The `?sanitize=true` parameter is crucial for SVGs as it ensures GitHub renders the SVG safely.

### Method 2: Object/Image Fallback (Better for Complex SVGs)

```markdown
<div align="center" class="svg-container">
<object type="image/svg+xml" data="https://github.com/three-horizon/SAP-GitHub-Integration-Playbook/raw/main/assets/images/architecture/playbook-overview.svg" style="width: 700px; max-width: 100%;" aria-label="Playbook Overview">
<img src="https://github.com/three-horizon/SAP-GitHub-Integration-Playbook/raw/main/assets/images/architecture/playbook-overview.svg?sanitize=true" width="700" alt="Playbook Overview">
</object>
</div>
```

This method provides a fallback for browsers that don't support SVG rendering via the object tag.

### Method 3: SVG with Caption

```markdown
<div align="center" class="svg-container">
<object type="image/svg+xml" data="https://github.com/three-horizon/SAP-GitHub-Integration-Playbook/raw/main/assets/images/architecture/playbook-overview.svg" style="width: 700px; max-width: 100%;" aria-label="Playbook Overview">
<img src="https://github.com/three-horizon/SAP-GitHub-Integration-Playbook/raw/main/assets/images/architecture/playbook-overview.svg?sanitize=true" width="700" alt="Playbook Overview">
</object>
<p><em>Figure 1: SAP-GitHub Integration Architecture Overview</em></p>
</div>
```

## SVG Validation

This repository includes SVG validation tools to ensure all SVGs meet our standards:

```bash
# Install dependencies
npm install

# Validate SVGs
npm run validate-svg
```

The validator checks for:
- Proper viewBox attributes
- Width and height attributes
- Correct SVG namespace
- Other quality checks

## Converting SVG to PNG (for Compatibility)

While SVG is preferred, sometimes PNG versions are needed for compatibility or presentations:

```bash
# Convert SVGs to PNGs
npm run convert-svg
```

This creates PNG versions in the `assets/images/png/` directory.

## Accessibility Considerations

1. **Always include alt text**: Descriptive alternative text for screen readers
2. **Use ARIA labels**: For more complex SVGs, use aria-label attributes
3. **Maintain contrast**: Ensure sufficient contrast between elements
4. **Avoid relying on color alone**: Use patterns or shapes in addition to colors
5. **Test with screen readers**: Verify accessibility with assistive technologies

## Recommended SVG Diagram Types

| Type | Best For | Example File |
|------|----------|--------------|
| Architecture | System relationships | `high-level-architecture.svg` |
| Workflow | Process steps | `development-workflow.svg` |
| Sequence | Time-based interactions | `authentication-sequence.svg` |
| Component | System components | `integration-components.svg` |
| Decision Tree | Decision processes | `transport-decision-tree.svg` |
| Data Flow | Information movement | `sap-to-github-flow.svg` |

## Example SVGs in Documentation

### Architecture Diagram Example

<div align="center">
<img src="https://github.com/three-horizon/SAP-GitHub-Integration-Playbook/raw/main/assets/images/architecture/high-level-architecture.svg?sanitize=true" width="700" alt="SAP-GitHub Integration Architecture">
<p><em>High-level Integration Architecture</em></p>
</div>

### Workflow Diagram Example

<div align="center">
<img src="https://github.com/three-horizon/SAP-GitHub-Integration-Playbook/raw/main/assets/images/flows/development-workflow.svg?sanitize=true" width="700" alt="Development Workflow">
<p><em>Development Workflow Process</em></p>
</div>

## Troubleshooting

| Issue | Solution |
|-------|----------|
| SVG not rendering | Add `?sanitize=true` to the URL |
| SVG too large | Set width attribute and use responsive styling |
| Text misaligned | Convert text to paths in the SVG |
| Accessibility issues | Add ARIA labels and ensure alt text |

---

For questions or assistance with SVG implementation, contact the documentation team. 