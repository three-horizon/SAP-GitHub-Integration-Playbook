# 📄 SVG Guidelines for Documentation

## 📋 Table of Contents

- [Overview](#overview)
- [Design Standards](#design-standards)
- [Creating SVGs](#creating-svgs)
- [File Structure](#file-structure)
- [Colors and Styles](#colors-and-styles)
- [Accessibility](#accessibility)
- [Best Practices](#best-practices)
- [Recommended Tools](#recommended-tools)
- [References](#references)

## Overview

This document establishes guidelines for creating and using SVG (Scalable Vector Graphics) images in the SAP-GitHub Integration Playbook documentation. SVGs are the preferred format for all diagrams and technical illustrations in the project due to their scalability, accessibility, and maintainability.

### Why SVG?

- **Scalability**: SVGs maintain quality at any resolution
- **Accessibility**: Allow inclusion of alternative text and descriptions
- **Editability**: Can be easily modified without quality loss
- **Size**: Generally have smaller file sizes than rasterized images
- **Interactivity**: Support interactivity and animations when needed
- **Consistency**: Facilitate maintaining a consistent visual style

## Design Standards

### Basic Template

All SVGs should follow a basic template available at `assets/templates/svg-template.svg`. This template includes:

- Basic SVG document structure
- Standard set of styles and colors for SAP and GitHub components
- Marker definitions for arrows and connectors
- Examples of common components

### Color Palette

Use the following colors to maintain visual consistency:

| Component | Fill Color | Border Color | Hexadecimal Code |
|------------|----------------------|--------------|-------------------|
| SAP Components | SAP Green | Dark SAP Green | #0BAF51 / #097F3A |
| GitHub Components | GitHub Purple | Dark GitHub Purple | #6E5494 / #4C3A68 |
| Integration Components | Neutral Gray | Dark Gray | #6B6B6B / #4A4A4A |
| Security Components | Red | Dark Red | #D32F2F / #B71C1C |
| Azure/Microsoft Components | Blue | Dark Blue | #0078D4 / #00598C |
| User/Dev Components | Amber Yellow | Dark Amber | #FFC107 / #FF8F00 |
| Connections | - | Dark Gray | #333333 |
| Diagram Background | White | - | #FFFFFF |

## Creating SVGs

### Recommended Process

1. **Planning**: 
   - Sketch the diagram on paper or wireframing tool
   - Identify main components and relationships
   - Choose the appropriate diagram type

2. **Creation**:
   - Use the SVG template as a starting point
   - Create the diagram using an SVG editing tool
   - Apply colors and styles according to the standard palette

3. **Refinement**:
   - Add metadata (title, description)
   - Check accessibility
   - Optimize SVG code

4. **Validation**:
   - Test in different browsers
   - Check readability at different sizes
   - Run SVG code validation

### Diagram Types

Choose the appropriate diagram type for the content:

- **Architecture Diagrams**: For system structures
- **Flowcharts**: For processes and workflows
- **Component Diagrams**: For relationships between elements
- **Sequence Diagrams**: For temporal interactions
- **Conceptual Diagrams**: For illustrating abstract concepts

## File Structure

### Element Organization

Structure SVG elements logically and consistently:

```svg
<svg>
  <!-- Metadata -->
  <title>Diagram Title</title>
  <desc>Detailed description of what the diagram represents</desc>
  
  <!-- Definitions -->
  <defs>
    <!-- Styles -->
    <style>/* CSS Styles */</style>
    
    <!-- Markers, gradients, patterns -->
    <marker><!-- Marker definitions --></marker>
  </defs>
  
  <!-- Background -->
  <rect class="background"><!-- Background --></rect>
  
  <!-- Diagram Content -->
  <!-- Group related elements -->
  <g id="section-sap"><!-- SAP Components --></g>
  <g id="section-github"><!-- GitHub Components --></g>
  <g id="section-integration"><!-- Integration Components --></g>
  
  <!-- Connections -->
  <g id="connections"><!-- Connection lines and arrows --></g>
  
  <!-- Texts -->
  <g id="labels"><!-- Texts and labels --></g>
</svg>
```

### Naming Conventions

Use meaningful IDs and classes:

- IDs: Unique, camelCase, specific (e.g., `sapSystem`, `githubActions`)
- Classes: Reusable, descriptive, for styles (e.g., `sap-component`, `connection-arrow`)

## Colors and Styles

### Style Application

Prefer CSS styles within the SVG:

```svg
<style type="text/css">
  /* Base styles */
  .diagram-background {
    fill: white;
  }
  
  /* SAP Components */
  .sap-component {
    fill: #0BAF51;
    stroke: #097F3A;
    stroke-width: 2;
  }
  
  /* Connections */
  .connection {
    stroke: #333333;
    stroke-width: 2;
    fill: none;
  }
</style>
```

### Text and Typography

To maintain typographic consistency:

- Main font: 'Segoe UI', Arial, sans-serif
- Sizes:
  - Diagram title: 24px
  - Section titles: 18px 
  - Component labels: 14px
  - Descriptive text: 12px
- Alignment: Use `text-anchor` for consistent alignment

## Accessibility

### Accessibility Requirements

All SVGs must be accessible:

1. **Title and Description**: Include `<title>` and `<desc>` elements
2. **Alternative Text**: Provide alt text when embedding in HTML
3. **Contrast**: Maintain adequate contrast between text and background
4. **Semantics**: Use logical grouping with the `<g>` element
5. **Text as Text**: Avoid converting text to paths

### Accessible SVG Example

```svg
<svg aria-labelledby="title desc" role="img">
  <title id="title">SAP-GitHub Integration Architecture</title>
  <desc id="desc">Diagram showing the main components of the integration between SAP systems and GitHub, including API layers, security, and data flows.</desc>
  
  <!-- Diagram content -->
</svg>
```

## Best Practices

### SVG Optimization

For better performance:

- Remove unnecessary editor metadata
- Simplify paths when possible
- Use optimization tools like SVGO
- Keep the file organized for easy future editing
- Limit use of complex effects

### Implementation Best Practices

- Use `viewBox` to ensure scalability
- Avoid fixed sizes with `width` and `height` in pixels when embedded in HTML
- Group related elements with the `<g>` tag
- Comment complex sections for easier maintenance
- Test in different browsers and devices

### What to Avoid

- Avoid importing rasterized images into SVG
- Don't use custom fonts not included in the SVG
- Avoid complex effects that affect performance
- Don't create excessively detailed SVGs
- Avoid automatic conversion of bitmap images to SVG

## Recommended Tools

### SVG Editors

- **[Inkscape](https://inkscape.org/)**: Complete open source SVG editor
- **[Figma](https://www.figma.com/)**: Design tool with good SVG support
- **[draw.io](https://draw.io/)**: Excellent for technical diagrams
- **[SVGOMG](https://jakearchibald.github.io/svgomg/)**: Online SVG optimizer
- **Code editors**: For manual edits and fine adjustments

### Validation Tools

- **[W3C Validator](https://validator.w3.org/)**: SVG code validation
- **[SVG Accessibility Tester](https://dequeuniversity.com/library/)**: Accessibility verification
- **[Stark](https://www.getstark.co/)**: Contrast and accessibility checking

## References

- [W3C SVG Specification](https://www.w3.org/TR/SVG2/)
- [MDN Web Docs - SVG](https://developer.mozilla.org/en-US/docs/Web/SVG)
- [SVG Accessibility API Mappings](https://www.w3.org/TR/svg-aam-1.0/)
- [Material Design Icons](https://material.io/resources/icons/) - Icon inspiration
- [SAP Fiori Guidelines](https://experience.sap.com/fiori-design/) - Visual reference for SAP components 