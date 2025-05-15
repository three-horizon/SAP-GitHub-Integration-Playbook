# 📄 Image and Diagram Design Guidelines

**Section:** [Architecture Standards](./README.md) | **Previous:** [Documentation Standards](./documentation-standards.md) | **Next:** [Code Standards](./code-standards.md)

## 📋 Table of Contents

- [Overview](#overview)
- [Design Principles](#design-principles)
- [File Format and Organization](#file-format-and-organization)
- [Color Palette](#color-palette)
- [Typography](#typography)
- [Diagram Elements](#diagram-elements)
- [Diagram Types](#diagram-types)
- [Design Process](#design-process)
- [Tools and Resources](#tools-and-resources)
- [Accessibility](#accessibility)
- [Review Process](#review-process)

## 📋 Overview

This document establishes the standards for creating and maintaining images, diagrams, and visual elements throughout the SAP-GitHub Integration Playbook. Following these guidelines ensures consistency, clarity, and professionalism across all visual content.

All diagrams must be **detailed yet intuitive**, use the **specified color scheme** for different technologies and processes, contain text properly **positioned within containers**, and follow **modern design principles** for a polished appearance.

## Design Principles

### Core Principles

1. **Clarity**: Diagrams should communicate their message clearly with minimal cognitive load
2. **Consistency**: Visual language should be consistent across all diagrams
3. **Simplicity**: Convey complex ideas through simplified visual representations
4. **Hierarchy**: Use visual hierarchy to guide viewers through the diagram
5. **Accuracy**: Diagrams must accurately represent the technical reality

### Visual Aesthetics

1. **Modern Design**: Clean, minimal aesthetic with ample whitespace
2. **Professional Appearance**: Polished, aligned elements with consistent styling
3. **Meaningful Color**: Intentional use of color to convey meaning
4. **Visual Balance**: Properly distributed elements with balanced composition

## File Format and Organization

### File Format Requirements

- **SVG Format**: All diagrams must be in SVG format
- **Dimensions**: Base width of 1200px (height varies based on content)
- **File Naming**: Use `kebab-case-descriptive-names.svg`
- **Source Files**: Store source files (e.g., .drawio, .fig) in a separate repository

### Directory Structure

```
assets/
└── images/
    ├── architecture/    # System architecture diagrams
    ├── flows/           # Process and data flow diagrams
    ├── devsecops/       # DevSecOps and workflow diagrams
    ├── components/      # Component-level diagrams
    ├── deployment/      # Deployment scenario diagrams
    ├── icons/           # Standardized icons
    └── resources/       # Visual resources (color palette, etc.)
```

### Metadata and Annotations

- Include metadata in SVG files (title, description, creator)
- Provide alt text for accessibility when used in documentation
- Include version information and last updated date

## Color Palette

### Primary Color Palette

| Component Type | Primary Color | Hex Code | Usage |
|---------------|--------------|----------|-------|
| SAP Components | Blue | #0070f2 | SAP systems, services, and products |
| GitHub Components | Purple | #6e5494 | GitHub platforms, services, and features |
| Azure Components | Teal | #0078d4 | Microsoft Azure services and components |
| Security Components | Red | #d13438 | Security controls and monitoring |
| Integration Components | Green | #107c10 | Integration services and middleware |

### Supporting Color Palette

| Element | Color | Hex Code | Usage |
|---------|-------|----------|-------|
| Background | Light Gray | #f5f5f5 | Diagram background |
| Borders | Dark Gray | #666666 | Container borders |
| Text | Dark Gray | #333333 | All text elements |
| Flow Arrows | Dark Blue | #0078d7 | Flow indicators |
| Highlight | Yellow | #ffb900 | Important elements |

### Color Application Rules

1. Use colors consistently for specific component types
2. Maintain sufficient contrast for readability
3. Use color gradients sparingly and consistently
4. Limit the number of colors in a single diagram
5. Use tints and shades of the primary colors for variations

## Typography

### Font Selection

- **Primary Font**: Segoe UI, Helvetica, Arial, sans-serif
- **Alternative Font**: Open Sans, Roboto, sans-serif
- **Monospace Font**: Consolas, "Courier New", monospace (for code)

### Text Guidelines

- **Title**: 24px, semi-bold
- **Subtitle**: 18px, semi-bold
- **Body Text**: 14px, regular
- **Labels**: 12px, regular
- **Annotations**: 10px, italic

### Text Usage Rules

1. All text must be in English
2. Use sentence case for most text elements
3. Keep text concise and focused
4. Text must remain within container boundaries
5. Maintain consistent alignment (usually left)
6. Ensure sufficient contrast with background

## Diagram Elements

### Container Elements

- **Rectangles**: Rounded corners (4px radius)
- **Borders**: 1-2px thickness, solid lines
- **Padding**: Minimum 12px inside containers
- **Grouping**: Visually group related elements

### Connection Elements

- **Arrows**: Use consistent arrow styles for flows
- **Line Styles**: Solid for direct flows, dashed for logical relationships
- **Line Weight**: 1-2px for most connections
- **Direction**: Clearly indicate flow direction

### Icons and Symbols

- Use the standardized icon set from `/assets/images/icons/`
- Maintain consistent size relative to text
- Position icons logically within containers
- Use official product icons where appropriate

## Diagram Types

### Architecture Diagrams

- **Purpose**: Show system structure and relationships
- **Layout**: Hierarchical or layered
- **Key Elements**: Components, interfaces, systems
- **Example**: `high-level-architecture.svg`

### Flow Diagrams

- **Purpose**: Illustrate processes or data movement
- **Layout**: Left-to-right or top-to-bottom flow
- **Key Elements**: Process steps, decision points, data
- **Example**: `code-sync-process.svg`

### Component Diagrams

- **Purpose**: Detail internal structure of components
- **Layout**: Nested containers with clear boundaries
- **Key Elements**: Subcomponents, interfaces, dependencies
- **Example**: `sap-components.svg`

### DevSecOps Diagrams

- **Purpose**: Illustrate development and security workflows
- **Layout**: Circular for cycles, linear for pipelines
- **Key Elements**: Phases, tools, responsibilities
- **Example**: `devsecops-cycle.svg`

### Deployment Diagrams

- **Purpose**: Show system deployment across environments
- **Layout**: Network-style with clear boundaries
- **Key Elements**: Infrastructure, networks, locations
- **Example**: `enterprise-deployment.svg`

## Design Process

### Planning Phase

1. Identify diagram purpose and key information
2. Determine appropriate diagram type
3. Sketch initial concept and layout
4. Review sketch with technical stakeholders

### Creation Phase

1. Use recommended tools (draw.io, Lucidchart, Figma)
2. Apply the standard color palette and typography
3. Use established visual elements and icons
4. Include appropriate annotations and labels

### Review Phase

1. Technical review for accuracy
2. Design review for adherence to standards
3. Accessibility check for color contrast and text size
4. Final approval before committing

## Tools and Resources

### Recommended Tools

1. **draw.io / diagrams.net**: Primary tool for technical diagrams
   - Free and open-source
   - Exports to SVG
   - Supports version control integration

2. **Lucidchart**: Alternative for complex diagrams
   - Collaborative editing
   - Extensive shape libraries
   - Good for enterprise diagrams

3. **Figma**: For highly polished designs
   - Component-based design system
   - Collaborative editing
   - Design system management

### Resources

- **Icon Libraries**: Standard icons in `/assets/images/icons/`
- **Templates**: Starter templates in `/assets/images/resources/`
- **Color Palette**: Official colors in `/assets/images/resources/color-palette.svg`

## Accessibility

### Requirements

1. **Color Contrast**: Minimum 4.5:1 ratio for text
2. **Text Size**: Minimum 12px for readable text
3. **Alt Text**: Descriptive alt text for all images
4. **Color Independence**: Information must not rely solely on color

### Testing

1. Use contrast checkers to verify ratios
2. Test diagrams in grayscale to ensure understanding
3. Review with accessibility tools
4. Include text descriptions for complex diagrams

## Review Process

### Review Criteria

1. **Technical Accuracy**: Does the diagram accurately represent the technical reality?
2. **Visual Standards**: Does it follow the design guidelines?
3. **Clarity**: Is the information clear and understandable?
4. **Accessibility**: Does it meet accessibility requirements?

### Review Workflow

1. Designer submits diagram for review
2. Technical review by subject matter experts
3. Design review by documentation team
4. Feedback incorporated into diagram
5. Final approval and inclusion in documentation

---




