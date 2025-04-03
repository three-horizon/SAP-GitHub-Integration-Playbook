# Documentation Style Guide

## Overview

This style guide establishes standards for creating consistent, high-quality documentation throughout the SAP-GitHub Integration Playbook. By following these guidelines, you'll help ensure that our documentation is clear, accessible, and valuable to our users.

## Language and Tone

### Writing Style

- Use clear, concise language
- Write in an active voice rather than passive
- Address the reader directly using "you" rather than "the user"
- Be professional but conversational
- Avoid jargon, or explain it when it must be used
- Use present tense whenever possible

### Example

**Preferred:**
> You can connect your SAP system to GitHub using the provided integration components.

**Not Preferred:**
> The SAP system can be connected to GitHub by users through utilization of the integration components that are provided.

## Document Structure

### Headings and Organization

- Use a single H1 (`#`) for the document title
- Structure content with H2 (`##`), H3 (`###`), and H4 (`####`) headings
- Limit heading nesting to 4 levels maximum
- Keep headings concise and descriptive
- Use sentence case for headings (capitalize first word and proper nouns only)

### Lists

- Use bulleted lists for items without sequence
- Use numbered lists for sequential steps or priority items
- Keep list items parallel in structure
- End each list item with a period if it's a complete sentence

## Formatting Conventions

### Code and Commands

- Use code blocks with appropriate language specification
- Enclose inline code, file names, and commands in backticks
- Provide context before code blocks

```bash
# Example command with comments
./scripts/check-broken-links.sh --verbose
```

### Emphasis

- Use **bold** for UI elements, buttons, and important concepts
- Use *italics* sparingly for new terms or slight emphasis
- Avoid using ALL CAPS for emphasis

### Links

- Use descriptive link text (avoid "click here" or "more information")
- Use relative links for internal documentation
- Verify all links resolve correctly
- Include the full URL for external links

## Images and Diagrams

### Usage

- Include images to clarify complex concepts
- Provide descriptive alt text for all images
- Keep file sizes optimized for web (< 500KB)
- Use SVG format for diagrams when possible

### Example

```markdown
![SAP to GitHub integration architecture diagram](../assets/images/architecture/integration-diagram.png)
```

## Technical Content

### Procedures

- Begin procedural steps with action verbs
- Number steps for multi-step procedures
- Include expected results or verification steps
- Identify prerequisites before procedures

### Examples

- Provide practical, real-world examples
- Include expected output for commands
- Use placeholder text consistently (`<example>`, `UPPERCASE`, or `{variables}`)
- Explain why an example is useful

## Reference Documentation

### API Documentation

- Document all parameters with types and descriptions
- Provide request and response examples
- Include authentication requirements
- Note any rate limiting or restrictions

### Configuration

- Document all configuration options with:
  - Valid values/ranges
  - Default values
  - Impact of changes
  - Examples for common scenarios

## Content Maintenance

### Version Indicators

- Clearly mark content specific to particular versions
- Include version information in metadata
- Note deprecated features

### Document Metadata

All documents should include standardized metadata:

```markdown
---

**Document Metadata:**
- Last Updated: YYYY-MM-DD
- Contributors: Names or teams
- Version: X.Y.Z
```

## Accessibility Considerations

- Use descriptive link text that makes sense out of context
- Provide alt text for all images and diagrams
- Use sufficient color contrast for text and diagrams
- Create a logical heading structure

---

**Document Metadata:**
- Last Updated: 2023-09-01
- Contributors: Documentation Team
- Version: 1.0.0