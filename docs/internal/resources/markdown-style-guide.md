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

- Always include alt text with descriptive content
- Use SVG format for all diagrams and images when possible
- Include captions below images with italics using the format: `*Caption text*`
- For diagrams, use the standardized format:
  ```markdown
  <div align="center">

  <img src="/assets/images/path/diagram-name.svg" alt="Descriptive Alt Text" width="800">

  *Descriptive Caption for the Diagram*
  </div>
  ```

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
