# Documentation Standards

## Table of Contents

- [Overview](#overview)
- [Markdown Best Practices](#markdown-best-practices)
- [Document Structure](#document-structure)
- [Formatting Guidelines](#formatting-guidelines)
- [Images and Media](#images-and-media)
- [Code Examples](#code-examples)
- [Links and References](#links-and-references)
- [Document Metadata](#document-metadata)
- [Versioning](#versioning)
- [Accessibility Guidelines](#accessibility-guidelines)

## Overview

This document defines the documentation standards for this playbook. Following these standards ensures consistency across all documentation, enhances readability, and improves maintainability.

## Markdown Best Practices

### File Naming

- Use lowercase for filenames
- Separate words with hyphens, not underscores (e.g., `getting-started.md`, not `getting_started.md`)
- Keep filenames concise and descriptive
- Avoid special characters and spaces in filenames

### Headings

- Use ATX-style headings (`#` syntax) 
- Include a space after the `#` character
- Start with H1 (`#`) for the document title
- Use H2 (`##`) for main sections
- Use H3 (`###`) for subsections
- Avoid skipping heading levels (e.g., don't go from H2 to H4)
- Include a blank line before and after headings

### Paragraphs

- Use a blank line to separate paragraphs
- Avoid long paragraphs; aim for 3-5 sentences per paragraph
- Use line breaks (`<br>`) sparingly

## Document Structure

All documents should follow this general structure:

1. **Title** (H1): Document title
2. **Navigation aids**: Section, previous, next links
3. **Table of Contents**: For documents longer than 3 sections
4. **Overview/Introduction**: Brief explanation of the document's purpose
5. **Main Content**: Organized in logical sections
6. **Next Steps/Conclusion**: What to do next
7. **Related Documents**: Links to related documentation
8. **Document Metadata**: Version, last updated, etc.

### Example Structure

```markdown
# Document Title

**Section:** [Section Name](./section.md) | **Previous:** [Previous Doc](./prev.md) | **Next:** [Next Doc](./next.md)

## Table of Contents

- [Overview](#overview)
- [Section 1](#section-1)
- [Section 2](#section-2)
- [Next Steps](#next-steps)

## Overview

Brief introduction to the document.

## Section 1

Content for section 1.

## Section 2

Content for section 2.

## Next Steps

What to do next.

## Related Documents

- [Related Document 1](./related1.md)
- [Related Document 2](./related2.md)

---

**Document Metadata:**
- Last Updated: YYYY-MM-DD
- Version: X.Y.Z
- Status: [Draft/Review/Published]
```

## Formatting Guidelines

### Lists

- Use unordered lists (`-`) for items without sequence
- Use ordered lists (`1.`) for sequential steps
- Indent sublists with 2 or 4 spaces
- Include a blank line before and after lists
- Be consistent with punctuation in list items

### Emphasis

- Use **bold** (`**text**`) for emphasis and UI elements
- Use *italics* (`*text*`) for new terms or slight emphasis
- Use `code formatting` for code, commands, file paths, and technical values
- Use blockquotes (`>`) for notes, warnings, or quotations

### Tables

- Use tables for structured data
- Include a header row
- Align columns for readability
- Keep tables simple; break complex tables into smaller ones

Example:

```markdown
| Column 1 | Column 2 | Column 3 |
|----------|----------|----------|
| Value 1  | Value 2  | Value 3  |
| Value 4  | Value 5  | Value 6  |
```

### Callouts

Use this format for callouts:

```markdown
> **Note:** Information that should be highlighted.

> **Warning:** Critical information about potential issues.

> **Tip:** Helpful advice for better usage.
```

## Images and Media

### Image Guidelines

- Store images in the `assets/images` directory
- Organize images in subdirectories by topic
- Use descriptive filenames
- Include alt text for accessibility
- Add captions for complex images
- Use SVG format for diagrams when possible
- Optimize image sizes for web

### Image Syntax

```markdown
![Alt text](../../assets/images/category/image-name.png)
*Caption explaining the image*
```

### Diagrams

- Use consistent styling across all diagrams
- Include a legend for complex diagrams
- Maintain a standard color scheme

## Code Examples

### Code Blocks

- Use fenced code blocks with language specification
- Specify the language for syntax highlighting
- Add comments to explain complex code
- Keep examples concise and focused

Example:

```markdown
```python
# This is a Python example
def hello_world():
    print("Hello, world!")
```
```

### Inline Code

- Use backticks (`) for inline code references
- Use inline code for:
  - Command names
  - File names
  - Function names
  - Variable names
  - Short code snippets

## Links and References

### Internal Links

- Use relative paths for links within the repository
- Link to headings using anchor links when referencing sections in the same document
- Check all links before committing

Example:

```markdown
[Link to document](../category/document.md)
[Link to section](#section-name)
```

### External Links

- Use descriptive link text
- Include the full URL
- Consider adding link title attributes for additional context

Example:

```markdown
[GitHub Documentation](https://docs.github.com "GitHub's official documentation")
```

## Document Metadata

Each document should include metadata at the end:

```markdown
---

**Document Metadata:**
- Last Updated: YYYY-MM-DD
- Author: [Author Name/Team]
- Version: X.Y.Z
- Status: [Draft/Review/Published]
```

## Versioning

- Use semantic versioning (MAJOR.MINOR.PATCH)
- Increment the:
  - MAJOR version for incompatible changes
  - MINOR version for new backwards-compatible functionality
  - PATCH version for backwards-compatible bug fixes
- Include a version history for significant documents

## Accessibility Guidelines

- Use descriptive link text (not "click here")
- Include alt text for all images
- Maintain a logical heading structure
- Use sufficient color contrast
- Don't rely solely on color to convey information
- Use simple, clear language
- Provide text alternatives for non-text content

---

**Document Metadata:**
- Last Updated: 2023-09-01
- Version: 1.0.0
- Status: Published 