# PNG Image Directory

This directory contains PNG versions of the SVG files found in the repository, created to ensure maximum compatibility with GitHub Pages.

## Why PNGs?

While SVG files offer superior quality and scalability, some browsers and environments (including GitHub Pages in certain configurations) may have issues displaying them correctly. PNG files serve as a reliable fallback option.

## Images Included

The PNGs in this directory correspond to the SVGs in the following directories:

- `architecture/` - Architecture diagrams and overviews
- `devsecops/` - DevSecOps related diagrams
- `icons/` - Icon files
- `flows/` - Process flow diagrams
- `components/` - Component diagrams

## How to Use

When embedding images in your documents, you can use the following pattern to reference PNG versions:

```markdown
![Description](https://github.com/three-horizon/SAP-GitHub-Integration-Playbook/raw/main/assets/images/png/image-name.png)
```

Or in HTML:

```html
<img src="https://github.com/three-horizon/SAP-GitHub-Integration-Playbook/raw/main/assets/images/png/image-name.png" alt="Description">
```

## Converting SVGs to PNGs

To convert additional SVGs to PNGs, use the `svg-to-png-converter.html` tool in the repository root. 