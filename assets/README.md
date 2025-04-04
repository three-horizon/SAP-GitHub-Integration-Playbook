# Assets Directory

This directory contains all assets for the SAP-GitHub Integration Playbook:

- `images/`: Contains all images referenced in the documentation
  - `architecture/`: Architecture diagrams
  - `github-pages/`: GitHub Pages-related images

## Adding Images

When adding images to this directory, please follow these guidelines:

1. Place images in appropriate subdirectories based on content
2. Use descriptive filenames in lowercase with hyphens (e.g., `sap-github-workflow.png`)
3. Optimize images for web before adding (compress JPG/PNG files)
4. Include alt text when referencing images in markdown

## Referencing Images in Documentation

To reference images in markdown files, use the following syntax:

```markdown
![Alt text description](/assets/images/category/image-name.png)
```

For Jekyll-specific image inclusion with more options, use:

```markdown
{% include image.html url="/assets/images/category/image-name.png" alt="Description" caption="Optional caption" %}
``` 