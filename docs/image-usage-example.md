---
layout: default
title: Image Usage Example
---

# Image Usage Examples

This document demonstrates the proper ways to reference images in this repository.

## Direct GitHub Image Reference

This method works in both GitHub and GitHub Pages:

![SAP GitHub Integration](https://github.com/three-horizon/SAP-GitHub-Integration-Playbook/raw/main/assets/images/sap-github-integration.svg)

## Jekyll Site.Baseurl Reference

This method works best in GitHub Pages:

![SAP GitHub Integration]({{ site.baseurl }}/assets/images/sap-github-integration.svg)

## Using the Include Helper

This method provides the most flexibility with caption support:

{% include image.html url="/assets/images/sap-github-integration.svg" alt="SAP GitHub Integration" caption="SAP and GitHub Integration Diagram" %}

## Testing SVG Support

SVGs are properly supported and render correctly:

![SVG Test]({{ site.baseurl }}/assets/images/sap-github-integration.svg)

## Best Practices

1. Always store images in `assets/images/` directory
2. Use SVG format when possible for better quality
3. For GitHub viewing, use the direct GitHub raw link
4. For GitHub Pages, use the site.baseurl format or the include helper 