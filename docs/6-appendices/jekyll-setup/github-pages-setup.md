# 📄 Setting Up GitHub Pages for SAP-GitHub Integration Playbook

## 📋 Table of Contents

- [✅ Prerequisites](#prerequisites)
- [Steps to Enable GitHub Pages](#steps-to-enable-github-pages)
- [Troubleshooting](#troubleshooting)
- [Maintenance](#maintenance)
- [Customization](#customization)


This document provides instructions for setting up GitHub Pages to publish the SAP-GitHub Integration Playbook as a website.

## ✅ Prerequisites

- Admin access to the GitHub repository
- The repository should already have the GitHub Pages workflow file (`.github/workflows/github-pages.yml`)
- The `.nojekyll` file should be present in the root directory

## Steps to Enable GitHub Pages

1. **Navigate to Repository Settings**
   - Go to your repository on GitHub
   - Click on the "Settings" tab

2. **Configure GitHub Pages**
   - In the left sidebar, click on "Pages"
   - Under "Source", select "GitHub Actions" as the build and deployment source
   
<div align="center">
  
   ![GitHub Pages Source Settings](../assets/images/github-pages-source.png)
  
  *\1*
</div>

3. **Configure Custom Domain (Optional)**
   - If you want to use a custom domain, enter it in the "Custom domain" field
   - Click "Save"
   - Add the necessary DNS records for your domain according to GitHub's instructions

4. **Trigger the GitHub Pages Workflow**
   - Go to the "Actions" tab in your repository
   - Find the "GitHub Pages" workflow
   - Click "Run workflow" on the main branch

5. **Verify Deployment**
   - After the workflow completes, go back to Settings > Pages
   - You should see a message saying "Your site is published at [URL]"
   - Click on the URL to view your published documentation

## Troubleshooting

If you encounter issues with GitHub Pages deployment:

1. **Check Workflow Runs**
   - Go to the "Actions" tab to see if the GitHub Pages workflow completed successfully
   - Review any error messages in the workflow logs

2. **Verify Repository Permissions**
   - Ensure the GitHub Actions have the necessary permissions to deploy to Pages
   - Check that the workflow has `permissions: contents: read, pages: write, id-token: write`

3. **Environment Name Issue**
   - If you see an error about an invalid environment name, try removing the environment section from the workflow file:
   ```yaml
   deploy:
     # Remove this section if there are issues
     # environment:
     #   name: github-pages
     #   url: ${{ steps.deployment.outputs.page_url }}
     runs-on: ubuntu-latest
     needs: build
     steps:
       - name: Deploy to GitHub Pages
         id: deployment
         uses: actions/deploy-pages@v2
   ```

## Maintenance

Once GitHub Pages is set up, the site will automatically update whenever changes are pushed to the main branch. The GitHub Actions workflow will build and deploy the updated content.

## Customization

To customize the appearance of the GitHub Pages site:

1. Edit the styles in the `.eleventy.js` file in the GitHub Pages workflow
2. Update the navigation links and structure as needed
3. Commit and push the changes to trigger a new deployment

---

**Document Metadata:**
- Last Updated: 2023-09-01
- Version: 1.0.0
- Status: Published 

---

<details>
<summary><strong>📊 Document Metadata</strong></summary>

- **Last Updated:** 2025-04-07
- **Version:** 1.0.0
- **Status:** Published
</details>
