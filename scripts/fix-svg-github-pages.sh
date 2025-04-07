#!/bin/bash

# Script to fix SVG rendering issues for GitHub Pages
# This script implements a more compatible approach for SVG display

# Set base directory
BASE_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
TIMESTAMP=$(date +%Y%m%d_%H%M%S)
BACKUP_DIR="${BASE_DIR}/backup_svg_githubpages_${TIMESTAMP}"

# Create backup directory
mkdir -p "${BACKUP_DIR}"
echo "Created backup directory: ${BACKUP_DIR}"

# Function to fix SVG references for GitHub Pages
fix_svg_github_pages() {
    local file="$1"
    local tempfile="${file}.tmp"
    
    # Skip if not a Markdown file
    if [[ ! "$file" =~ \.md$ ]]; then
        echo "Not a Markdown file, skipping: $file"
        return
    fi
    
    # Backup file
    cp "$file" "${BACKUP_DIR}/$(basename "$file").bak"
    
    # Calculate relative path for URLs based on file location
    local depth=$(echo "$file" | tr -dc '/' | wc -c)
    local base_url=""
    
    # For depth > 0, add ../ for each level
    for ((i=0; i<depth-1; i++)); do
        base_url="${base_url}../"
    done
    
    # Phase 1: Replace HTML img tags with direct URLs
    sed "s|<img src=\"\./assets/|<img src=\"${base_url}/assets/|g" "$file" > "$tempfile"
    mv "$tempfile" "$file"
    
    sed "s|<img src=\"\../assets/|<img src=\"${base_url}/assets/|g" "$file" > "$tempfile"
    mv "$tempfile" "$file"
    
    sed "s|<img src=\"\../../assets/|<img src=\"${base_url}/assets/|g" "$file" > "$tempfile"
    mv "$tempfile" "$file"
    
    # Phase 2: Add absolute URLs for GitHub Pages
    repo_name="SAP-GitHub-Integration-Playbook"
    github_url="https://three-horizon.github.io/${repo_name}"
    
    # Replace all relative <img src links with absolute GitHub Pages links
    sed "s|<img src=\"/assets/|<img src=\"${github_url}/assets/|g" "$file" > "$tempfile"
    mv "$tempfile" "$file"
    
    # Check if any changes were made
    if cmp -s "$file" "${BACKUP_DIR}/$(basename "$file").bak"; then
        echo "No changes needed in $file"
    else
        echo "Fixed SVG references in $file for GitHub Pages compatibility"
    fi
}

# Process all markdown files
echo "Finding Markdown files with SVG references..."
grep -l "\.svg" $(find "${BASE_DIR}" -name "*.md" -not -path "${BASE_DIR}/backup_*/*" -not -path "${BASE_DIR}/.git/*") > "${BACKUP_DIR}/files_to_process.txt"
file_count=$(wc -l < "${BACKUP_DIR}/files_to_process.txt")
echo "Found $file_count Markdown files with SVG references to process"

# Process each file
counter=0
while IFS= read -r file; do
    fix_svg_github_pages "$file"
    
    counter=$((counter + 1))
    if [ $((counter % 20)) -eq 0 ]; then
        echo "Processed $counter of $file_count files..."
    fi
done < "${BACKUP_DIR}/files_to_process.txt"

echo "✅ Completed GitHub Pages SVG fixes for $counter files"
echo "📦 Original files backed up in: ${BACKUP_DIR}"

# Add additional CSS for GitHub Pages
echo "Adding GitHub Pages SVG CSS..."
cat > "${BASE_DIR}/assets/css/github-pages-svg.css" << EOF
/* CSS fixes for SVG display on GitHub Pages */

/* Make SVG images responsive */
img[src$=".svg"] {
  max-width: 100% !important;
  height: auto !important;
  display: inline-block !important;
}

/* Center container */
div[align="center"] {
  text-align: center;
  margin: 2rem auto;
}

/* Caption styling */
div[align="center"] em {
  display: block;
  margin-top: 0.5rem;
  font-style: italic;
  color: #555;
}

/* Fix SVG borders */
img[src$=".svg"] {
  border: none !important;
  background: transparent !important;
}

/* Fix SVG scaling on mobile */
@media (max-width: 768px) {
  img[src$=".svg"] {
    width: 100% !important;
    max-width: 500px !important;
  }
}
EOF

echo "✅ Created GitHub Pages SVG CSS"
echo "✨ Done!"