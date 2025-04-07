#!/bin/bash

# Script to fix SVG image references in Markdown files for better GitHub Pages compatibility
# This script converts Markdown image syntax to HTML img tags with width parameter

# Set base directory
BASE_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
TIMESTAMP=$(date +%Y%m%d_%H%M%S)
BACKUP_DIR="${BASE_DIR}/backup_svg_fix_${TIMESTAMP}"

# Create backup directory
mkdir -p "${BACKUP_DIR}"
echo "Created backup directory: ${BACKUP_DIR}"

# Function to fix SVG image references in a file
fix_svg_references() {
    local file="$1"
    local tempfile="${file}.temp"
    
    # Skip if not a Markdown file
    if [[ ! "$file" =~ \.md$ ]]; then
        echo "Not a Markdown file, skipping: $file"
        return
    fi
    
    # Backup file
    cp "$file" "${BACKUP_DIR}/$(basename "$file").bak"
    
    # Convert Markdown image syntax to HTML img tags for SVG images
    # This preserves the div alignment and keeps the caption
    sed -E '
        # Find the div align center pattern
        /<div align="center">/,/<\/div>/ {
            # Replace Markdown image with HTML img tag specifically for SVG images
            s/!\[(.*)\]\((.*\.svg)\)/\<img src="\2" alt="\1" width="700"\>/g
        }
    ' "$file" > "$tempfile"
    
    # Check if any changes were made
    if cmp -s "$file" "$tempfile"; then
        echo "No SVG references to fix in $file"
        rm "$tempfile"
    else
        mv "$tempfile" "$file"
        echo "Fixed SVG references in $file"
    fi
}

# Process all Markdown files
echo "Finding Markdown files in ${BASE_DIR}..."
cd "${BASE_DIR}"
find . -name "*.md" -not -path "*/backup_*/*" -not -path "*/.git/*" > "${BACKUP_DIR}/files_to_process.txt"
file_count=$(wc -l < "${BACKUP_DIR}/files_to_process.txt")
echo "Found $file_count Markdown files to process"

# Process each file
counter=0
while IFS= read -r file; do
    fix_svg_references "$file"
    
    counter=$((counter + 1))
    if [ $((counter % 50)) -eq 0 ]; then
        echo "Processed $counter of $file_count files..."
    fi
done < "${BACKUP_DIR}/files_to_process.txt"

echo "✅ Completed fixing SVG references in $counter files"
echo "📦 Original files backed up in: ${BACKUP_DIR}"