#!/bin/bash

# Script to list all SVG files in the assets/images directory
# Useful for determining which SVG files are available for use

# Set base directory
BASE_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
IMAGES_DIR="${BASE_DIR}/assets/images"
OUTPUT_FILE="${BASE_DIR}/svg_inventory.md"

echo "# SVG Image Inventory" > "$OUTPUT_FILE"
echo "" >> "$OUTPUT_FILE"
echo "This document lists all SVG files available in the repository for use in documentation." >> "$OUTPUT_FILE"
echo "" >> "$OUTPUT_FILE"
echo "Generated: $(date)" >> "$OUTPUT_FILE"
echo "" >> "$OUTPUT_FILE"
echo "## Available SVG Files" >> "$OUTPUT_FILE"
echo "" >> "$OUTPUT_FILE"

# Find and list all SVG files in a table format
echo "| Category | SVG File | Path |" >> "$OUTPUT_FILE"
echo "|----------|----------|------|" >> "$OUTPUT_FILE"

find "$IMAGES_DIR" -name "*.svg" | sort | while read -r svg_file; do
    rel_path=${svg_file#$BASE_DIR/}
    category=$(echo "$rel_path" | awk -F/ '{print $3}')
    filename=$(basename "$svg_file")
    echo "| $category | $filename | $rel_path |" >> "$OUTPUT_FILE"
done

echo "" >> "$OUTPUT_FILE"
echo "## Usage in Markdown" >> "$OUTPUT_FILE"
echo "" >> "$OUTPUT_FILE"
echo "To use these SVG files in your Markdown documentation, use the following HTML format:" >> "$OUTPUT_FILE"
echo "" >> "$OUTPUT_FILE"
echo '```html' >> "$OUTPUT_FILE"
echo '<div align="center">' >> "$OUTPUT_FILE"
echo '  ' >> "$OUTPUT_FILE"
echo '  <img src="./assets/images/category/filename.svg" alt="Description" width="700">' >> "$OUTPUT_FILE"
echo '  ' >> "$OUTPUT_FILE"
echo '  *Caption for the image*' >> "$OUTPUT_FILE"
echo '</div>' >> "$OUTPUT_FILE"
echo '```' >> "$OUTPUT_FILE"
echo "" >> "$OUTPUT_FILE"
echo "Replace:" >> "$OUTPUT_FILE"
echo "- \`category\` with the appropriate category folder name" >> "$OUTPUT_FILE"
echo "- \`filename.svg\` with the SVG file name" >> "$OUTPUT_FILE"
echo "- \`Description\` with a descriptive alt text" >> "$OUTPUT_FILE"
echo "- \`Caption for the image\` with an appropriate caption" >> "$OUTPUT_FILE"
echo "" >> "$OUTPUT_FILE"
echo "## Path Adjustments" >> "$OUTPUT_FILE"
echo "" >> "$OUTPUT_FILE"
echo "Adjust the path as needed for your document's location:" >> "$OUTPUT_FILE"
echo "" >> "$OUTPUT_FILE"
echo "- For root-level documents: \`./assets/images/...\`" >> "$OUTPUT_FILE"
echo "- For docs/ directory: \`../assets/images/...\`" >> "$OUTPUT_FILE"
echo "- For docs/section/ directory: \`../../assets/images/...\`" >> "$OUTPUT_FILE"

echo "✅ SVG inventory generated in $OUTPUT_FILE"