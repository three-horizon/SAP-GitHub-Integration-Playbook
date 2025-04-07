#!/bin/bash

# Script to validate SVG image references in Markdown files
# This script checks if SVG images are properly referenced for GitHub Pages compatibility

# Set base directory
BASE_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
TIMESTAMP=$(date +%Y%m%d_%H%M%S)
REPORT_DIR="${BASE_DIR}/reports"
REPORT_FILE="${REPORT_DIR}/svg_validation_${TIMESTAMP}.txt"

# Create report directory
mkdir -p "${REPORT_DIR}"
echo "Created report directory: ${REPORT_DIR}"

# Function to check SVG image references in a file
validate_svg_references() {
    local file="$1"
    local issues=0
    
    # Skip if not a Markdown file
    if [[ ! "$file" =~ \.md$ ]]; then
        return 0
    fi
    
    # Check for Markdown style SVG references
    markdown_refs=$(grep -c '!\[.*\](.*.svg)' "$file")
    if [ $markdown_refs -gt 0 ]; then
        echo "⚠️  File $file contains $markdown_refs Markdown-style SVG references" >> "$REPORT_FILE"
        issues=$((issues + markdown_refs))
    fi
    
    # Check for proper HTML img tags with width attribute
    html_refs=$(grep -c '<img.*src=.*\.svg' "$file")
    proper_html_refs=$(grep -c '<img.*src=.*\.svg.*width=' "$file")
    if [ $html_refs -gt 0 ] && [ $proper_html_refs -lt $html_refs ]; then
        echo "⚠️  File $file contains HTML SVG references without width attribute" >> "$REPORT_FILE"
        issues=$((issues + (html_refs - proper_html_refs)))
    fi
    
    # Check for SVG references outside of div align="center"
    div_pattern='<div align="center">\_.\{0,500\}\_<img src="\(.\+\.svg\)"\_.\{0,500\}\_<\/div>'
    total_svg_refs=$(grep -c '<img.*src=.*\.svg' "$file")
    
    # If we have SVG references but none in centered divs, flag this
    if [ $total_svg_refs -gt 0 ] && ! grep -q '<div align="center">.*<img.*src=.*\.svg' "$file"; then
        echo "⚠️  File $file contains SVG references outside of centered divs" >> "$REPORT_FILE"
        issues=$((issues + 1))
    fi
    
    # Check for missing alt text
    missing_alt=$(grep -c '<img.*src=.*\.svg[^>]*alt=""' "$file")
    if [ $missing_alt -gt 0 ]; then
        echo "⚠️  File $file contains $missing_alt SVG images without proper alt text" >> "$REPORT_FILE"
        issues=$((issues + missing_alt))
    fi
    
    return $issues
}

# Process all Markdown files
echo "Starting SVG reference validation at $(date)" > "$REPORT_FILE"
echo "Finding Markdown files in ${BASE_DIR}..." >> "$REPORT_FILE"
cd "${BASE_DIR}"
find . -name "*.md" -not -path "*/reports/*" -not -path "*/.git/*" > "${REPORT_DIR}/files_to_process.txt"
file_count=$(wc -l < "${REPORT_DIR}/files_to_process.txt")
echo "Found $file_count Markdown files to process" >> "$REPORT_FILE"
echo "" >> "$REPORT_FILE"
echo "=== Issues Found ===" >> "$REPORT_FILE"

# Initialize counters
total_issues=0
files_with_issues=0

# Process each file
while IFS= read -r file; do
    validate_svg_references "$file"
    file_issues=$?
    
    if [ $file_issues -gt 0 ]; then
        files_with_issues=$((files_with_issues + 1))
        total_issues=$((total_issues + file_issues))
    fi
done < "${REPORT_DIR}/files_to_process.txt"

# Summary report
echo "" >> "$REPORT_FILE"
echo "=== Summary ===" >> "$REPORT_FILE"
echo "Total files processed: $file_count" >> "$REPORT_FILE"
echo "Files with issues: $files_with_issues" >> "$REPORT_FILE"
echo "Total issues found: $total_issues" >> "$REPORT_FILE"

# Print summary to console
echo "✅ Completed SVG reference validation"
echo "📝 Report saved to: ${REPORT_FILE}"
echo "Files processed: $file_count"
echo "Files with issues: $files_with_issues"
echo "Total issues found: $total_issues"

# Exit with status code based on issues found
if [ $total_issues -gt 0 ]; then
    echo "⚠️  Issues found. Please check the report for details."
    exit 1
else
    echo "✅ No issues found. All SVG references are valid."
    exit 0
fi