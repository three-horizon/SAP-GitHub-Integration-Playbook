#!/bin/bash

# Script to fix SVG image references - adds proper centering divs around images
# This script ensures SVG images are displayed correctly on GitHub Pages

# Set base directory
BASE_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
TIMESTAMP=$(date +%Y%m%d_%H%M%S)
BACKUP_DIR="${BASE_DIR}/backup_svg_center_${TIMESTAMP}"

# Create backup directory
mkdir -p "${BACKUP_DIR}"
echo "Created backup directory: ${BACKUP_DIR}"

# Function to wrap SVG images in centered divs
fix_svg_centering() {
    local file="$1"
    local tempfile="${file}.temp"
    
    # Skip if not a Markdown file
    if [[ ! "$file" =~ \.md$ ]]; then
        echo "Not a Markdown file, skipping: $file"
        return
    fi
    
    # Backup file
    cp "$file" "${BACKUP_DIR}/$(basename "$file").bak"
    
    # Find SVG image references that are not in centered divs, and wrap them
    # This regex looks for <img src="*.svg" ... > tags that are not already in div align="center"
    
    # First convert Markdown style SVG references to HTML
    sed -E 's/!\[(.*)\]\((.*\.svg)\)/<img src="\2" alt="\1" width="700">/g' "$file" > "$tempfile"
    mv "$tempfile" "$file"
    
    # Now wrap standalone SVG img tags in centered divs
    awk '
    BEGIN { in_div = 0; div_buffer = ""; }
    
    # Track if we are inside a div align="center"
    /<div align="center">/ { in_div = 1; }
    /<\/div>/ { if (in_div) in_div = 0; }
    
    # For img tags with SVG sources that are not in a div
    /<img.*src=".*\.svg".*>/ && !in_div {
        # Output a centered div with the image and a caption placeholder
        print "<div align=\"center\">"
        print ""
        print $0
        print ""
        print "  *Caption for this diagram*"
        print "</div>"
        next
    }
    
    # Print all other lines as-is
    { print }
    ' "$file" > "$tempfile"
    
    # Check if any changes were made
    if cmp -s "$file" "$tempfile"; then
        echo "No SVG centering issues to fix in $file"
        rm "$tempfile"
    else
        mv "$tempfile" "$file"
        echo "Fixed SVG centering in $file"
    fi
}

# Process all files with issues from the validation report
echo "Finding files with issues..."
REPORT_FILE=$(find "${BASE_DIR}/reports" -name "svg_validation_*.txt" | sort -r | head -1)

if [ -z "$REPORT_FILE" ]; then
    echo "No validation report found. Running validation first..."
    "${BASE_DIR}/scripts/validate-svg-references.sh"
    REPORT_FILE=$(find "${BASE_DIR}/reports" -name "svg_validation_*.txt" | sort -r | head -1)
fi

# Extract files with issues
grep "⚠️" "$REPORT_FILE" | grep -o "File \./[^ ]*" | sed 's/File \.\///' > "${BACKUP_DIR}/files_to_fix.txt"
file_count=$(wc -l < "${BACKUP_DIR}/files_to_fix.txt")
echo "Found $file_count files to fix"

# Process each file with issues
counter=0
while IFS= read -r file; do
    fix_svg_centering "${BASE_DIR}/$file"
    
    counter=$((counter + 1))
    echo "Processed $counter of $file_count files..."
done < "${BACKUP_DIR}/files_to_fix.txt"

echo "✅ Completed fixing SVG centering in $counter files"
echo "📦 Original files backed up in: ${BACKUP_DIR}"

# Run validation again to check if issues were fixed
echo "Running validation again to check results..."
"${BASE_DIR}/scripts/validate-svg-references.sh"