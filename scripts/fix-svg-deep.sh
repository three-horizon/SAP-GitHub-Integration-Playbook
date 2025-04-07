#!/bin/bash

# Script to perform a deep fix of SVG image references
# This script handles special cases and ensures proper linking of SVG images

# Set base directory
BASE_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
TIMESTAMP=$(date +%Y%m%d_%H%M%S)
BACKUP_DIR="${BASE_DIR}/backup_svg_deep_${TIMESTAMP}"

# Create backup directory
mkdir -p "${BACKUP_DIR}"
echo "Created backup directory: ${BACKUP_DIR}"

# Function to perform a deep fix of SVG references in a file
deep_fix_svg() {
    local file="$1"
    local tempfile="${file}.temp"
    local file_dir=$(dirname "$file")
    local rel_path
    
    # Calculate relative path from file directory to assets/images
    if [[ "$file_dir" == "$BASE_DIR" ]]; then
        rel_path="./assets/images"
    elif [[ "$file_dir" == "$BASE_DIR/docs" ]]; then
        rel_path="../assets/images"
    else
        rel_path="../../assets/images"
    fi
    
    # Skip if not a Markdown file
    if [[ ! "$file" =~ \.md$ ]]; then
        echo "Not a Markdown file, skipping: $file"
        return
    fi
    
    # Backup file
    cp "$file" "${BACKUP_DIR}/$(basename "$file").bak"
    
    # Phase 1: Replace broken paths with correct paths
    sed "s|../../assets/images/path/to/|${rel_path}/|g" "$file" > "$tempfile"
    mv "$tempfile" "$file"
    
    sed "s|../assets/images/path/to/|${rel_path}/|g" "$file" > "$tempfile"
    mv "$tempfile" "$file"
    
    sed "s|./assets/images/path/to/|${rel_path}/|g" "$file" > "$tempfile"
    mv "$tempfile" "$file"
    
    # Phase 2: Replace Markdown style SVG references with HTML img tags
    sed 's/!\[\(.*\)\](\(.*\.svg\))/<img src="\2" alt="\1" width="700">/g' "$file" > "$tempfile"
    mv "$tempfile" "$file"
    
    # Phase 3: Add missing alt text
    sed 's/<img src="\([^"]*\.svg\)"[^>]*>/<img src="\1" alt="SVG Diagram" width="700">/g' "$file" > "$tempfile"
    mv "$tempfile" "$file"
    
    # Phase 4: Ensure all SVG references are in centered divs
    awk '
    BEGIN { in_div = 0; }
    
    # Track if we are inside a div align="center"
    /<div align="center">/ { in_div = 1; print; next; }
    /<\/div>/ { if (in_div) in_div = 0; print; next; }
    
    # For img tags with SVG sources that are not in a div
    /<img.*src=".*\.svg".*>/ && !in_div {
        # Output a centered div with the image and a caption placeholder
        print "<div align=\"center\">"
        print ""
        print $0
        print ""
        print "  *SAP-GitHub Integration Diagram*"
        print "</div>"
        next
    }
    
    # Print all other lines as-is
    { print }
    ' "$file" > "$tempfile"
    
    # Check if any changes were made
    if cmp -s "$file" "$tempfile"; then
        echo "No changes needed in $file"
        rm "$tempfile"
    else
        mv "$tempfile" "$file"
        echo "Fixed SVG references in $file"
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
    deep_fix_svg "${BASE_DIR}/$file"
    
    counter=$((counter + 1))
    echo "Processed $counter of $file_count files..."
done < "${BACKUP_DIR}/files_to_fix.txt"

echo "✅ Completed deep SVG fixes in $counter files"
echo "📦 Original files backed up in: ${BACKUP_DIR}"

# Run validation again to check if issues were fixed
echo "Running validation again to check results..."
"${BASE_DIR}/scripts/validate-svg-references.sh"