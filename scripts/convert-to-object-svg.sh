#!/bin/bash

# Script to convert SVG img tags to object tags for better GitHub Pages compatibility
# This ensures SVG images will be rendered on GitHub Pages

# Set base directory
BASE_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
TIMESTAMP=$(date +%Y%m%d_%H%M%S)
BACKUP_DIR="${BASE_DIR}/backup_svg_object_${TIMESTAMP}"

# Create backup directory
mkdir -p "${BACKUP_DIR}"
echo "Created backup directory: ${BACKUP_DIR}"

# Function to convert img SVG tags to object tags
convert_to_object_svg() {
    local file="$1"
    local tempfile="${file}.tmp"
    
    # Skip if not a Markdown file
    if [[ ! "$file" =~ \.md$ ]]; then
        echo "Not a Markdown file, skipping: $file"
        return
    fi
    
    # Backup file
    cp "$file" "${BACKUP_DIR}/$(basename "$file").bak"
    
    # Create a temporary file
    cat "$file" > "$tempfile"
    
    # Find all divs with img SVG and convert to object
    awk '
    BEGIN {
        in_div = 0;
        div_content = "";
        div_complete = 0;
    }
    
    # Detect div starts
    /<div align="center"/ {
        in_div = 1;
        div_content = $0;
        div_complete = 0;
        next;
    }
    
    # Collect div content
    in_div && !div_complete {
        div_content = div_content "\n" $0;
        
        # Check if div is complete
        if ($0 ~ /<\/div>/) {
            div_complete = 1;
            
            # Check if div contains SVG img
            if (div_content ~ /<img.*\.svg/) {
                # Extract the SVG path, alt text
                svg_path = div_content;
                gsub(/.*<img[^>]*src="([^"]*\.svg)".*/, "\\1", svg_path);
                
                alt_text = div_content;
                gsub(/.*<img[^>]*alt="([^"]*)".*/, "\\1", alt_text);
                if (alt_text == div_content) alt_text = "SVG Diagram";
                
                # Extract caption
                caption = div_content;
                if (div_content ~ /\*([^*]+)\*/) {
                    gsub(/.*\*([^*]+)\*.*/, "\\1", caption);
                } else {
                    caption = "";
                }
                
                # Build new div with object and img fallback
                new_div = "<div align=\"center\" class=\"svg-container\">\n";
                new_div = new_div "  <!-- Using both object and img as fallback for maximum compatibility -->\n";
                new_div = new_div "  <object type=\"image/svg+xml\" data=\"" svg_path "\" style=\"width: 700px; max-width: 100%;\" aria-label=\"" alt_text "\">\n";
                new_div = new_div "    <img src=\"" svg_path "\" alt=\"" alt_text "\" width=\"700\" />\n";
                new_div = new_div "  </object>\n";
                
                if (caption != "") {
                    new_div = new_div "  \n  *" caption "*\n";
                }
                
                new_div = new_div "</div>";
                
                # Print the converted div
                print new_div;
            } else {
                # No SVG, print original div
                print div_content;
            }
            
            in_div = 0;
            div_content = "";
            next;
        }
        next;
    }
    
    # Print all other lines
    !in_div { print }
    ' "$tempfile" > "$file"
    
    # Clean up temp file
    rm "$tempfile"
    
    # Check if file changed
    if cmp -s "$file" "${BACKUP_DIR}/$(basename "$file").bak"; then
        echo "No SVG divs to convert in $file"
    else
        echo "Converted SVG divs in $file"
    fi
}

# Process specified files or find files containing SVG references
if [ $# -gt 0 ]; then
    # Process specified files
    for file in "$@"; do
        if [ -f "$file" ]; then
            convert_to_object_svg "$file"
        else
            echo "File not found: $file"
        fi
    done
else
    # Find all markdown files
    echo "Finding Markdown files..."
    find "${BASE_DIR}" -name "*.md" -not -path "${BASE_DIR}/backup_*/*" -not -path "${BASE_DIR}/.git/*" > "${BACKUP_DIR}/files_to_process.txt"
    file_count=$(wc -l < "${BACKUP_DIR}/files_to_process.txt")
    echo "Found $file_count Markdown files to process"
    
    # Process each file
    counter=0
    while IFS= read -r file; do
        convert_to_object_svg "$file"
        
        counter=$((counter + 1))
        if [ $((counter % 20)) -eq 0 ]; then
            echo "Processed $counter of $file_count files..."
        fi
    done < "${BACKUP_DIR}/files_to_process.txt"
    
    echo "✅ Converted SVG img tags to object tags in $counter files"
    echo "📦 Original files backed up in: ${BACKUP_DIR}"
fi

# Create global notice with instructions
echo "💡 TIP: For the changes to take effect on GitHub Pages, commit and push the changes."
echo "♻️ If SVGs still don't render, try adjusting paths to be absolute URLs for GitHub Pages."