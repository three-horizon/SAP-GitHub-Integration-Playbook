#!/bin/bash

# Script to fix document structure issues
# This script addresses common problems with document headers, SVG references and titles

# Set base directory
BASE_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
TIMESTAMP=$(date +%Y%m%d_%H%M%S)
BACKUP_DIR="${BASE_DIR}/backup_doc_structure_${TIMESTAMP}"

# Create backup directory
mkdir -p "${BACKUP_DIR}"
echo "Created backup directory: ${BACKUP_DIR}"

# Function to fix document structure in a file
fix_document_structure() {
    local file="$1"
    local tempfile="${file}.tmp"
    
    # Skip if not a Markdown file
    if [[ ! "$file" =~ \.md$ ]]; then
        echo "Not a Markdown file, skipping: $file"
        return
    fi
    
    # Backup file
    cp "$file" "${BACKUP_DIR}/$(basename "$file").bak"
    
    # Get the current section based on file location for appropriate emoji
    local dirname=$(dirname "$file")
    local emoji="📄"
    
    # Set emoji based on directory
    if [[ "$dirname" == *"1-architecture"* ]]; then
        emoji="🏛️"
    elif [[ "$dirname" == *"2-implementation-guide"* ]]; then
        emoji="🔧"
    elif [[ "$dirname" == *"3-developer-guide"* ]]; then
        emoji="👨‍💻"
    elif [[ "$dirname" == *"4-operations-guide"* ]]; then
        emoji="🛠️"
    elif [[ "$dirname" == *"5-reference"* ]]; then
        emoji="📚"
    elif [[ "$dirname" == *"6-appendices"* ]]; then
        emoji="📑"
    elif [[ "$dirname" == *"examples"* ]]; then
        emoji="💡"
    elif [[ "$dirname" == *"scripts"* ]]; then
        emoji="🔄"
    fi
    
    # Special cases for common files
    filename=$(basename "$file")
    if [[ "$filename" == "README.md" ]]; then
        emoji="📋"
    elif [[ "$filename" == "CODE_OF_CONDUCT.md" ]]; then
        emoji="🤝"
    elif [[ "$filename" == "CONTRIBUTING.md" ]]; then
        emoji="🔄"
    elif [[ "$filename" == "LICENSE" ]]; then
        emoji="📜"
    fi
    
    # Fix 1: Get the current title and fix repeated emojis
    # Extract first line that starts with # (the title)
    title=$(grep -m 1 "^# " "$file")
    # Remove everything until the first alphanumeric character after #
    clean_title=$(echo "$title" | sed -E 's/^# [^a-zA-Z0-9]+ */# /g')
    # Add the proper emoji
    new_title="# ${emoji} ${clean_title#"# "}"
    
    # Fix 2: Ensure proper spacing around divs
    # This is a complex task that requires multi-line processing, using awk
    awk -v title="$new_title" '
    BEGIN {
        title_found = 0;
        in_div = 0;
        div_content = "";
        line_after_div = "";
        skip_next = 0;
    }
    
    # If this is the first title line, replace it with our fixed title
    /^# / && !title_found {
        print title;
        title_found = 1;
        next;
    }
    
    # Detect div starts
    /<div align="center">/ {
        if (in_div) {
            # We were already in a div, print the accumulated content
            print div_content;
        }
        in_div = 1;
        div_content = $0;
        next;
    }
    
    # Collect div content
    in_div && !/<\/div>/ {
        div_content = div_content "\n" $0;
        next;
    }
    
    # Detect div ends
    /<\/div>/ && in_div {
        # Complete the div content
        div_content = div_content "\n" $0;
        
        # Check if the next line starts with a heading
        if ($0 ~ /<\/div>##/) {
            # Split the line
            gsub(/<\/div>##/, "</div>\n##", $0);
            print div_content;
            div_content = "";
            in_div = 0;
        } else {
            # Normal div end
            print div_content "\n";
            div_content = "";
            in_div = 0;
        }
        next;
    }
    
    # Print all other lines
    !in_div { print }
    ' "$file" > "$tempfile"
    
    # Fix 3: Update SVG paths and ensure all SVG references have proper width
    sed -i '' -E 's|<img src="\./assets/images/overview.svg"|<img src="./assets/images/architecture/playbook-overview.svg"|g' "$tempfile"
    sed -i '' -E 's|<img src="\./assets/images/path/to/[^"]*\.svg"|<img src="./assets/images/architecture/playbook-overview.svg"|g' "$tempfile"
    sed -i '' -E 's|<img src="[^"]*\.svg"([^>]*)>|<img src="\1" alt="SVG Diagram" width="700">|g' "$tempfile"
    
    # Check if any changes were made
    if cmp -s "$file" "$tempfile"; then
        echo "No changes needed in $file"
        rm "$tempfile"
    else
        mv "$tempfile" "$file"
        echo "Fixed document structure in $file"
    fi
}

# Process all markdown files
echo "Finding Markdown files..."
find "${BASE_DIR}" -name "*.md" -not -path "${BASE_DIR}/backup_*/*" -not -path "${BASE_DIR}/.git/*" > "${BACKUP_DIR}/files_to_process.txt"
file_count=$(wc -l < "${BACKUP_DIR}/files_to_process.txt")
echo "Found $file_count Markdown files to process"

# Process each file
counter=0
while IFS= read -r file; do
    fix_document_structure "$file"
    
    counter=$((counter + 1))
    if [ $((counter % 50)) -eq 0 ]; then
        echo "Processed $counter of $file_count files..."
    fi
done < "${BACKUP_DIR}/files_to_process.txt"

echo "✅ Completed document structure fixes for $counter files"
echo "📦 Original files backed up in: ${BACKUP_DIR}"