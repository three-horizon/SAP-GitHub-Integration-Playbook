#!/bin/bash

# Script to apply modern documentation templates to specific key files
# Usage: ./apply-specific-docs.sh [section]
# Example: ./apply-specific-docs.sh 3-developer-guide

# Set base directory
BASE_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
TIMESTAMP=$(date +%Y%m%d_%H%M%S)
BACKUP_DIR="${BASE_DIR}/backup_section_${TIMESTAMP}"

# Check for section parameter
if [ -z "$1" ]; then
  echo "Please provide a section name (e.g., 1-architecture, 2-implementation-guide, etc.)"
  exit 1
fi

SECTION="$1"

# Create backup directory
mkdir -p "${BACKUP_DIR}"
echo "Created backup directory: ${BACKUP_DIR}"

# Change to base directory to avoid path issues
cd "${BASE_DIR}"

# Find files in the specified section
find_cmd="find ./docs/${SECTION} -name '*.md' -not -path '*/\.git/*'"

# Create list of files to process
eval $find_cmd > "${BACKUP_DIR}/files_to_process.txt"
file_count=$(wc -l < "${BACKUP_DIR}/files_to_process.txt")
echo "Found $file_count markdown files to process in section ${SECTION}"

if [ "$file_count" -eq 0 ]; then
  echo "No files found. Please check the section name."
  exit 1
fi

# Function to add emoji to headings
add_emoji_to_headings() {
    local file="$1"
    local tempfile="${file}.temp"
    
    # Backup file
    cp "$file" "${BACKUP_DIR}/$(basename "$file").bak"
    
    # Add emojis to headings if not already present
    sed -E '
        # Add emoji to main title if not present
        1 s/^# ([^🔄📋📄🔍🏛️🔧👨‍💻🛠️📚📑🚀✅⚙️🔄➡️🔗🛡️💻🤝])/# 📄 \1/g;
        # Add emoji to Overview section if not present
        s/^## Overview/## 📋 Overview/g;
        # Add emoji to Prerequisites section if not present
        s/^## Prerequisites/## ✅ Prerequisites/g;
        # Add emoji to Implementation section if not present
        s/^## Implementation/## 🔧 Implementation/g;
        # Add emoji to Configuration section if not present
        s/^## Configuration/## ⚙️ Configuration/g;
        # Add emoji to Process section if not present
        s/^## Process/## 🔄 Process/g;
        # Add emoji to Next Steps section if not present
        s/^## Next Steps/## ➡️ Next Steps/g;
        # Add emoji to Related Documents section if not present
        s/^## Related Documents/## 🔗 Related Documents/g;
        # Add emoji to Security section if not present
        s/^## Security/## 🛡️ Security/g;
        # Add emoji to Examples section if not present
        s/^## Example/## 💻 Example/g;
        # Add emoji to Contributing section if not present
        s/^## Contributing/## 🤝 Contributing/g;
        # Add emoji to Table of Contents section if not present
        s/^## Table of Contents/## 📋 Table of Contents/g;
    ' "$file" > "$tempfile"
    
    mv "$tempfile" "$file"
    echo "Added emojis to headings in $file"
}

# Function to center images
center_images() {
    local file="$1"
    local tempfile="${file}.temp"
    
    # Check if file has ![image] that's not in a div already
    if grep -q "!\[.*\](" "$file" && ! grep -q "<div align=\"center\"" "$file"; then
        awk '
        BEGIN { in_img_block = 0 }
        {
            # If line contains an image and is not already in a centered div
            if ($0 ~ /!\[.*\]\(.*\)/ && !in_img_block && $0 !~ /<div align="center">/) {
                # Extract the image line
                img_line = $0
                # Extract image alt text
                alt_text = img_line
                gsub(/.*!\[(.*)\].*/, "\\1", alt_text)
                
                # Output centered div
                print "<div align=\"center\">\n  "
                print img_line
                print "  "
                print "  *" alt_text "*"
                print "</div>"
                in_img_block = 0
                next
            }
            # Regular line, just print it
            print $0
        }' "$file" > "$tempfile"
        
        mv "$tempfile" "$file"
        echo "Centered images in $file"
    else
        echo "No images to center in $file or already centered"
    fi
}

# Function to add metadata section if not present
add_metadata_section() {
    local file="$1"
    local tempfile="${file}.temp"
    
    # Check if metadata section already exists
    if grep -q "<details>" "$file"; then
        echo "Metadata section already exists in $file"
        return
    fi
    
    # Get current date
    local current_date=$(date +%Y-%m-%d)
    
    # Add metadata section at the end of file
    cat "$file" > "$tempfile"
    echo "" >> "$tempfile"
    echo "---" >> "$tempfile"
    echo "" >> "$tempfile"
    echo "<details>" >> "$tempfile"
    echo "<summary><strong>📊 Document Metadata</strong></summary>" >> "$tempfile"
    echo "" >> "$tempfile"
    echo "- **Last Updated:** ${current_date}" >> "$tempfile"
    echo "- **Version:** 1.0.0" >> "$tempfile"
    echo "- **Status:** Published" >> "$tempfile"
    echo "</details>" >> "$tempfile"
    
    mv "$tempfile" "$file"
    echo "Added metadata section to $file"
}

# Function to add table of contents if not present
add_table_of_contents() {
    local file="$1"
    local tempfile="${file}.temp"
    
    # Check if table of contents already exists
    if grep -q "## 📋 Table of Contents" "$file"; then
        echo "Table of contents already exists in $file"
        return
    fi
    
    # If file has at least 3 headings, add TOC
    if [ $(grep -c "^##" "$file") -ge 3 ]; then
        # Create TOC content
        local toc_content="## 📋 Table of Contents\n\n"
        
        # Extract headings
        while IFS= read -r line; do
            if [[ $line =~ ^##[^#] ]]; then
                # Extract emoji if present
                emoji=$(echo "$line" | grep -o -E '[🔄📋📄🔍🏛️🔧👨‍💻🛠️📚📑🚀✅⚙️🔄➡️🔗🛡️💻🤝]' || echo "")
                
                # Extract heading text
                heading_text=$(echo "$line" | sed -E 's/^##[[:space:]]*([🔄📋📄🔍🏛️🔧👨‍💻🛠️📚📑🚀✅⚙️🔄➡️🔗🛡️💻🤝][[:space:]]*)?//')
                
                # Create link
                link=$(echo "$heading_text" | tr '[:upper:]' '[:lower:]' | sed -E 's/[^a-z0-9]+/-/g' | sed -E 's/^-+|-+$//g')
                
                # Add to TOC
                if [[ -n "$emoji" ]]; then
                    toc_content="${toc_content}- [$emoji $heading_text](#$link)\n"
                else
                    toc_content="${toc_content}- [$heading_text](#$link)\n"
                fi
            fi
        done < "$file"
        
        # Find first empty line after title to insert TOC
        line_number=$(grep -n -m1 "^$" "$file" | cut -d: -f1)
        
        if [ -n "$line_number" ]; then
            # Build new file with TOC
            head -n "$line_number" "$file" > "$tempfile"
            echo -e "$toc_content" >> "$tempfile"
            echo "" >> "$tempfile"
            tail -n +$((line_number + 1)) "$file" >> "$tempfile"
            
            mv "$tempfile" "$file"
            echo "Added table of contents to $file"
        else
            echo "Could not find position to insert TOC in $file"
        fi
    else
        echo "Not enough headings for TOC in $file"
    fi
}

# Function to convert notes and warnings to callout blocks
convert_to_callout_blocks() {
    local file="$1"
    local tempfile="${file}.temp"
    
    # Convert notes and warnings to callout blocks
    sed -E '
        s/^Note: /> **ℹ️ Note:** /g;
        s/^NOTE: /> **ℹ️ Note:** /g;
        s/^WARNING: /> **⚠️ Warning:** /g;
        s/^Warning: /> **⚠️ Warning:** /g;
        s/^Tip: /> **💡 Tip:** /g;
        s/^TIP: /> **💡 Tip:** /g;
        s/^Important: /> **❗ Important:** /g;
        s/^IMPORTANT: /> **❗ Important:** /g;
    ' "$file" > "$tempfile"
    
    mv "$tempfile" "$file"
    echo "Converted notes and warnings in $file"
}

# Process each file
counter=0
while IFS= read -r file; do
    # Skip already modernized files
    if [[ "$file" == *"/README.md" ]]; then
        echo "⏩ Skipping README file: $file"
        continue
    fi
    
    echo "🔄 Processing: $file"
    
    # Apply transformations
    add_emoji_to_headings "$file"
    center_images "$file"
    add_table_of_contents "$file"
    convert_to_callout_blocks "$file"
    add_metadata_section "$file"
    
    counter=$((counter + 1))
    echo "✅ Processed $counter of $file_count files"
done < "${BACKUP_DIR}/files_to_process.txt"

echo "✅ Completed processing $counter files in section ${SECTION}"
echo "📦 Original files backed up in: ${BACKUP_DIR}"