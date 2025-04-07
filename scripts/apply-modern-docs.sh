#!/bin/bash

# Script to apply modern Markdown formatting to all documentation files
# Usage: ./scripts/apply-modern-docs.sh

set -e

echo "📝 Starting documentation modernization process..."

# Check dependencies
if ! command -v find &> /dev/null; then
    echo "❌ Error: 'find' command is required but not installed."
    exit 1
fi

# Create a backup directory
timestamp=$(date +%Y%m%d%H%M%S)
backup_dir="backup_docs_$timestamp"
mkdir -p "$backup_dir"
echo "📦 Created backup directory: $backup_dir"

# Function to add emoji to title
add_emoji_to_title() {
    local file=$1
    local section_name=$(basename $(dirname "$file"))
    local emoji="📄"
    
    # If the file doesn't start with a title, skip it
    if ! grep -q "^# " "$file"; then
        return
    fi
    
    # Determine appropriate emoji based on section or filename
    case "$section_name" in
        "1-architecture") emoji="🏛️";;
        "2-implementation-guide") emoji="🔧";;
        "3-developer-guide") emoji="👨‍💻";;
        "4-operations-guide") emoji="🛠️";;
        "5-reference") emoji="📚";;
        "6-appendices") emoji="📑";;
    esac
    
    # Special cases for common files
    if [[ $(basename "$file") == "README.md" ]]; then
        emoji="📋"
    fi
    
    # Replace first heading with emoji version, but preserve YAML frontmatter if present
    if grep -q "^---" "$file" && grep -q "^---" <(tail -n +2 "$file"); then
        # File has frontmatter, find the first heading after it
        frontmatter_end=$(grep -n "^---" "$file" | sed -n '2p' | cut -d: -f1)
        sed -i.bak "$(($frontmatter_end+1)),\$ s/^# /# $emoji /" "$file"
    else
        # No frontmatter, just replace the first heading
        sed -i.bak '0,/^# / s/^# /# '"$emoji"' /' "$file"
    fi
    
    rm -f "$file.bak"
}

# Function to add centered diagrams placeholder
add_centered_diagram_placeholder() {
    local file=$1
    local has_diagram=false
    local section_name=$(basename $(dirname "$file"))
    local diagram_path=""
    local diagram_caption=""
    
    # Default diagram path based on section
    case "$section_name" in
        "1-architecture") 
            diagram_path="../../assets/images/architecture/high-level-architecture.svg"
            diagram_caption="Architecture diagram for SAP-GitHub integration"
            ;;
        "2-implementation-guide") 
            diagram_path="../../assets/images/flows/sap-github-workflow.svg"
            diagram_caption="Implementation workflow diagram"
            ;;
        "3-developer-guide") 
            diagram_path="../../assets/images/flows/github-to-sap-flow.svg"
            diagram_caption="Development workflow diagram"
            ;;
        "4-operations-guide") 
            diagram_path="../../assets/images/flows/security-monitoring-flow.svg"
            diagram_caption="Operations monitoring diagram"
            ;;
        "5-reference") 
            diagram_path="../../assets/images/architecture/high-level-architecture.svg"
            diagram_caption="Reference architecture diagram"
            ;;
        "6-appendices") 
            diagram_path="../../assets/images/flows/integration-decision-tree.svg"
            diagram_caption="Integration decision tree diagram"
            ;;
        *) 
            diagram_path="../../assets/images/architecture/high-level-architecture.svg"
            diagram_caption="SAP-GitHub Integration diagram"
            ;;
    esac
    
    # Check if file already has a centered diagram
    if grep -q "<div align=\"center\"" "$file"; then
        has_diagram=true
    fi
    
    # Add centered diagram placeholder after the first section if not present
    if [ "$has_diagram" = false ]; then
        # Find first empty line after the first heading
        # Skip YAML frontmatter if present
        if grep -q "^---" "$file" && grep -q "^---" <(tail -n +2 "$file"); then
            frontmatter_end=$(grep -n "^---" "$file" | sed -n '2p' | cut -d: -f1)
            line_number=$(tail -n +$(($frontmatter_end+1)) "$file" | grep -n "^$" | head -1 | cut -d: -f1)
            line_number=$(($line_number + $frontmatter_end))
        else
            line_number=$(grep -n "^$" "$file" | head -1 | cut -d: -f1)
        fi
        
        if [ -n "$line_number" ]; then
            # Insert placeholder
            sed -i.bak "${line_number}a\\
<div align=\"center\">\\
  \\
  ![$(basename "$file" .md) Overview](${diagram_path})\\
  \\
  *${diagram_caption}*\\
</div>" "$file"
            rm -f "$file.bak"
        fi
    fi
}

# Function to add table of contents
add_table_of_contents() {
    local file=$1
    local has_toc=false
    
    # Check if file already has a table of contents
    if grep -q "## Table of Contents\|## 📋 Table of Contents" "$file"; then
        has_toc=true
    fi
    
    # Add TOC after the first heading if not present and file is long enough
    if [ "$has_toc" = false ]; then
        # Count the number of headings
        heading_count=$(grep -c "^##" "$file")
        
        if [ "$heading_count" -ge 3 ]; then
            # Generate TOC entries based on existing headings
            # First find all headings after the title
            headings=$(grep -n "^## " "$file" | sed 's/:[[:space:]]*## /|/g')
            
            toc_entries=""
            while IFS='|' read -r line heading; do
                # Convert heading to link format
                link=$(echo "$heading" | tr '[:upper:]' '[:lower:]' | sed 's/[^a-z0-9 -]//g' | tr ' ' '-')
                
                # Remove emoji from link but keep it in the display text
                link_without_emoji=$(echo "$link" | sed 's/[^a-z0-9-]//g')
                
                # Add to TOC entries
                emoji=""
                if [[ "$heading" =~ \:([a-z0-9_-]+)\: ]]; then
                    emoji="${BASH_REMATCH[0]}"
                    toc_entries="$toc_entries- [$heading](#$link_without_emoji)\\n"
                else
                    toc_entries="$toc_entries- [$heading](#$link)\\n"
                fi
            done <<< "$headings"
            
            # Find place to insert TOC - first empty line after title
            # Skip YAML frontmatter if present
            if grep -q "^---" "$file" && grep -q "^---" <(tail -n +2 "$file"); then
                frontmatter_end=$(grep -n "^---" "$file" | sed -n '2p' | cut -d: -f1)
                title_line=$(tail -n +$(($frontmatter_end+1)) "$file" | grep -n "^# " | head -1 | cut -d: -f1)
                title_line=$(($title_line + $frontmatter_end))
                line_number=$(tail -n +$(($title_line+1)) "$file" | grep -n "^$" | head -1 | cut -d: -f1)
                line_number=$(($line_number + $title_line))
            else
                title_line=$(grep -n "^# " "$file" | head -1 | cut -d: -f1)
                line_number=$(tail -n +$(($title_line+1)) "$file" | grep -n "^$" | head -1 | cut -d: -f1)
                line_number=$(($line_number + $title_line))
            fi
            
            if [ -n "$line_number" ]; then
                # Insert TOC
                sed -i.bak "${line_number}a\\
## 📋 Table of Contents\\
\\
$toc_entries" "$file"
                rm -f "$file.bak"
            fi
        fi
    fi
}

# Function to add emoji to section headers
add_emoji_to_sections() {
    local file=$1
    
    # Add emoji to common section headers
    sed -i.bak '
        s/^## Overview$/## 📋 Overview/g;
        s/^## Getting Started$/## 🚀 Getting Started/g;
        s/^## Prerequisites$/## ✅ Prerequisites/g;
        s/^## Installation$/## 💾 Installation/g;
        s/^## Configuration$/## ⚙️ Configuration/g;
        s/^## Usage$/## 🔍 Usage/g;
        s/^## Examples$/## 💻 Examples/g;
        s/^## API Reference$/## 📚 API Reference/g;
        s/^## Security$/## 🛡️ Security/g;
        s/^## Related Documents$/## 🔗 Related Documents/g;
        s/^## Next Steps$/## ➡️ Next Steps/g;
        s/^## Implementation$/## 🔧 Implementation/g;
        s/^## Architecture$/## 🏛️ Architecture/g;
        s/^## Troubleshooting$/## 🔨 Troubleshooting/g;
        s/^## Monitoring$/## 📊 Monitoring/g;
        s/^## References$/## 📚 References/g;
        s/^## Contents$/## 📋 Table of Contents/g;
        s/^## FAQ$/## ❓ FAQ/g;
        s/^## Features$/## ✨ Features/g;
        s/^## Components$/## 🧩 Components/g;
        s/^## Integration$/## 🔄 Integration/g;
        s/^## Testing$/## 🧪 Testing/g;
        s/^## Deployment$/## 🚀 Deployment/g;
        s/^## Maintenance$/## 🔧 Maintenance/g;
        s/^## Backup$/## 💾 Backup/g;
        s/^## Support$/## 🆘 Support/g;
        s/^## Training$/## 🎓 Training/g;
        s/^## Best Practices$/## ✨ Best Practices/g;
    ' "$file"
    rm -f "$file.bak"
}

# Function to add metadata section
add_metadata_section() {
    local file=$1
    local has_metadata=false
    
    # Check if file already has a metadata section
    if grep -q "Document Metadata\|📊 Document Metadata" "$file"; then
        has_metadata=true
    fi
    
    # Add metadata section if not present
    if [ "$has_metadata" = false ]; then
        # Get current date
        current_date=$(date +%Y-%m-%d)
        
        # Check if file already has a horizontal rule at the end
        if ! grep -q "^---$" <<< "$(tail -n 3 "$file")"; then
            echo -e "\n---" >> "$file"
        fi
        
        # Add metadata section at the end of the file
        echo -e "\n<details>\n<summary><strong>📊 Document Metadata</strong></summary>\n\n- **Last Updated:** $current_date\n- **Author:** SAP-GitHub Integration Team\n- **Version:** 1.0.0\n- **Status:** Draft\n</details>" >> "$file"
    fi
}

# Function to convert notes and warnings to callout blocks
convert_to_callout_blocks() {
    local file=$1
    
    # Convert notes and warnings to callout blocks
    sed -i.bak '
        s/^Note: /> **ℹ️ Note:** /g;
        s/^NOTE: /> **ℹ️ Note:** /g;
        s/^WARNING: /> **⚠️ Warning:** /g;
        s/^Warning: /> **⚠️ Warning:** /g;
        s/^Tip: /> **💡 Tip:** /g;
        s/^TIP: /> **💡 Tip:** /g;
        s/^Important: /> **❗ Important:** /g;
        s/^IMPORTANT: /> **❗ Important:** /g;
    ' "$file"
    rm -f "$file.bak"
}

# Process all documentation files
echo "🔍 Finding all markdown files..."
find_cmd="find . -name '*.md' -not -path '*/_includes/*' -not -path '*/\.git/*' -not -path '*/backup_docs_*/*'"

# Create list of files to process
eval $find_cmd > "$backup_dir/files_to_process.txt"
file_count=$(wc -l < "$backup_dir/files_to_process.txt")
echo "📄 Found $file_count markdown files to process"

# Process each file
counter=0
while IFS= read -r file; do
    # Skip already modernized files
    if [[ "$file" == "./DOCUMENTATION_GUIDELINES.md" || 
          "$file" == "./docs/6-appendices/templates/document-template.md" || 
          "$file" == "./docs/6-appendices/templates/checklist-template.md" ||
          "$file" == "./assets/README.md" ||
          "$file" == "./README.md" ||
          "$file" == "./docs/README.md" ||
          "$file" == "./docs/1-architecture/README.md" ||
          "$file" == "./docs/2-implementation-guide/README.md" ||
          "$file" == "./docs/3-developer-guide/README.md" ||
          "$file" == "./docs/4-operations-guide/README.md" ||
          "$file" == "./docs/5-reference/README.md" ||
          "$file" == "./docs/6-appendices/README.md" ]]; then
        echo "⏩ Skipping already modernized file: $file"
        continue
    fi
    
    # Make backup of file
    cp "$file" "$backup_dir/$(basename "$file").bak"
    
    # Apply transformations
    echo "🔄 Processing: $file"
    add_emoji_to_title "$file"
    add_emoji_to_sections "$file"
    add_centered_diagram_placeholder "$file"
    add_table_of_contents "$file"
    convert_to_callout_blocks "$file"
    add_metadata_section "$file"
    
    counter=$((counter + 1))
    if [ $((counter % 10)) -eq 0 ]; then
        echo "✅ Processed $counter of $file_count files..."
    fi
done < "$backup_dir/files_to_process.txt"

echo "✅ Processed all $file_count markdown files"
echo "🔄 Documentation modernization complete!"
echo "📦 Original files backed up in: $backup_dir"
echo ""
echo "🔍 Next steps:"
echo "  1. Review the updated files and make manual adjustments as needed"
echo "  2. Run ./scripts/check-broken-links.sh to verify all links are working"
echo "  3. Update diagram paths to point to actual SVG files"