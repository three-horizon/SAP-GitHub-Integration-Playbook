#!/bin/bash

# Script to convert SVG files to PNG for backward compatibility
# Requires Inkscape or svgexport to be installed

echo "SAP-GitHub Integration Playbook - SVG to PNG Conversion"
echo "======================================================"

# Check if we have a conversion tool available
if command -v inkscape >/dev/null 2>&1; then
    CONVERTER="inkscape"
    echo "Using Inkscape for SVG conversion"
elif command -v svgexport >/dev/null 2>&1; then
    CONVERTER="svgexport"
    echo "Using svgexport for SVG conversion"
else
    echo "Error: No SVG conversion tool found. Please install Inkscape or svgexport (npm install -g svgexport)."
    exit 1
fi

# Create output directories if they don't exist
mkdir -p assets/images/png

# Function to convert SVG to PNG using the available converter
convert_svg_to_png() {
    local svg_file=$1
    local png_file=$(echo "$svg_file" | sed 's/\.svg$/.png/' | sed 's|assets/images/|assets/images/png/|')
    local png_dir=$(dirname "$png_file")
    
    # Create directory if it doesn't exist
    mkdir -p "$png_dir"
    
    echo "Converting: $svg_file -> $png_file"
    
    if [ "$CONVERTER" = "inkscape" ]; then
        inkscape --export-filename="$png_file" "$svg_file" --export-dpi=144 >/dev/null 2>&1
    else
        svgexport "$svg_file" "$png_file" 2x >/dev/null 2>&1
    fi
    
    if [ $? -eq 0 ]; then
        echo "✓ Success"
    else
        echo "✗ Failed"
        FAILED_CONVERSIONS=$((FAILED_CONVERSIONS+1))
    fi
}

# Find SVG files
SVG_FILES=$(find assets/images -type f -name "*.svg" | sort)
TOTAL_FILES=$(echo "$SVG_FILES" | wc -l)
FAILED_CONVERSIONS=0

echo "Found $TOTAL_FILES SVG files to convert"
echo ""

# Convert each SVG file
echo "$SVG_FILES" | while read -r file; do
    convert_svg_to_png "$file"
done

echo ""
echo "======================================================"
echo "Conversion Summary:"
echo "Total SVG files: $TOTAL_FILES"
echo "Failed conversions: $FAILED_CONVERSIONS"

if [ $FAILED_CONVERSIONS -gt 0 ]; then
    echo "WARNING: Some conversions failed. Check the output for details."
    exit 1
else
    echo "All SVG files successfully converted to PNG!"
    exit 0
fi 