#!/bin/bash

# Script to identify broken links in the repository
# Usage: ./check-broken-links.sh

echo "Starting documentation link check..."

# Check if lychee is installed
if ! command -v lychee &> /dev/null; then
    echo "Lychee link checker not found. Installing..."
    cargo install lychee
    if [ $? -ne 0 ]; then
        echo "Failed to install lychee. Please install manually: https://github.com/lycheeverse/lychee"
        exit 1
    fi
fi

# Create output directory
mkdir -p reports

# Run lychee on all markdown files
echo "Scanning repository for broken links..."
lychee --verbose --no-progress --format json --output reports/broken-links.json '**/*.md' '**/*.html'

# Count broken links
broken_count=$(grep -c "\"status\": \"error\"" reports/broken-links.json)

echo "Scan complete!"
echo "Found $broken_count potential broken links."
echo "Detailed report saved to reports/broken-links.json"
echo ""
echo "To see a list of broken links, run:"
echo "grep -B 2 -A 2 '\"status\": \"error\"' reports/broken-links.json" 