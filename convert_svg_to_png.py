#!/usr/bin/env python3
"""
Convert SVG files to PNG using Inkscape or another available method.
This script will look for SVG files in the assets/images directory and
create corresponding PNGs in assets/images/png.
"""

import os
import subprocess
import sys
from pathlib import Path

# Directories to search for SVGs
SVG_DIRS = [
    "assets/images/architecture",
    "assets/images/devsecops",
    "assets/images/icons",
    "assets/images/flows",
    "assets/images/components",
]

# Output directory for PNGs
PNG_DIR = "assets/images/png"

def check_inkscape():
    """Check if Inkscape is available on the system."""
    try:
        subprocess.run(["inkscape", "--version"], 
                      capture_output=True, 
                      text=True, 
                      check=True)
        return True
    except (subprocess.SubprocessError, FileNotFoundError):
        return False

def convert_with_inkscape(svg_path, png_path, width=1400):
    """Convert SVG to PNG using Inkscape."""
    try:
        command = [
            "inkscape",
            "--export-filename", png_path,
            "--export-width", str(width),
            svg_path
        ]
        subprocess.run(command, check=True, capture_output=True)
        print(f"✅ Converted {svg_path} to {png_path}")
        return True
    except subprocess.SubprocessError as e:
        print(f"❌ Error converting {svg_path}: {e}")
        return False

def convert_with_imagemagick(svg_path, png_path, width=1400):
    """Convert SVG to PNG using ImageMagick."""
    try:
        command = [
            "convert",
            "-background", "none",
            "-size", f"{width}x",
            svg_path,
            png_path
        ]
        subprocess.run(command, check=True, capture_output=True)
        print(f"✅ Converted {svg_path} to {png_path}")
        return True
    except subprocess.SubprocessError as e:
        print(f"❌ Error converting {svg_path}: {e}")
        return False

def main():
    """Main function to convert SVGs to PNGs."""
    print("Starting SVG to PNG conversion...")
    
    # Create output directory if it doesn't exist
    os.makedirs(PNG_DIR, exist_ok=True)
    
    # Check for conversion tools
    has_inkscape = check_inkscape()
    
    if not has_inkscape:
        print("⚠️ Inkscape not found. Trying ImageMagick...")
        try:
            subprocess.run(["convert", "--version"], 
                          capture_output=True, 
                          text=True, 
                          check=True)
        except (subprocess.SubprocessError, FileNotFoundError):
            print("❌ Neither Inkscape nor ImageMagick found. Cannot convert SVGs.")
            sys.exit(1)
    
    # Find and convert SVGs
    total_svgs = 0
    successful_conversions = 0
    
    for svg_dir in SVG_DIRS:
        if not os.path.exists(svg_dir):
            print(f"⚠️ Directory {svg_dir} does not exist, skipping.")
            continue
            
        for svg_file in Path(svg_dir).glob("*.svg"):
            total_svgs += 1
            svg_path = str(svg_file)
            png_name = svg_file.stem + ".png"
            png_path = os.path.join(PNG_DIR, png_name)
            
            if has_inkscape:
                success = convert_with_inkscape(svg_path, png_path)
            else:
                success = convert_with_imagemagick(svg_path, png_path)
            
            if success:
                successful_conversions += 1
    
    print(f"\nConversion complete: {successful_conversions}/{total_svgs} SVGs converted to PNG.")
    print(f"PNG files are stored in {PNG_DIR}")

if __name__ == "__main__":
    main() 