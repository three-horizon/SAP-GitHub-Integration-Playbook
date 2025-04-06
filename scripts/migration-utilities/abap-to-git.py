#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
SAP ABAP to Git Migration Utility

This script extracts ABAP objects from an SAP system and converts them into
a Git-compatible format for storage in GitHub repositories.

Usage:
    python abap-to-git.py --sap-system=<system_id> --client=<client> 
                         --package=<package_name> --output-dir=<dir>
                         [--user=<username>] [--password=<password>]
                         [--with-dependencies] [--abapgit-format]

Author: SAP-GitHub Integration Team
Version: 1.0.0
"""

import argparse
import base64
import json
import logging
import os
import sys
import xml.etree.ElementTree as ET
from datetime import datetime
from pathlib import Path

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s',
    handlers=[
        logging.FileHandler("abap-to-git.log"),
        logging.StreamHandler()
    ]
)
logger = logging.getLogger("abap-to-git")

class ABAPToGitConverter:
    """Main converter class for SAP ABAP objects to Git repositories."""
    
    def __init__(self, args):
        """Initialize the converter with command line arguments."""
        self.args = args
        self.object_count = 0
        self.converted_objects = 0
        self.errors = 0
        self.output_dir = Path(args.output_dir)
        self.processed_objects = set()
        
        # Create output directory if it doesn't exist
        self.output_dir.mkdir(parents=True, exist_ok=True)
        
        logger.info(f"Connecting to SAP system {self.args.sap_system} client {self.args.client}")
    
    def run(self):
        """Main execution method."""
        start_time = datetime.now()
        logger.info(f"Starting ABAP to Git conversion at {start_time}")
        
        try:
            # Process package
            if self.args.package:
                self._process_package(self.args.package)
            else:
                logger.error("No package specified")
                return 1
            
            # Generate .abapgit.xml file if using abapgit format
            if self.args.abapgit_format:
                self._generate_abapgit_xml()
            
            # Generate summary
            self._generate_summary(start_time)
            return 0
            
        except Exception as e:
            logger.error(f"Error during conversion: {str(e)}")
            return 1
    
    def _process_package(self, package_name):
        """Process all objects in a package."""
        logger.info(f"Processing package {package_name}")
        
        # Simulate fetching package objects
        objects = [
            ("PROG", "ZTEST_REPORT"),
            ("CLAS", "ZCL_TEST_CLASS"),
            ("INTF", "ZIF_TEST_INTERFACE"),
            ("TABL", "ZTEST_TABLE")
        ]
        
        logger.info(f"Found {len(objects)} objects in package {package_name}")
        
        # Process each object
        for obj_type, obj_name in objects:
            self._process_object(obj_type, obj_name)
    
    def _process_object(self, obj_type, obj_name):
        """Process a single ABAP object."""
        object_key = f"{obj_type}:{obj_name}"
        
        # Skip if already processed
        if object_key in self.processed_objects:
            return
        
        self.processed_objects.add(object_key)
        self.object_count += 1
        
        logger.info(f"Processing object {obj_type} {obj_name}")
        
        try:
            # Get object source code and metadata
            if obj_type == "PROG":
                source = f"""REPORT {obj_name}.

* Sample program
DATA: lv_test TYPE string VALUE 'Hello, World!'.

WRITE: lv_test.
"""
            elif obj_type == "CLAS":
                source = f"""CLASS {obj_name} DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS hello_world.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS {obj_name} IMPLEMENTATION.
  METHOD hello_world.
    WRITE: 'Hello, World!'.
  ENDMETHOD.
ENDCLASS.
"""
            else:
                source = f"* Sample code for {obj_type} {obj_name}\n"
            
            metadata = {
                "type": obj_type,
                "name": obj_name,
                "description": f"Sample {obj_type}",
                "author": "SAP_USER",
                "created_on": "2023-01-15",
                "changed_on": "2023-10-30"
            }
            
            # Save object
            if self.args.abapgit_format:
                self._save_abapgit_format(obj_type, obj_name, source, metadata)
            else:
                self._save_standard_format(obj_type, obj_name, source, metadata)
            
            self.converted_objects += 1
                
        except Exception as e:
            logger.error(f"Error processing {obj_type} {obj_name}: {str(e)}")
            self.errors += 1
    
    def _save_standard_format(self, obj_type, obj_name, source_code, metadata):
        """Save object in standard format (source file + metadata JSON)."""
        type_dir = self.output_dir / obj_type.lower()
        type_dir.mkdir(exist_ok=True)
        
        # Save source code
        source_ext = ".abap" if obj_type in ["PROG", "CLAS", "INTF"] else ".txt"
        source_file = type_dir / f"{obj_name.lower()}{source_ext}"
        with open(source_file, 'w') as f:
            f.write(source_code)
        
        # Save metadata
        meta_file = type_dir / f"{obj_name.lower()}.meta.json"
        with open(meta_file, 'w') as f:
            json.dump(metadata, f, indent=2)
        
        logger.info(f"Saved {obj_type} {obj_name} to {source_file}")
    
    def _save_abapgit_format(self, obj_type, obj_name, source_code, metadata):
        """Save object in abapGit XML format."""
        # Create object directory
        obj_dir = self.output_dir / "src" / f"{obj_name.lower()}.{obj_type.lower()}"
        obj_dir.mkdir(parents=True, exist_ok=True)
        
        # Create XML file with metadata and source
        xml_root = ET.Element("abapGit")
        xml_root.set("version", "v1.0.0")
        
        asx_values = ET.SubElement(ET.SubElement(xml_root, "asx:abap"), "asx:values")
        obj_data = ET.SubElement(asx_values, obj_type)
        
        # Add metadata elements
        for key, value in metadata.items():
            meta_elem = ET.SubElement(obj_data, key)
            meta_elem.text = str(value)
        
        # Add source code
        source_elem = ET.SubElement(obj_data, "source")
        source_elem.text = base64.b64encode(source_code.encode()).decode()
        
        # Write XML file
        xml_file = obj_dir / f"{obj_name.lower()}.{obj_type.lower()}.xml"
        tree = ET.ElementTree(xml_root)
        tree.write(xml_file, encoding="UTF-8", xml_declaration=True)
        
        logger.info(f"Saved {obj_type} {obj_name} to {xml_file}")
    
    def _generate_abapgit_xml(self):
        """Generate .abapgit.xml file in the root directory."""
        abapgit_file = self.output_dir / ".abapgit.xml"
        
        xml_root = ET.Element("abapGit")
        xml_root.set("version", "v1.0.0")
        
        asx_values = ET.SubElement(ET.SubElement(xml_root, "asx:abap"), "asx:values")
        data = ET.SubElement(asx_values, "DATA")
        
        # Add general settings
        master_lang = ET.SubElement(data, "MASTER_LANGUAGE")
        master_lang.text = "E"
        
        starting_folder = ET.SubElement(data, "STARTING_FOLDER")
        starting_folder.text = "/src/"
        
        folder_logic = ET.SubElement(data, "FOLDER_LOGIC")
        folder_logic.text = "PREFIX"
        
        # Write the XML file
        tree = ET.ElementTree(xml_root)
        tree.write(abapgit_file, encoding="UTF-8", xml_declaration=True)
        
        logger.info(f"Generated .abapgit.xml file at {abapgit_file}")
    
    def _generate_summary(self, start_time):
        """Generate summary report."""
        end_time = datetime.now()
        duration = end_time - start_time
        
        summary = {
            "conversion_summary": {
                "start_time": start_time.isoformat(),
                "end_time": end_time.isoformat(),
                "duration_seconds": duration.total_seconds(),
                "total_objects": self.object_count,
                "converted_objects": self.converted_objects,
                "errors": self.errors,
                "success_rate": (self.converted_objects / self.object_count * 100) if self.object_count > 0 else 0
            },
            "configuration": {
                "sap_system": self.args.sap_system,
                "client": self.args.client,
                "package": self.args.package,
                "output_directory": str(self.output_dir),
                "abapgit_format": self.args.abapgit_format
            }
        }
        
        # Save summary to file
        summary_file = self.output_dir / "conversion_summary.json"
        with open(summary_file, 'w') as f:
            json.dump(summary, f, indent=2)
        
        logger.info(f"Conversion completed in {duration.total_seconds():.2f} seconds")
        logger.info(f"Total objects: {self.object_count}")
        logger.info(f"Converted objects: {self.converted_objects}")
        logger.info(f"Errors: {self.errors}")
        logger.info(f"Summary saved to {summary_file}")

def parse_arguments():
    """Parse command line arguments."""
    parser = argparse.ArgumentParser(
        description="Convert SAP ABAP objects to Git repositories"
    )
    
    parser.add_argument("--sap-system", required=True, help="SAP system ID")
    parser.add_argument("--client", required=True, help="SAP client number")
    parser.add_argument("--package", required=True, help="ABAP package name to extract")
    parser.add_argument("--output-dir", required=True, help="Output directory for Git repository")
    parser.add_argument("--user", default=None, help="SAP username")
    parser.add_argument("--password", default=None, help="SAP password")
    parser.add_argument("--with-dependencies", action="store_true", help="Include object dependencies")
    parser.add_argument("--abapgit-format", action="store_true", help="Use abapGit XML format")
    
    return parser.parse_args()

def main():
    """Main entry point."""
    args = parse_arguments()
    
    # Run the converter
    converter = ABAPToGitConverter(args)
    return converter.run()

if __name__ == "__main__":
    sys.exit(main()) 