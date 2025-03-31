#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
SAP-GitHub Integration Configuration Validation Script

This script validates the configuration for SAP-GitHub integration,
checking for required settings, connectivity, and integration readiness.

Usage:
    python3 check_config.py [options]
"""

import os
import sys
import json
import argparse
import logging
import requests
from datetime import datetime
from typing import Dict, List, Any, Optional

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
    handlers=[
        logging.StreamHandler(sys.stdout),
        logging.FileHandler(f"validation_{datetime.now().strftime('%Y%m%d_%H%M%S')}.log")
    ]
)
logger = logging.getLogger("sap-github-validator")

# Constants
CONFIG_FILE_DEFAULT = "config/sap-github-config.json"
GITHUB_API_URL = "https://api.github.com"
SAP_DEFAULT_PORT = 8000


class ConfigValidator:
    """Validator for SAP-GitHub integration configuration."""

    def __init__(self, config_file: str = CONFIG_FILE_DEFAULT):
        """Initialize the validator with the specified configuration file."""
        self.config_file = config_file
        self.config = self._load_config()
        self.github_token = os.environ.get("GITHUB_TOKEN")
        self.validation_results = {
            "timestamp": datetime.now().isoformat(),
            "config_file": config_file,
            "overall_status": "unknown",
            "github": {"status": "unknown", "issues": []},
            "sap": {"status": "unknown", "issues": []},
            "integration": {"status": "unknown", "issues": []}
        }

    def _load_config(self) -> Dict[str, Any]:
        """Load configuration from the JSON file."""
        try:
            with open(self.config_file, 'r') as f:
                config = json.load(f)
            logger.debug(f"Loaded configuration from {self.config_file}")
            return config
        except FileNotFoundError:
            logger.error(f"Configuration file {self.config_file} not found")
            sys.exit(1)
        except json.JSONDecodeError:
            logger.error(f"Invalid JSON in configuration file {self.config_file}")
            sys.exit(1)

    def validate_all(self) -> Dict[str, Any]:
        """Run all validation checks and return the results."""
        logger.info("Starting configuration validation...")
        
        self.validate_github_config()
        self.validate_sap_config()
        self.validate_integration_config()
        
        # Determine overall status
        statuses = [
            self.validation_results["github"]["status"],
            self.validation_results["sap"]["status"],
            self.validation_results["integration"]["status"]
        ]
        
        if "failed" in statuses:
            self.validation_results["overall_status"] = "failed"
        elif "warning" in statuses:
            self.validation_results["overall_status"] = "warning"
        else:
            self.validation_results["overall_status"] = "passed"
        
        logger.info(f"Validation completed with status: {self.validation_results['overall_status']}")
        return self.validation_results

    def validate_github_config(self) -> None:
        """Validate GitHub configuration."""
        logger.info("Validating GitHub configuration...")
        issues = []
        
        # Check organization
        org = self.config.get("github", {}).get("organization")
        if not org:
            issues.append("GitHub organization not specified")
        
        # Check repositories
        repos = self.config.get("github", {}).get("repositories", [])
        if not repos:
            issues.append("No GitHub repositories configured")
        
        for i, repo in enumerate(repos):
            if not repo.get("name"):
                issues.append(f"Repository #{i+1} has no name specified")
            if not repo.get("url"):
                issues.append(f"Repository #{i+1} ({repo.get('name', 'unknown')}) has no URL specified")
        
        # Check GitHub API connectivity
        if self.github_token:
            try:
                headers = {
                    "Authorization": f"token {self.github_token}",
                    "Accept": "application/vnd.github+json"
                }
                
                response = requests.get(f"{GITHUB_API_URL}/user", headers=headers)
                if response.status_code != 200:
                    issues.append(f"Failed to authenticate with GitHub API: {response.status_code}")
                else:
                    # Check access to repositories
                    for repo in repos:
                        repo_name = repo.get("name")
                        if repo_name and org:
                            repo_response = requests.get(
                                f"{GITHUB_API_URL}/repos/{org}/{repo_name}",
                                headers=headers
                            )
                            if repo_response.status_code != 200:
                                issues.append(f"Repository {org}/{repo_name} not accessible: {repo_response.status_code}")
            except Exception as e:
                issues.append(f"Error connecting to GitHub API: {e}")
        else:
            issues.append("GITHUB_TOKEN environment variable not set")
        
        # Update validation results
        if not issues:
            self.validation_results["github"]["status"] = "passed"
            logger.info("GitHub configuration validation passed")
        elif any(issue.startswith("Error") or "not specified" in issue or "not accessible" in issue for issue in issues):
            self.validation_results["github"]["status"] = "failed"
            logger.error("GitHub configuration validation failed")
        else:
            self.validation_results["github"]["status"] = "warning"
            logger.warning("GitHub configuration validation has warnings")
            
        self.validation_results["github"]["issues"] = issues

    def validate_sap_config(self) -> None:
        """Validate SAP configuration."""
        logger.info("Validating SAP configuration...")
        issues = []
        
        # Check SAP systems
        systems = self.config.get("sap", {}).get("systems", [])
        if not systems:
            issues.append("No SAP systems configured")
        
        for i, system in enumerate(systems):
            if not system.get("id"):
                issues.append(f"SAP system #{i+1} has no ID specified")
            if not system.get("host"):
                issues.append(f"SAP system #{i+1} ({system.get('id', 'unknown')}) has no host specified")
            if not system.get("client"):
                issues.append(f"SAP system #{i+1} ({system.get('id', 'unknown')}) has no client specified")
        
        # Check SAP connectivity (placeholder)
        # In a real implementation, you would check connectivity to each SAP system
        # using the appropriate libraries or APIs
        
        # Update validation results
        if not issues:
            self.validation_results["sap"]["status"] = "passed"
            logger.info("SAP configuration validation passed")
        elif any("not configured" in issue or "not specified" in issue for issue in issues):
            self.validation_results["sap"]["status"] = "failed"
            logger.error("SAP configuration validation failed")
        else:
            self.validation_results["sap"]["status"] = "warning"
            logger.warning("SAP configuration validation has warnings")
            
        self.validation_results["sap"]["issues"] = issues

    def validate_integration_config(self) -> None:
        """Validate integration-specific configuration."""
        logger.info("Validating integration configuration...")
        issues = []
        
        # Check abapGit path
        abapgit_path = self.config.get("integration", {}).get("abapGitPath")
        if not abapgit_path:
            issues.append("abapGit path not specified")
        elif not os.path.exists(abapgit_path):
            issues.append(f"abapGit path not found: {abapgit_path}")
        
        # Check workflow templates path
        workflow_templates = self.config.get("integration", {}).get("workflowTemplates")
        if not workflow_templates:
            issues.append("Workflow templates path not specified")
        elif not os.path.exists(workflow_templates):
            issues.append(f"Workflow templates path not found: {workflow_templates}")
        
        # Check mappings
        mappings = self.config.get("integration", {}).get("mappings", [])
        if not mappings:
            issues.append("No integration mappings configured")
        
        for i, mapping in enumerate(mappings):
            if not mapping.get("sap_system"):
                issues.append(f"Mapping #{i+1} has no SAP system specified")
            if not mapping.get("github_repo"):
                issues.append(f"Mapping #{i+1} has no GitHub repository specified")
            if not mapping.get("sap_package"):
                issues.append(f"Mapping #{i+1} has no SAP package specified")
        
        # Check cross-references
        sap_systems = {system.get("id") for system in self.config.get("sap", {}).get("systems", [])}
        github_repos = {repo.get("name") for repo in self.config.get("github", {}).get("repositories", [])}
        
        for i, mapping in enumerate(mappings):
            sap_system = mapping.get("sap_system")
            github_repo = mapping.get("github_repo")
            
            if sap_system and sap_system not in sap_systems:
                issues.append(f"Mapping #{i+1} references non-existent SAP system: {sap_system}")
            if github_repo and github_repo not in github_repos:
                issues.append(f"Mapping #{i+1} references non-existent GitHub repository: {github_repo}")
        
        # Update validation results
        if not issues:
            self.validation_results["integration"]["status"] = "passed"
            logger.info("Integration configuration validation passed")
        elif any("not specified" in issue or "not found" in issue or "non-existent" in issue for issue in issues):
            self.validation_results["integration"]["status"] = "failed"
            logger.error("Integration configuration validation failed")
        else:
            self.validation_results["integration"]["status"] = "warning"
            logger.warning("Integration configuration validation has warnings")
            
        self.validation_results["integration"]["issues"] = issues

    def print_results(self) -> None:
        """Print validation results in a human-readable format."""
        print("\n=== SAP-GitHub Integration Configuration Validation Results ===")
        print(f"Date: {self.validation_results['timestamp']}")
        print(f"Configuration File: {self.validation_results['config_file']}")
        print(f"Overall Status: {self.validation_results['overall_status'].upper()}")
        print("\n--- GitHub Configuration ---")
        print(f"Status: {self.validation_results['github']['status'].upper()}")
        if self.validation_results['github']['issues']:
            print("Issues:")
            for issue in self.validation_results['github']['issues']:
                print(f"  - {issue}")
        else:
            print("No issues found.")
        
        print("\n--- SAP Configuration ---")
        print(f"Status: {self.validation_results['sap']['status'].upper()}")
        if self.validation_results['sap']['issues']:
            print("Issues:")
            for issue in self.validation_results['sap']['issues']:
                print(f"  - {issue}")
        else:
            print("No issues found.")
        
        print("\n--- Integration Configuration ---")
        print(f"Status: {self.validation_results['integration']['status'].upper()}")
        if self.validation_results['integration']['issues']:
            print("Issues:")
            for issue in self.validation_results['integration']['issues']:
                print(f"  - {issue}")
        else:
            print("No issues found.")
        
        print("\n=== End of Validation Results ===\n")

    def save_results(self, output_file: str) -> bool:
        """Save validation results to a file in JSON format."""
        try:
            with open(output_file, 'w') as f:
                json.dump(self.validation_results, f, indent=2)
            logger.info(f"Validation results saved to {output_file}")
            return True
        except Exception as e:
            logger.error(f"Failed to save validation results: {e}")
            return False


def main():
    """Main function to parse arguments and run validation."""
    parser = argparse.ArgumentParser(description="SAP-GitHub Integration Configuration Validator")
    parser.add_argument("--config", default=CONFIG_FILE_DEFAULT, help="Path to configuration file")
    parser.add_argument("--output", help="Path to save validation results (JSON format)")
    parser.add_argument("--quiet", action="store_true", help="Suppress console output")
    parser.add_argument("--verbose", action="store_true", help="Enable verbose logging")
    
    args = parser.parse_args()
    
    # Set logging level
    if args.verbose:
        logger.setLevel(logging.DEBUG)
    
    # Run validation
    validator = ConfigValidator(args.config)
    results = validator.validate_all()
    
    # Print results
    if not args.quiet:
        validator.print_results()
    
    # Save results if output file specified
    if args.output:
        validator.save_results(args.output)
    
    # Exit with appropriate code
    if results["overall_status"] == "failed":
        sys.exit(1)
    else:
        sys.exit(0)


if __name__ == "__main__":
    main()
