#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
SAP-GitHub Integration Helper Script

This script provides utility functions to help manage the integration
between SAP systems and GitHub repositories.

Usage:
    python3 helper_script.py [command] [options]

Commands:
    list-repos        List all configured GitHub repositories
    list-systems      List all configured SAP systems
    check-connection  Check connection to SAP systems
    setup-webhook     Set up a GitHub webhook for a repository
    create-report     Generate integration status report
    sync-status       Show synchronization status between SAP and GitHub
"""

import os
import sys
import json
import yaml
import argparse
import logging
import requests
from datetime import datetime
from typing import Dict, List, Any, Optional, Tuple

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
    handlers=[
        logging.StreamHandler(sys.stdout),
        logging.FileHandler(f"helper_{datetime.now().strftime('%Y%m%d_%H%M%S')}.log")
    ]
)
logger = logging.getLogger("sap-github-helper")

# Constants
CONFIG_FILE_DEFAULT = "config/sap-github-config.json"
GITHUB_API_URL = "https://api.github.com"
SAP_DEFAULT_PORT = 8000  # Default port for HTTP connections to SAP


class SAPGitHubHelper:
    """Helper class for SAP-GitHub integration operations."""

    def __init__(self, config_file: str = CONFIG_FILE_DEFAULT):
        """Initialize the helper with the specified configuration file."""
        self.config_file = config_file
        self.config = self._load_config()
        self.github_token = os.environ.get("GITHUB_TOKEN")
        
        if not self.github_token:
            logger.warning("GITHUB_TOKEN environment variable not set. Some operations may fail.")

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

    def _save_config(self) -> None:
        """Save configuration back to the JSON file."""
        try:
            with open(self.config_file, 'w') as f:
                json.dump(self.config, f, indent=2)
            logger.debug(f"Saved configuration to {self.config_file}")
        except Exception as e:
            logger.error(f"Failed to save configuration: {e}")
            sys.exit(1)

    def list_github_repositories(self) -> List[Dict[str, Any]]:
        """List all GitHub repositories configured in the integration."""
        repos = self.config.get("github", {}).get("repositories", [])
        
        if not repos:
            logger.info("No GitHub repositories configured")
            return []
        
        logger.info(f"Found {len(repos)} configured GitHub repositories")
        for i, repo in enumerate(repos, 1):
            logger.info(f"{i}. {repo.get('name')} - {repo.get('url')}")
            
        return repos

    def list_sap_systems(self) -> List[Dict[str, Any]]:
        """List all SAP systems configured in the integration."""
        systems = self.config.get("sap", {}).get("systems", [])
        
        if not systems:
            logger.info("No SAP systems configured")
            return []
        
        logger.info(f"Found {len(systems)} configured SAP systems")
        for i, system in enumerate(systems, 1):
            logger.info(f"{i}. {system.get('id')} ({system.get('description', 'No description')}) - {system.get('host')}:{system.get('port', SAP_DEFAULT_PORT)}")
            
        return systems

    def check_sap_connection(self, system_id: Optional[str] = None) -> Dict[str, bool]:
        """
        Check connection to one or all SAP systems.
        
        Args:
            system_id: Optional ID of a specific SAP system to check
            
        Returns:
            Dictionary mapping system IDs to connection status (True/False)
        """
        systems = self.config.get("sap", {}).get("systems", [])
        results = {}
        
        if not systems:
            logger.info("No SAP systems configured")
            return results
        
        for system in systems:
            if system_id and system.get("id") != system_id:
                continue
                
            sys_id = system.get("id", "unknown")
            host = system.get("host")
            port = system.get("port", SAP_DEFAULT_PORT)
            
            if not host:
                logger.warning(f"System {sys_id} has no host configured")
                results[sys_id] = False
                continue
                
            # Simple ping test - in a real implementation, this would use proper SAP connectivity
            try:
                logger.info(f"Testing connection to SAP system {sys_id} ({host}:{port})...")
                # This is a placeholder - in a real implementation, use proper SAP connectivity libraries
                # like pyrfc, SAP SDK, or direct HTTP calls to SAP OData services
                connected = self._simulate_sap_connection(system)
                results[sys_id] = connected
                status = "SUCCESS" if connected else "FAILED"
                logger.info(f"Connection to {sys_id}: {status}")
            except Exception as e:
                logger.error(f"Error connecting to {sys_id}: {e}")
                results[sys_id] = False
                
        return results

    def _simulate_sap_connection(self, system: Dict[str, Any]) -> bool:
        """
        Simulate SAP connection for demo purposes.
        
        In a real implementation, this would use proper SAP connectivity libraries.
        """
        # This is just a placeholder simulation
        # In a real implementation, use pyrfc, SAP SDK, or direct HTTP calls to SAP OData services
        return True

    def setup_github_webhook(self, repo_name: str, webhook_url: str, events: List[str] = None) -> bool:
        """
        Set up a GitHub webhook for a repository.
        
        Args:
            repo_name: Name of the GitHub repository
            webhook_url: URL to receive webhook events
            events: List of events to trigger the webhook (default: ["push", "pull_request"])
            
        Returns:
            True if successful, False otherwise
        """
        if not self.github_token:
            logger.error("GITHUB_TOKEN environment variable not set")
            return False
            
        if events is None:
            events = ["push", "pull_request"]
            
        # Find repository details
        org = self.config.get("github", {}).get("organization", "")
        
        if not org:
            logger.error("GitHub organization not configured")
            return False
            
        logger.info(f"Setting up webhook for {org}/{repo_name} to {webhook_url}")
        
        # Set up webhook via GitHub API
        try:
            headers = {
                "Authorization": f"token {self.github_token}",
                "Accept": "application/vnd.github+json"
            }
            
            payload = {
                "name": "web",
                "active": True,
                "events": events,
                "config": {
                    "url": webhook_url,
                    "content_type": "json",
                    "insecure_ssl": "0"
                }
            }
            
            response = requests.post(
                f"{GITHUB_API_URL}/repos/{org}/{repo_name}/hooks",
                headers=headers,
                json=payload
            )
            
            if response.status_code in (201, 200):
                logger.info(f"Webhook set up successfully for {org}/{repo_name}")
                return True
            else:
                logger.error(f"Failed to set up webhook: {response.status_code} - {response.text}")
                return False
                
        except Exception as e:
            logger.error(f"Error setting up webhook: {e}")
            return False

    def create_status_report(self, output_file: str) -> bool:
        """
        Generate an integration status report.
        
        Args:
            output_file: Path to save the report to
            
        Returns:
            True if successful, False otherwise
        """
        report = {
            "timestamp": datetime.now().isoformat(),
            "integration_status": {
                "github": self._get_github_status(),
                "sap_systems": self._get_sap_status(),
                "synchronization": self._get_sync_status()
            },
            "configuration_summary": {
                "github_repositories": len(self.config.get("github", {}).get("repositories", [])),
                "sap_systems": len(self.config.get("sap", {}).get("systems", [])),
                "last_updated": self.config.get("last_updated", "unknown")
            }
        }
        
        # Save report
        try:
            # Determine file format based on extension
            if output_file.endswith(".json"):
                with open(output_file, 'w') as f:
                    json.dump(report, f, indent=2)
            elif output_file.endswith((".yml", ".yaml")):
                with open(output_file, 'w') as f:
                    yaml.dump(report, f, sort_keys=False)
            else:
                # Default to text format
                with open(output_file, 'w') as f:
                    f.write(self._format_report_as_text(report))
                    
            logger.info(f"Status report generated: {output_file}")
            return True
            
        except Exception as e:
            logger.error(f"Failed to generate report: {e}")
            return False

    def _get_github_status(self) -> Dict[str, Any]:
        """Get GitHub integration status."""
        status = {"connected": False, "repositories": {}}
        
        # Check GitHub API connectivity
        if self.github_token:
            try:
                headers = {
                    "Authorization": f"token {self.github_token}",
                    "Accept": "application/vnd.github+json"
                }
                
                response = requests.get(f"{GITHUB_API_URL}/user", headers=headers)
                status["connected"] = response.status_code == 200
                
                if status["connected"]:
                    user_data = response.json()
                    status["user"] = user_data.get("login")
                    
                    # Check repositories
                    for repo in self.config.get("github", {}).get("repositories", []):
                        repo_name = repo.get("name")
                        if repo_name:
                            org = self.config.get("github", {}).get("organization", "")
                            repo_response = requests.get(
                                f"{GITHUB_API_URL}/repos/{org}/{repo_name}",
                                headers=headers
                            )
                            status["repositories"][repo_name] = {
                                "exists": repo_response.status_code == 200,
                                "last_updated": repo.get("last_sync", "never")
                            }
                
            except Exception as e:
                logger.error(f"Error checking GitHub status: {e}")
                
        return status

    def _get_sap_status(self) -> Dict[str, Any]:
        """Get SAP systems status."""
        status = {}
        
        # Check SAP systems connectivity
        for system in self.config.get("sap", {}).get("systems", []):
            sys_id = system.get("id", "unknown")
            status[sys_id] = {
                "connected": self._simulate_sap_connection(system),
                "last_sync": system.get("last_sync", "never"),
                "abapgit_installed": system.get("abapgit_installed", False)
            }
                
        return status

    def _get_sync_status(self) -> Dict[str, Any]:
        """Get synchronization status between SAP and GitHub."""
        # This is a placeholder for a real implementation
        # In a real scenario, you would check the actual synchronization status
        # between each SAP system and GitHub repository
        
        return {
            "last_full_sync": self.config.get("integration", {}).get("last_full_sync", "never"),
            "packages_in_sync": 0,
            "packages_out_of_sync": 0,
            "sync_jobs": {
                "active": 0,
                "pending": 0,
                "failed": 0
            }
        }

    def _format_report_as_text(self, report: Dict[str, Any]) -> str:
        """Format report as plain text."""
        lines = [
            "SAP-GitHub Integration Status Report",
            f"Generated: {report['timestamp']}",
            "-" * 50,
            "",
            "GitHub Status:",
            f"  Connected: {report['integration_status']['github']['connected']}",
        ]
        
        if "user" in report['integration_status']['github']:
            lines.append(f"  User: {report['integration_status']['github']['user']}")
            
        lines.append("")
        lines.append("GitHub Repositories:")
        for repo_name, repo_status in report['integration_status']['github'].get('repositories', {}).items():
            lines.append(f"  {repo_name}:")
            lines.append(f"    Exists: {repo_status['exists']}")
            lines.append(f"    Last Updated: {repo_status['last_updated']}")
            
        lines.append("")
        lines.append("SAP Systems:")
        for sys_id, sys_status in report['integration_status']['sap_systems'].items():
            lines.append(f"  {sys_id}:")
            lines.append(f"    Connected: {sys_status['connected']}")
            lines.append(f"    abapGit Installed: {sys_status['abapgit_installed']}")
            lines.append(f"    Last Sync: {sys_status['last_sync']}")
            
        lines.append("")
        lines.append("Synchronization Status:")
        sync_status = report['integration_status']['synchronization']
        lines.append(f"  Last Full Sync: {sync_status['last_full_sync']}")
        lines.append(f"  Packages In Sync: {sync_status['packages_in_sync']}")
        lines.append(f"  Packages Out of Sync: {sync_status['packages_out_of_sync']}")
        lines.append("  Sync Jobs:")
        lines.append(f"    Active: {sync_status['sync_jobs']['active']}")
        lines.append(f"    Pending: {sync_status['sync_jobs']['pending']}")
        lines.append(f"    Failed: {sync_status['sync_jobs']['failed']}")
        
        lines.append("")
        lines.append("Configuration Summary:")
        lines.append(f"  GitHub Repositories: {report['configuration_summary']['github_repositories']}")
        lines.append(f"  SAP Systems: {report['configuration_summary']['sap_systems']}")
        lines.append(f"  Last Updated: {report['configuration_summary']['last_updated']}")
        
        return "\n".join(lines)

    def show_sync_status(self) -> Dict[str, Any]:
        """
        Show synchronization status between SAP systems and GitHub repositories.
        
        Returns:
            Dictionary with synchronization status information
        """
        logger.info("Checking synchronization status...")
        
        # Get SAP systems and GitHub repositories
        systems = self.config.get("sap", {}).get("systems", [])
        repos = self.config.get("github", {}).get("repositories", [])
        
        if not systems:
            logger.warning("No SAP systems configured")
            
        if not repos:
            logger.warning("No GitHub repositories configured")
            
        # For each SAP system and repository combination that has configured mappings,
        # check the synchronization status
        sync_status = {}
        
        for system in systems:
            sys_id = system.get("id", "unknown")
            sync_status[sys_id] = {}
            
            for repo in repos:
                repo_name = repo.get("name", "unknown")
                
                # Find mappings for this system and repository
                mappings = []
                for mapping in self.config.get("integration", {}).get("mappings", []):
                    if mapping.get("sap_system") == sys_id and mapping.get("github_repo") == repo_name:
                        mappings.append(mapping)
                
                if mappings:
                    logger.info(f"Checking sync status for {sys_id} -> {repo_name}...")
                    # In a real implementation, you would check the actual synchronization status
                    # by comparing SAP package contents with GitHub repository contents
                    
                    # This is a placeholder for demonstration
                    sync_status[sys_id][repo_name] = {
                        "mappings": len(mappings),
                        "packages_in_sync": len(mappings),  # Placeholder
                        "packages_out_of_sync": 0,  # Placeholder
                        "last_sync": self._get_last_sync(sys_id, repo_name),
                        "status": "in_sync"  # Placeholder - would be determined by actual comparison
                    }
                    
                    logger.info(f"Status for {sys_id} -> {repo_name}: {sync_status[sys_id][repo_name]['status']}")
        
        return sync_status

    def _get_last_sync(self, system_id: str, repo_name: str) -> str:
        """Get last synchronization timestamp for a system and repository."""
        # In a real implementation, this would come from sync logs or a database
        return "2023-01-01T00:00:00Z"  # Placeholder
    

def main():
    """Main function to process command line arguments and execute commands."""
    parser = argparse.ArgumentParser(description="SAP-GitHub Integration Helper Script")
    parser.add_argument("command", choices=[
        "list-repos", "list-systems", "check-connection",
        "setup-webhook", "create-report", "sync-status"
    ], help="Command to execute")
    
    # Common options
    parser.add_argument("--config", default=CONFIG_FILE_DEFAULT, help="Path to configuration file")
    parser.add_argument("--verbose", action="store_true", help="Enable verbose logging")
    
    # Command-specific options
    parser.add_argument("--system", help="SAP system ID for system-specific commands")
    parser.add_argument("--repo", help="GitHub repository name for repo-specific commands")
    parser.add_argument("--webhook-url", help="Webhook URL for setup-webhook command")
    parser.add_argument("--events", help="Comma-separated list of events for webhooks")
    parser.add_argument("--output", help="Output file for reports")
    
    args = parser.parse_args()
    
    # Set logging level
    if args.verbose:
        logger.setLevel(logging.DEBUG)
        logger.debug("Verbose logging enabled")
    
    # Create helper instance
    helper = SAPGitHubHelper(args.config)
    
    # Execute command
    try:
        if args.command == "list-repos":
            helper.list_github_repositories()
            
        elif args.command == "list-systems":
            helper.list_sap_systems()
            
        elif args.command == "check-connection":
            results = helper.check_sap_connection(args.system)
            all_connected = all(results.values()) if results else False
            sys.exit(0 if all_connected else 1)
            
        elif args.command == "setup-webhook":
            if not args.repo or not args.webhook_url:
                logger.error("setup-webhook requires --repo and --webhook-url")
                sys.exit(1)
                
            events = args.events.split(",") if args.events else None
            success = helper.setup_github_webhook(args.repo, args.webhook_url, events)
            sys.exit(0 if success else 1)
            
        elif args.command == "create-report":
            if not args.output:
                logger.error("create-report requires --output")
                sys.exit(1)
                
            success = helper.create_status_report(args.output)
            sys.exit(0 if success else 1)
            
        elif args.command == "sync-status":
            status = helper.show_sync_status()
            # Print status summary
            for system, repos in status.items():
                print(f"System {system}:")
                for repo, repo_status in repos.items():
                    print(f"  Repository {repo}: {repo_status['status']} (Last sync: {repo_status['last_sync']})")
                    
    except Exception as e:
        logger.error(f"Error executing command {args.command}: {e}")
        sys.exit(1)


if __name__ == "__main__":
    main()
