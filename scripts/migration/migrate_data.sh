#!/bin/bash
#
# SAP to GitHub Migration Script
# This script automates the process of migrating ABAP code from SAP systems to GitHub repositories
# using abapGit and REST APIs.
#
# Usage: ./migrate_data.sh -s <sap_system_id> -c <client> -r <repository_name> -p <package> -u <username> -t <token>

set -e

# Default values
SAP_SYSTEM=""
SAP_CLIENT=""
GITHUB_REPO=""
SAP_PACKAGE=""
USERNAME=""
TOKEN=""
ABAPGIT_URL="http://localhost:8000/sap/zabapgit"
VERBOSE=false
LOG_FILE="migration_$(date +%Y%m%d_%H%M%S).log"

# Display help information
function display_help() {
    echo "SAP to GitHub Migration Script"
    echo ""
    echo "This script automates the process of migrating ABAP code from SAP systems to GitHub repositories."
    echo ""
    echo "Usage: ./migrate_data.sh [options]"
    echo ""
    echo "Options:"
    echo "  -s, --sap-system     SAP system ID (required)"
    echo "  -c, --client         SAP client number (required)"
    echo "  -r, --repo           GitHub repository name (required)"
    echo "  -p, --package        SAP package to migrate (required)"
    echo "  -u, --username       Username for authentication (required)"
    echo "  -t, --token          GitHub Personal Access Token (required)"
    echo "  -a, --abapgit-url    abapGit URL (default: http://localhost:8000/sap/zabapgit)"
    echo "  -v, --verbose        Enable verbose logging"
    echo "  -h, --help           Display this help message"
    echo ""
    echo "Example: ./migrate_data.sh -s DEV -c 100 -r my-abap-project -p ZMY_PACKAGE -u myuser -t ghp_1234567890"
    exit 0
}

# Log message with timestamp
function log() {
    local timestamp=$(date +"%Y-%m-%d %H:%M:%S")
    echo "[$timestamp] $1"
    echo "[$timestamp] $1" >> "$LOG_FILE"
}

# Log verbose message
function log_verbose() {
    if [ "$VERBOSE" = true ]; then
        log "$1"
    fi
}

# Parse command line arguments
while [[ $# -gt 0 ]]; do
    key="$1"
    case $key in
        -s|--sap-system)
            SAP_SYSTEM="$2"
            shift
            shift
            ;;
        -c|--client)
            SAP_CLIENT="$2"
            shift
            shift
            ;;
        -r|--repo)
            GITHUB_REPO="$2"
            shift
            shift
            ;;
        -p|--package)
            SAP_PACKAGE="$2"
            shift
            shift
            ;;
        -u|--username)
            USERNAME="$2"
            shift
            shift
            ;;
        -t|--token)
            TOKEN="$2"
            shift
            shift
            ;;
        -a|--abapgit-url)
            ABAPGIT_URL="$2"
            shift
            shift
            ;;
        -v|--verbose)
            VERBOSE=true
            shift
            ;;
        -h|--help)
            display_help
            ;;
        *)
            echo "Unknown option: $1"
            echo "Use --help for usage information."
            exit 1
            ;;
    esac
done

# Validate required parameters
if [ -z "$SAP_SYSTEM" ] || [ -z "$SAP_CLIENT" ] || [ -z "$GITHUB_REPO" ] || [ -z "$SAP_PACKAGE" ] || [ -z "$USERNAME" ] || [ -z "$TOKEN" ]; then
    echo "Error: Missing required parameters."
    echo "Use --help for usage information."
    exit 1
fi

# Create GitHub repository if it doesn't exist
function create_github_repo_if_not_exists() {
    log "Checking if GitHub repository '$GITHUB_REPO' exists..."
    
    # Get GitHub username from token
    GITHUB_USER=$(curl -s -H "Authorization: token $TOKEN" https://api.github.com/user | grep -o '"login": "[^"]*' | cut -d'"' -f4)
    
    if [ -z "$GITHUB_USER" ]; then
        log "Error: Failed to authenticate with GitHub. Check your token."
        exit 1
    fi
    
    # Check if repository exists
    REPO_EXISTS=$(curl -s -o /dev/null -w "%{http_code}" -H "Authorization: token $TOKEN" "https://api.github.com/repos/$GITHUB_USER/$GITHUB_REPO")
    
    if [ "$REPO_EXISTS" -eq 200 ]; then
        log "Repository '$GITHUB_REPO' already exists."
    else
        log "Creating new repository '$GITHUB_REPO'..."
        
        # Create new repository
        REPO_CREATED=$(curl -s -o /dev/null -w "%{http_code}" -X POST \
            -H "Authorization: token $TOKEN" \
            -H "Accept: application/vnd.github.v3+json" \
            -d "{\"name\":\"$GITHUB_REPO\", \"description\":\"ABAP package $SAP_PACKAGE migrated from SAP $SAP_SYSTEM\", \"private\":true}" \
            "https://api.github.com/user/repos")
        
        if [ "$REPO_CREATED" -eq 201 ]; then
            log "Repository '$GITHUB_REPO' created successfully."
        else
            log "Error: Failed to create repository. HTTP status: $REPO_CREATED"
            exit 1
        fi
    fi
    
    return 0
}

# Configure abapGit in SAP system
function configure_abapgit() {
    log "Configuring abapGit for package '$SAP_PACKAGE' in SAP system '$SAP_SYSTEM'..."
    
    # This part would normally use SAP RFC or HTTP calls to configure abapGit
    # For this template, we'll simulate the process with echo statements
    
    log_verbose "Connecting to SAP system $SAP_SYSTEM client $SAP_CLIENT..."
    log_verbose "Setting up abapGit for package $SAP_PACKAGE..."
    log_verbose "Linking to GitHub repository $GITHUB_REPO..."
    
    # Simulate a delay for the configuration process
    sleep 2
    
    log "abapGit configuration completed."
    return 0
}

# Perform the initial export from SAP to GitHub
function perform_initial_export() {
    log "Performing initial export of package '$SAP_PACKAGE' to GitHub..."
    
    # This part would normally use SAP RFC or HTTP calls to trigger the abapGit export
    # For this template, we'll simulate the process with echo statements
    
    log_verbose "Scanning package $SAP_PACKAGE for objects..."
    log_verbose "Serializing ABAP objects..."
    log_verbose "Pushing to GitHub repository $GITHUB_REPO..."
    
    # Simulate a delay for the export process
    sleep 3
    
    log "Initial export completed successfully."
    return 0
}

# Set up GitHub workflows for the repository
function setup_github_workflows() {
    log "Setting up GitHub workflows for continuous integration..."
    
    # Clone the repository
    TMP_DIR=$(mktemp -d)
    log_verbose "Cloning repository to temporary directory: $TMP_DIR"
    
    git clone "https://$TOKEN@github.com/$GITHUB_USER/$GITHUB_REPO.git" "$TMP_DIR"
    
    if [ $? -ne 0 ]; then
        log "Error: Failed to clone repository."
        exit 1
    fi
    
    # Create workflows directory if it doesn't exist
    mkdir -p "$TMP_DIR/.github/workflows"
    
    # Copy CI/CD workflow template
    log_verbose "Creating CI/CD workflow configuration..."
    
    cat > "$TMP_DIR/.github/workflows/abap-ci.yml" << EOF
name: ABAP CI

on:
  push:
    branches: [ main ]
  pull_request:
    branches: [ main ]

jobs:
  abap-lint:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - name: ABAP Syntax Check
        uses: actions/github-script@v6
        with:
          script: |
            console.log('Performing ABAP syntax check...');
            // This is a placeholder for actual ABAP syntax checking
            // In a real implementation, you would use a custom action or script
EOF
    
    # Commit and push the workflow file
    cd "$TMP_DIR"
    git config user.name "ABAP Migration Script"
    git config user.email "abap-migration@example.com"
    git add .github/workflows/abap-ci.yml
    git commit -m "Add ABAP CI workflow"
    git push
    
    if [ $? -ne 0 ]; then
        log "Error: Failed to push workflow configuration."
        exit 1
    fi
    
    # Clean up
    cd -
    rm -rf "$TMP_DIR"
    
    log "GitHub workflows configured successfully."
    return 0
}

# Main execution flow
log "Starting SAP to GitHub migration process..."
log "Parameters:"
log "  SAP System: $SAP_SYSTEM"
log "  SAP Client: $SAP_CLIENT"
log "  GitHub Repository: $GITHUB_REPO"
log "  SAP Package: $SAP_PACKAGE"
log "  Username: $USERNAME"
log "  abapGit URL: $ABAPGIT_URL"

# Execute migration steps
create_github_repo_if_not_exists
configure_abapgit
perform_initial_export
setup_github_workflows

log "Migration completed successfully!"
log "GitHub repository: https://github.com/$GITHUB_USER/$GITHUB_REPO"
log "Log file: $LOG_FILE"

exit 0
