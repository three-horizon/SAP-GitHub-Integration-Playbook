#!/bin/bash
#
# SAP-GitHub Integration Dependencies Installation Script
# This script installs and configures all required dependencies for the SAP-GitHub integration.
#
# Usage: ./install_deps.sh [options]

set -e

# Default values
INSTALL_DIR="$(pwd)"
SAP_GUI=false
ABAPGIT=true
NODE=true
PYTHON=true
DOCKER=false
FORCE=false
VERBOSE=false
LOG_FILE="install_log_$(date +%Y%m%d_%H%M%S).log"

# Check if running as root
if [ "$(id -u)" -eq 0 ]; then
    IS_ROOT=true
else
    IS_ROOT=false
fi

# Display help information
function display_help() {
    echo "SAP-GitHub Integration Dependencies Installation Script"
    echo ""
    echo "This script installs and configures all required dependencies for the SAP-GitHub integration."
    echo ""
    echo "Usage: ./install_deps.sh [options]"
    echo ""
    echo "Options:"
    echo "  -d, --dir           Installation directory (default: current directory)"
    echo "  --sap-gui          Install SAP GUI (default: false)"
    echo "  --no-abapgit       Skip abapGit installation (default: install)"
    echo "  --no-node          Skip Node.js installation (default: install)"
    echo "  --no-python        Skip Python installation (default: install)"
    echo "  --docker           Install Docker (default: false)"
    echo "  -f, --force        Force reinstallation of components"
    echo "  -v, --verbose      Enable verbose logging"
    echo "  -h, --help         Display this help message"
    echo ""
    echo "Example: ./install_deps.sh --dir /opt/sap-github --sap-gui --docker"
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

# Check if a command exists
function command_exists() {
    command -v "$1" >/dev/null 2>&1
}

# Parse command line arguments
while [[ $# -gt 0 ]]; do
    key="$1"
    case $key in
        -d|--dir)
            INSTALL_DIR="$2"
            shift
            shift
            ;;
        --sap-gui)
            SAP_GUI=true
            shift
            ;;
        --no-abapgit)
            ABAPGIT=false
            shift
            ;;
        --no-node)
            NODE=false
            shift
            ;;
        --no-python)
            PYTHON=false
            shift
            ;;
        --docker)
            DOCKER=true
            shift
            ;;
        -f|--force)
            FORCE=true
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

# Create installation directory if it doesn't exist
if [ ! -d "$INSTALL_DIR" ]; then
    log "Creating installation directory: $INSTALL_DIR"
    mkdir -p "$INSTALL_DIR"
fi

# Detect operating system
OS=""
if [ -f /etc/os-release ]; then
    . /etc/os-release
    OS=$ID
elif [ -f /etc/lsb-release ]; then
    . /etc/lsb-release
    OS=$DISTRIB_ID
elif [ "$(uname)" == "Darwin" ]; then
    OS="macos"
else
    OS="unknown"
fi

log "Detected operating system: $OS"

# Install package based on OS
function install_package() {
    local package=$1
    
    log_verbose "Installing package: $package"
    
    case $OS in
        ubuntu|debian)
            if [ "$IS_ROOT" = true ]; then
                apt-get install -y "$package"
            else
                sudo apt-get install -y "$package"
            fi
            ;;
        rhel|centos|fedora)
            if [ "$IS_ROOT" = true ]; then
                yum install -y "$package"
            else
                sudo yum install -y "$package"
            fi
            ;;
        macos)
            if command_exists brew; then
                brew install "$package"
            else
                log "Error: Homebrew not found. Please install Homebrew first."
                log "Run: /bin/bash -c \"$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)\""
                exit 1
            fi
            ;;
        *)
            log "Error: Unsupported operating system for automatic installation."
            log "Please install $package manually."
            exit 1
            ;;
    esac
}

# Function to install Node.js
function install_node() {
    if command_exists node && [ "$FORCE" = false ]; then
        local version=$(node -v)
        log "Node.js is already installed: $version"
        return 0
    fi
    
    log "Installing Node.js..."
    
    case $OS in
        ubuntu|debian)
            if [ "$IS_ROOT" = true ]; then
                curl -fsSL https://deb.nodesource.com/setup_16.x | bash -
                apt-get install -y nodejs
            else
                curl -fsSL https://deb.nodesource.com/setup_16.x | sudo bash -
                sudo apt-get install -y nodejs
            fi
            ;;
        rhel|centos|fedora)
            if [ "$IS_ROOT" = true ]; then
                curl -fsSL https://rpm.nodesource.com/setup_16.x | bash -
                yum install -y nodejs
            else
                curl -fsSL https://rpm.nodesource.com/setup_16.x | sudo bash -
                sudo yum install -y nodejs
            fi
            ;;
        macos)
            if command_exists brew; then
                brew install node@16
            else
                log "Error: Homebrew not found. Please install Homebrew first."
                exit 1
            fi
            ;;
        *)
            log "Error: Unsupported operating system for Node.js installation."
            log "Please install Node.js manually from https://nodejs.org/"
            exit 1
            ;;
    esac
    
    log "Node.js installed successfully: $(node -v)"
    log "NPM installed successfully: $(npm -v)"
}

# Function to install Python
function install_python() {
    if command_exists python3 && [ "$FORCE" = false ]; then
        local version=$(python3 --version)
        log "Python is already installed: $version"
        return 0
    fi
    
    log "Installing Python..."
    
    case $OS in
        ubuntu|debian)
            install_package "python3"
            install_package "python3-pip"
            ;;
        rhel|centos|fedora)
            install_package "python3"
            install_package "python3-pip"
            ;;
        macos)
            install_package "python3"
            ;;
        *)
            log "Error: Unsupported operating system for Python installation."
            log "Please install Python manually from https://www.python.org/"
            exit 1
            ;;
    esac
    
    log "Python installed successfully: $(python3 --version)"
    log "Pip installed successfully: $(pip3 --version)"
}

# Function to install Docker
function install_docker() {
    if command_exists docker && [ "$FORCE" = false ]; then
        local version=$(docker --version)
        log "Docker is already installed: $version"
        return 0
    fi
    
    log "Installing Docker..."
    
    case $OS in
        ubuntu|debian)
            if [ "$IS_ROOT" = true ]; then
                apt-get update
                apt-get install -y apt-transport-https ca-certificates curl gnupg lsb-release
                curl -fsSL https://download.docker.com/linux/ubuntu/gpg | gpg --dearmor -o /usr/share/keyrings/docker-archive-keyring.gpg
                echo "deb [arch=amd64 signed-by=/usr/share/keyrings/docker-archive-keyring.gpg] https://download.docker.com/linux/ubuntu $(lsb_release -cs) stable" > /etc/apt/sources.list.d/docker.list
                apt-get update
                apt-get install -y docker-ce docker-ce-cli containerd.io
            else
                sudo apt-get update
                sudo apt-get install -y apt-transport-https ca-certificates curl gnupg lsb-release
                curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo gpg --dearmor -o /usr/share/keyrings/docker-archive-keyring.gpg
                echo "deb [arch=amd64 signed-by=/usr/share/keyrings/docker-archive-keyring.gpg] https://download.docker.com/linux/ubuntu $(lsb_release -cs) stable" | sudo tee /etc/apt/sources.list.d/docker.list > /dev/null
                sudo apt-get update
                sudo apt-get install -y docker-ce docker-ce-cli containerd.io
                sudo usermod -aG docker $USER
            fi
            ;;
        rhel|centos|fedora)
            if [ "$IS_ROOT" = true ]; then
                yum install -y yum-utils
                yum-config-manager --add-repo https://download.docker.com/linux/centos/docker-ce.repo
                yum install -y docker-ce docker-ce-cli containerd.io
                systemctl start docker
                systemctl enable docker
            else
                sudo yum install -y yum-utils
                sudo yum-config-manager --add-repo https://download.docker.com/linux/centos/docker-ce.repo
                sudo yum install -y docker-ce docker-ce-cli containerd.io
                sudo systemctl start docker
                sudo systemctl enable docker
                sudo usermod -aG docker $USER
            fi
            ;;
        macos)
            log "For macOS, please download and install Docker Desktop from https://www.docker.com/products/docker-desktop"
            log "After installation, run this script again to continue with the setup."
            exit 0
            ;;
        *)
            log "Error: Unsupported operating system for Docker installation."
            log "Please install Docker manually from https://docs.docker.com/get-docker/"
            exit 1
            ;;
    esac
    
    log "Docker installed successfully: $(docker --version)"
}

# Function to download and prepare abapGit
function prepare_abapgit() {
    log "Preparing abapGit resources..."
    
    # Create abapGit directory
    mkdir -p "$INSTALL_DIR/abapgit"
    
    # Download latest abapGit ZIP
    log_verbose "Downloading latest abapGit release..."
    curl -sL https://github.com/abapGit/abapGit/releases/latest/download/zabapgit.zip -o "$INSTALL_DIR/abapgit/zabapgit.zip"
    
    log "abapGit resources prepared in $INSTALL_DIR/abapgit"
}

# Function to install SAP GUI (placeholder)
function install_sap_gui() {
    log "SAP GUI installation requested."
    log "Note: SAP GUI installation is not automated. Please install manually."
    
    case $OS in
        macos)
            log "For macOS, download SAP GUI from the SAP software center and follow installation instructions."
            log "SAP GUI for Java is available at: https://support.sap.com/en/product/connectors/gui-java.html"
            ;;
        ubuntu|debian|rhel|centos|fedora)
            log "For Linux, download SAP GUI for Java from SAP software center."
            log "SAP GUI for Java is available at: https://support.sap.com/en/product/connectors/gui-java.html"
            ;;
        *)
            log "Please download SAP GUI from the SAP software center and follow installation instructions."
            ;;
    esac
    
    log "After installing SAP GUI, you can continue with the integration setup."
}

# Install required system dependencies
log "Installing required system dependencies..."

case $OS in
    ubuntu|debian)
        if [ "$IS_ROOT" = true ]; then
            apt-get update
            apt-get install -y curl git jq unzip zip
        else
            sudo apt-get update
            sudo apt-get install -y curl git jq unzip zip
        fi
        ;;
    rhel|centos|fedora)
        if [ "$IS_ROOT" = true ]; then
            yum install -y curl git jq unzip zip
        else
            sudo yum install -y curl git jq unzip zip
        fi
        ;;
    macos)
        if command_exists brew; then
            brew install curl git jq
        else
            log "Error: Homebrew not found. Please install Homebrew first."
            exit 1
        fi
        ;;
    *)
        log "Warning: Unsupported OS. Please ensure curl, git, jq, unzip, and zip are installed."
        ;;
esac

# Install selected components
if [ "$NODE" = true ]; then
    install_node
fi

if [ "$PYTHON" = true ]; then
    install_python
fi

if [ "$DOCKER" = true ]; then
    install_docker
fi

if [ "$ABAPGIT" = true ]; then
    prepare_abapgit
fi

if [ "$SAP_GUI" = true ]; then
    install_sap_gui
fi

# Install GitHub CLI
log "Installing GitHub CLI..."
case $OS in
    ubuntu|debian)
        if [ "$IS_ROOT" = true ]; then
            curl -fsSL https://cli.github.com/packages/githubcli-archive-keyring.gpg | dd of=/usr/share/keyrings/githubcli-archive-keyring.gpg
            echo "deb [arch=$(dpkg --print-architecture) signed-by=/usr/share/keyrings/githubcli-archive-keyring.gpg] https://cli.github.com/packages stable main" | tee /etc/apt/sources.list.d/github-cli.list > /dev/null
            apt-get update
            apt-get install -y gh
        else
            curl -fsSL https://cli.github.com/packages/githubcli-archive-keyring.gpg | sudo dd of=/usr/share/keyrings/githubcli-archive-keyring.gpg
            echo "deb [arch=$(dpkg --print-architecture) signed-by=/usr/share/keyrings/githubcli-archive-keyring.gpg] https://cli.github.com/packages stable main" | sudo tee /etc/apt/sources.list.d/github-cli.list > /dev/null
            sudo apt-get update
            sudo apt-get install -y gh
        fi
        ;;
    rhel|centos|fedora)
        if [ "$IS_ROOT" = true ]; then
            dnf install -y 'dnf-command(config-manager)'
            dnf config-manager --add-repo https://cli.github.com/packages/rpm/gh-cli.repo
            dnf install -y gh
        else
            sudo dnf install -y 'dnf-command(config-manager)'
            sudo dnf config-manager --add-repo https://cli.github.com/packages/rpm/gh-cli.repo
            sudo dnf install -y gh
        fi
        ;;
    macos)
        brew install gh
        ;;
    *)
        log "Please install GitHub CLI manually from https://github.com/cli/cli#installation"
        ;;
esac

# Install required Node.js packages
if [ "$NODE" = true ]; then
    log "Installing Node.js packages..."
    npm install -g yaml-cli @sap/abap-deploy
    log "Node.js packages installed."
fi

# Install required Python packages
if [ "$PYTHON" = true ]; then
    log "Installing Python packages..."
    pip3 install requests PyYAML gitpython
    log "Python packages installed."
fi

# Create configuration directory
CONFIG_DIR="$INSTALL_DIR/config"
log "Creating configuration directory: $CONFIG_DIR"
mkdir -p "$CONFIG_DIR"

# Create a basic configuration file
log "Creating basic configuration file..."
cat > "$CONFIG_DIR/sap-github-config.json" << EOF
{
  "sap": {
    "systems": []
  },
  "github": {
    "organization": "",
    "repositories": []
  },
  "integration": {
    "abapGitPath": "${INSTALL_DIR}/abapgit",
    "workflowTemplates": "${INSTALL_DIR}/templates"
  }
}
EOF

log "Installation completed successfully!"
log "Configuration directory: $CONFIG_DIR"
log "Log file: $LOG_FILE"

log "Next steps:"
log "1. Edit the configuration file at $CONFIG_DIR/sap-github-config.json"
log "2. Configure your SAP systems for abapGit"
log "3. Set up GitHub authentication"
log "4. Run the integration setup script"

exit 0
