# SAP-GitHub Integration - Development Environment Setup Script
#
# This PowerShell script automates the setup of a development environment
# for SAP-GitHub integration projects.
#
# Usage:
#   .\setup-dev-environment.ps1 -SapGui $true -GitClient $true -SapCloud $true -InstallDir "C:\SAPGitDev"
#
# Author: SAP-GitHub Integration Team
# Version: 1.0.0

param (
    [Parameter(Mandatory=$false)]
    [string]$InstallDir = "$env:USERPROFILE\SAPGitDev",
    
    [Parameter(Mandatory=$false)]
    [bool]$SapGui = $true,
    
    [Parameter(Mandatory=$false)]
    [bool]$GitClient = $true,
    
    [Parameter(Mandatory=$false)]
    [bool]$SapCloud = $false,
    
    [Parameter(Mandatory=$false)]
    [bool]$VSCode = $true,
    
    [Parameter(Mandatory=$false)]
    [bool]$NodeJs = $true,
    
    [Parameter(Mandatory=$false)]
    [bool]$Python = $true,
    
    [Parameter(Mandatory=$false)]
    [string]$GitHubOrg = "",
    
    [Parameter(Mandatory=$false)]
    [string]$SapSystemId = ""
)

# Configuration
$SapGuiVersion = "7.70"
$GitVersion = "2.40.1"
$NodeJsVersion = "18.16.0"
$PythonVersion = "3.11.3"
$VSCodeVersion = "latest"
$LogFile = "setup_log.txt"

# Function to log messages
function Log-Message {
    param (
        [string]$Message,
        [string]$Level = "INFO"
    )
    
    $timestamp = Get-Date -Format "yyyy-MM-dd HH:mm:ss"
    $logMessage = "[$timestamp] [$Level] $Message"
    
    Write-Host $logMessage
    Add-Content -Path "$InstallDir\$LogFile" -Value $logMessage
}

# Function to create directory if it doesn't exist
function Create-DirectoryIfNotExists {
    param (
        [string]$Path
    )
    
    if (-not (Test-Path $Path)) {
        Log-Message "Creating directory: $Path"
        New-Item -ItemType Directory -Path $Path -Force | Out-Null
        Log-Message "Directory created successfully" 
    } else {
        Log-Message "Directory already exists: $Path"
    }
}

# Function to download file if it doesn't exist
function Download-File {
    param (
        [string]$Url,
        [string]$OutputPath
    )
    
    if (-not (Test-Path $OutputPath)) {
        Log-Message "Downloading $Url to $OutputPath"
        try {
            Invoke-WebRequest -Uri $Url -OutFile $OutputPath
            Log-Message "Download completed successfully"
            return $true
        } catch {
            Log-Message "Error downloading file: $_" "ERROR"
            return $false
        }
    } else {
        Log-Message "File already exists: $OutputPath"
        return $true
    }
}

# Function to install software using chocolatey
function Install-WithChocolatey {
    param (
        [string]$PackageName,
        [string]$Version = ""
    )
    
    $chocoArgs = "install $PackageName -y"
    if ($Version -ne "" -and $Version -ne "latest") {
        $chocoArgs += " --version=$Version"
    }
    
    Log-Message "Installing $PackageName using Chocolatey..."
    
    try {
        # Check if Chocolatey is installed
        if (-not (Get-Command choco -ErrorAction SilentlyContinue)) {
            Log-Message "Chocolatey not found, installing..."
            Set-ExecutionPolicy Bypass -Scope Process -Force
            [System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072
            Invoke-Expression ((New-Object System.Net.WebClient).DownloadString('https://chocolatey.org/install.ps1'))
            Log-Message "Chocolatey installed successfully"
        }
        
        # Install the package
        Invoke-Expression "choco $chocoArgs"
        Log-Message "$PackageName installation completed"
        return $true
    } catch {
        Log-Message "Error installing $PackageName: $_" "ERROR"
        return $false
    }
}

# Function to generate configuration file
function Generate-ConfigFile {
    $configFilePath = "$InstallDir\sap-git-config.json"
    Log-Message "Generating configuration file: $configFilePath"
    
    $config = @{
        "installation" = @{
            "date" = (Get-Date -Format "yyyy-MM-dd")
            "version" = "1.0.0"
            "installDir" = $InstallDir
            "components" = @{
                "sapGui" = $SapGui
                "gitClient" = $GitClient
                "sapCloud" = $SapCloud
                "vsCode" = $VSCode
                "nodeJs" = $NodeJs
                "python" = $Python
            }
        }
        "integration" = @{
            "gitHubOrg" = $GitHubOrg
            "sapSystemId" = $SapSystemId
        }
        "tools" = @{}
    }
    
    $config | ConvertTo-Json -Depth 5 | Set-Content -Path $configFilePath
    Log-Message "Configuration file generated successfully"
}

# Function to configure Git client for SAP development
function Configure-GitForSap {
    Log-Message "Configuring Git for SAP development..."
    
    try {
        # Create useful .gitignore for SAP projects
        $gitignorePath = "$InstallDir\sap.gitignore"
        
        @"
# SAP-specific files to ignore

# Temporary files
*.~*
*.bak
tmp/
temp/

# SAP GUI files
*.pse
locales/

# ABAP Unit test files
*.test

# SAP HANA
*.hdbbackup
*.hdbsql
*.hdblog

# SAP Cloud Platform
.che/
gen/
.mta/
mta_archives/
dist/
node_modules/
package-lock.json

# SAPUI5/Fiori
sap-ui-cachebuster-info.json
visual_ext_index.html
/webapp/visual_ext_index.html
extended_runnable_file.html
.*/extended_runnable_file.html
mock_preview_wlsrvc.html
.*/mock_preview_wlsrvc.html
fioriHtmlRunner.html
.*/fioriHtmlRunner.html
UIAdaptation_index.html
changes_preview.js
AppVariant_index.html
AppVariantPreviewPayload.zip
mergedManifestDescriptor.json
APIExternalProducer.js
.*/APIExternalProducer.js

# IDE settings
.vscode/
.idea/
.project
.settings/
.classpath
.nwabaprc
.jshintrc
.DS_Store
Thumbs.db
"@ | Set-Content -Path $gitignorePath
        
        # Configure global Git settings suitable for SAP projects
        git config --global core.autocrlf false
        git config --global core.longpaths true
        git config --global merge.ff only
        
        # Create Git template for SAP commit messages
        $commitTemplate = @"
# SAP Change Template
# <type>: <short description>
# |<---- Max 50 chars ---->|

# Detailed description

# <type> can be:
# feat:     New feature
# fix:      Bug fix
# docs:     Documentation changes
# style:    Formatting changes
# refactor: Code refactoring
# perf:     Performance improvements
# test:     Adding tests
# chore:    Maintenance tasks
# trans:    Related to transports

# SAP Metadata:
# Transport: 
# Package: 
# Object: 
"@
        
        $commitTemplatePath = "$InstallDir\git-commit-template.txt"
        $commitTemplate | Set-Content -Path $commitTemplatePath
        git config --global commit.template $commitTemplatePath
        
        Log-Message "Git configuration for SAP development completed"
        return $true
    } catch {
        Log-Message "Error configuring Git for SAP: $_" "ERROR"
        return $false
    }
}

# Function to install VSCode extensions for SAP development
function Install-VSCodeExtensions {
    Log-Message "Installing VSCode extensions for SAP development..."
    
    try {
        $extensions = @(
            "SAP.vscode-ui5-language-assistant",
            "SAPSE.vscode-cds",
            "larshp.vscode-abap",
            "marcellourbani.vscode-abap",
            "github.copilot",
            "github.vscode-github-actions",
            "ms-python.python",
            "hookyqr.beautify",
            "dbaeumer.vscode-eslint",
            "mhutchie.git-graph",
            "humao.rest-client"
        )
        
        foreach ($extension in $extensions) {
            code --install-extension $extension
            Log-Message "Installed VSCode extension: $extension"
        }
        
        # Create VSCode workspace settings
        $vscodeFolderPath = "$InstallDir\.vscode"
        Create-DirectoryIfNotExists -Path $vscodeFolderPath
        
        $settingsJson = @"
{
    "abap.lint.activate": true,
    "abap.trace.server": true,
    "files.autoSave": "afterDelay",
    "files.autoSaveDelay": 1000,
    "editor.formatOnSave": true,
    "git.enableSmartCommit": true,
    "git.confirmSync": false,
    "ui5.plugin.addImportStatementOnQuickfix": true,
    "ui5.plugin.jsCodeLens.appendOwnDescription": true,
    "ui5.plugin.xmlCodeLens.appendOwnDescription": true,
    "ui5.validator.enabled": true,
    "ui5.ui5Paths.discoverable": true
}
"@
        
        $settingsPath = "$vscodeFolderPath\settings.json"
        $settingsJson | Set-Content -Path $settingsPath
        
        Log-Message "VSCode configuration for SAP development completed"
        return $true
    } catch {
        Log-Message "Error installing VSCode extensions: $_" "ERROR"
        return $false
    }
}

# Function to create project templates
function Create-ProjectTemplates {
    Log-Message "Setting up project templates..."
    
    try {
        $templatesDir = "$InstallDir\templates"
        Create-DirectoryIfNotExists -Path $templatesDir
        
        # Create SAPUI5/Fiori project template
        $fioriTemplateDir = "$templatesDir\fiori-app"
        Create-DirectoryIfNotExists -Path $fioriTemplateDir
        
        # Basic Fiori app structure
        Create-DirectoryIfNotExists -Path "$fioriTemplateDir\webapp"
        Create-DirectoryIfNotExists -Path "$fioriTemplateDir\webapp\view"
        Create-DirectoryIfNotExists -Path "$fioriTemplateDir\webapp\controller"
        Create-DirectoryIfNotExists -Path "$fioriTemplateDir\webapp\model"
        Create-DirectoryIfNotExists -Path "$fioriTemplateDir\webapp\i18n"
        
        # Create sample manifest.json
        $manifestJson = @"
{
  "sap.app": {
    "id": "com.example.fioriapp",
    "type": "application",
    "title": "{{appTitle}}",
    "description": "{{appDescription}}",
    "applicationVersion": {
      "version": "1.0.0"
    },
    "sourceTemplate": {
      "id": "ui5template.basicSAPUI5ApplicationProject",
      "version": "1.0.0"
    },
    "dataSources": {
      "mainService": {
        "uri": "/sap/opu/odata/sap/ZSERVICE_SRV/",
        "type": "OData",
        "settings": {
          "odataVersion": "2.0",
          "localUri": "localService/metadata.xml"
        }
      }
    }
  },
  "sap.ui": {
    "technology": "UI5",
    "icons": {
      "icon": "",
      "favIcon": "",
      "phone": "",
      "phone@2": "",
      "tablet": "",
      "tablet@2": ""
    },
    "deviceTypes": {
      "desktop": true,
      "tablet": true,
      "phone": true
    }
  },
  "sap.ui5": {
    "rootView": {
      "viewName": "com.example.fioriapp.view.App",
      "type": "XML",
      "async": true,
      "id": "app"
    },
    "dependencies": {
      "minUI5Version": "1.96.0",
      "libs": {
        "sap.ui.core": {},
        "sap.m": {},
        "sap.ui.layout": {}
      }
    },
    "contentDensities": {
      "compact": true,
      "cozy": true
    },
    "models": {
      "i18n": {
        "type": "sap.ui.model.resource.ResourceModel",
        "settings": {
          "bundleName": "com.example.fioriapp.i18n.i18n"
        }
      },
      "": {
        "dataSource": "mainService",
        "preload": true
      }
    },
    "routing": {
      "config": {
        "routerClass": "sap.m.routing.Router",
        "viewType": "XML",
        "viewPath": "com.example.fioriapp.view",
        "controlId": "app",
        "controlAggregation": "pages",
        "async": true
      },
      "routes": [
        {
          "pattern": "",
          "name": "main",
          "target": "main"
        }
      ],
      "targets": {
        "main": {
          "viewName": "Main",
          "viewLevel": 1
        }
      }
    }
  }
}
"@
        
        $manifestPath = "$fioriTemplateDir\webapp\manifest.json"
        $manifestJson | Set-Content -Path $manifestPath
        
        # Create sample Fiori app view
        $mainViewXml = @"
<mvc:View
    controllerName="com.example.fioriapp.controller.Main"
    xmlns:mvc="sap.ui.core.mvc"
    xmlns="sap.m"
    displayBlock="true">
    <Page title="{i18n>title}">
        <content>
            <VBox alignItems="Center" justifyContent="Center" height="100%">
                <Title text="{i18n>welcomeMessage}" class="sapUiLargeMargin"/>
                <Button text="{i18n>showData}" press="onShowData" class="sapUiSmallMargin"/>
                <Table id="idTable" 
                       items="{/EntitySet}"
                       visible="false">
                    <columns>
                        <Column>
                            <Text text="{i18n>columnId}"/>
                        </Column>
                        <Column>
                            <Text text="{i18n>columnName}"/>
                        </Column>
                        <Column>
                            <Text text="{i18n>columnDescription}"/>
                        </Column>
                    </columns>
                    <items>
                        <ColumnListItem>
                            <cells>
                                <Text text="{Id}"/>
                                <Text text="{Name}"/>
                                <Text text="{Description}"/>
                            </cells>
                        </ColumnListItem>
                    </items>
                </Table>
            </VBox>
        </content>
    </Page>
</mvc:View>
"@
        
        $mainViewPath = "$fioriTemplateDir\webapp\view\Main.view.xml"
        $mainViewXml | Set-Content -Path $mainViewPath
        
        # Create sample controller
        $mainControllerJs = @"
sap.ui.define([
    "sap/ui/core/mvc/Controller",
    "sap/m/MessageToast"
], function (Controller, MessageToast) {
    "use strict";

    return Controller.extend("com.example.fioriapp.controller.Main", {
        onInit: function () {
            // Controller initialization logic
        },
        
        onShowData: function () {
            var oTable = this.getView().byId("idTable");
            oTable.setVisible(true);
            MessageToast.show(this.getResourceBundle().getText("dataLoaded"));
        },
        
        getResourceBundle: function () {
            return this.getOwnerComponent().getModel("i18n").getResourceBundle();
        }
    });
});
"@
        
        $mainControllerPath = "$fioriTemplateDir\webapp\controller\Main.controller.js"
        $mainControllerJs | Set-Content -Path $mainControllerPath
        
        # Create sample i18n file
        $i18nProperties = @"
# App Descriptor
appTitle=SAP Fiori Template App
appDescription=A template Fiori application

# App View
title=Fiori Template
welcomeMessage=Welcome to the SAP-GitHub Integration App
showData=Show Data

# Table Columns
columnId=ID
columnName=Name
columnDescription=Description

# Messages
dataLoaded=Data loaded successfully
"@
        
        $i18nPath = "$fioriTemplateDir\webapp\i18n\i18n.properties"
        $i18nProperties | Set-Content -Path $i18nPath
        
        # Create package.json for UI5 tooling
        $packageJson = @"
{
  "name": "fiori-template",
  "version": "1.0.0",
  "description": "SAP Fiori Application Template",
  "private": true,
  "scripts": {
    "start": "ui5 serve --config=ui5.yaml --open index.html",
    "build": "ui5 build --config=ui5.yaml --clean-dest --dest dist",
    "deploy": "npm run build && fiori deploy --config fiori-service.yaml"
  },
  "devDependencies": {
    "@sap/ux-ui5-tooling": "^1.9.0",
    "@ui5/cli": "^2.14.11",
    "@ui5/fs": "^2.0.6",
    "@ui5/logger": "^2.0.1",
    "rimraf": "^3.0.2"
  }
}
"@
        
        $packageJsonPath = "$fioriTemplateDir\package.json"
        $packageJson | Set-Content -Path $packageJsonPath
        
        # Create UI5 tooling configuration
        $ui5YamlContent = @"
specVersion: '2.6'
metadata:
  name: fiori-template
type: application
server:
  customMiddleware:
    - name: fiori-tools-proxy
      afterMiddleware: compression
      configuration:
        backend:
          - path: /sap
            url: https://your-sap-system.example.com
        ui5:
          path:
            - /resources
            - /test-resources
          url: https://ui5.sap.com
          version: 1.96.0
    - name: fiori-tools-appreload
      afterMiddleware: compression
      configuration:
        port: 35729
        path: webapp
"@
        
        $ui5YamlPath = "$fioriTemplateDir\ui5.yaml"
        $ui5YamlContent | Set-Content -Path $ui5YamlPath
        
        # Create CAP project template
        $capTemplateDir = "$templatesDir\cap-app"
        Create-DirectoryIfNotExists -Path $capTemplateDir
        Create-DirectoryIfNotExists -Path "$capTemplateDir\db"
        Create-DirectoryIfNotExists -Path "$capTemplateDir\srv"
        Create-DirectoryIfNotExists -Path "$capTemplateDir\app"
        
        # Create sample package.json for CAP
        $capPackageJson = @"
{
  "name": "cap-template",
  "version": "1.0.0",
  "description": "CAP Application Template",
  "repository": "<Add your repository here>",
  "license": "UNLICENSED",
  "private": true,
  "dependencies": {
    "@sap/cds": "^6.1.0",
    "@sap/cds-odata-v2-adapter-proxy": "^1.9.0",
    "express": "^4.18.1",
    "hdb": "^0.19.0"
  },
  "devDependencies": {
    "@sap/ux-specification": "^1.102.14",
    "sqlite3": "^5.0.2"
  },
  "scripts": {
    "start": "cds run",
    "watch": "cds watch",
    "build": "cds build/all"
  },
  "cds": {
    "requires": {
      "db": {
        "kind": "sql"
      }
    }
  }
}
"@
        
        $capPackageJsonPath = "$capTemplateDir\package.json"
        $capPackageJson | Set-Content -Path $capPackageJsonPath
        
        Log-Message "Project templates created successfully"
        return $true
    } catch {
        Log-Message "Error creating project templates: $_" "ERROR"
        return $false
    }
}

# Main script execution

# Create installation directory
Create-DirectoryIfNotExists -Path $InstallDir
Log-Message "Starting development environment setup in $InstallDir"

# Install Git client
$success = $true
if ($GitClient) {
    $gitSuccess = Install-WithChocolatey -PackageName "git" -Version $GitVersion
    if ($gitSuccess) {
        $configSuccess = Configure-GitForSap
        $success = $success -and $configSuccess
    } else {
        $success = $false
    }
}

# Install SAP GUI (this would typically point to your organization's software repository)
if ($SapGui) {
    Log-Message "SAP GUI should be installed manually from your organization's software distribution"
    Log-Message "Please download SAP GUI $SapGuiVersion from the SAP Software Download Center"
}

# Install VSCode
if ($VSCode) {
    $vscodeSuccess = Install-WithChocolatey -PackageName "vscode" -Version $VSCodeVersion
    if ($vscodeSuccess) {
        $extensionsSuccess = Install-VSCodeExtensions
        $success = $success -and $extensionsSuccess
    } else {
        $success = $false
    }
}

# Install Node.js
if ($NodeJs) {
    $nodeSuccess = Install-WithChocolatey -PackageName "nodejs-lts" -Version $NodeJsVersion
    $success = $success -and $nodeSuccess
}

# Install Python
if ($Python) {
    $pythonSuccess = Install-WithChocolatey -PackageName "python" -Version $PythonVersion
    $success = $success -and $pythonSuccess
}

# Install SAP Cloud SDK/CLI
if ($SapCloud) {
    # Install SAP Cloud Application Programming Model
    try {
        Log-Message "Installing SAP Cloud Application Programming Model (CAP)"
        npm install -g @sap/cds-dk
        Log-Message "SAP CAP installed successfully"
        
        Log-Message "Installing SAP Fiori Tools"
        npm install -g @sap/ux-ui5-tooling fiori-tools
        Log-Message "SAP Fiori Tools installed successfully"
    } catch {
        Log-Message "Error installing SAP Cloud tools: $_" "ERROR"
        $success = $false
    }
}

# Create project templates
$templatesSuccess = Create-ProjectTemplates
$success = $success -and $templatesSuccess

# Generate config file
Generate-ConfigFile

# Report status
if ($success) {
    Log-Message "Development environment setup completed successfully!"
    Log-Message "Installation directory: $InstallDir"
    Log-Message "Templates directory: $InstallDir\templates"
} else {
    Log-Message "Development environment setup completed with some issues. Check the log for details." "WARNING"
}

Log-Message "Setup script execution completed" 