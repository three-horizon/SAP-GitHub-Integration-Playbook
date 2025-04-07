/**
 * SVG Validation Script
 * 
 * This script validates SVG files in the repository to ensure they follow
 * standard formatting and can be properly displayed across platforms.
 */

const fs = require('fs');
const path = require('path');
const glob = require('glob');
const chalk = require('chalk');
const { DOMParser } = require('xmldom');

// Configuration
const SVG_PATHS = [
  './assets/images/**/*.svg',
];

// SVG validation rules
const VALIDATION_RULES = [
  {
    name: 'Has viewBox attribute',
    validator: (doc) => doc.documentElement.hasAttribute('viewBox'),
    severity: 'error'
  },
  {
    name: 'Has width and height',
    validator: (doc) => 
      doc.documentElement.hasAttribute('width') && 
      doc.documentElement.hasAttribute('height'),
    severity: 'warning'
  },
  {
    name: 'SVG namespace defined',
    validator: (doc) => 
      doc.documentElement.namespaceURI === 'http://www.w3.org/2000/svg',
    severity: 'error'
  }
];

// Counters for summary
let totalFiles = 0;
let validFiles = 0;
let filesWithWarnings = 0;
let filesWithErrors = 0;

/**
 * Validate an SVG file
 * @param {string} filePath - Path to the SVG file
 * @returns {Object} Validation results
 */
function validateSvg(filePath) {
  const content = fs.readFileSync(filePath, 'utf8');
  const results = {
    filePath,
    warnings: [],
    errors: []
  };

  try {
    // Use xmldom to parse the SVG
    const parser = new DOMParser({
      errorHandler: {
        warning: (msg) => results.warnings.push(`XML parsing warning: ${msg}`),
        error: (msg) => results.errors.push(`XML parsing error: ${msg}`),
        fatalError: (msg) => results.errors.push(`XML fatal error: ${msg}`)
      }
    });
    
    const doc = parser.parseFromString(content, 'image/svg+xml');
    
    // Apply validation rules
    VALIDATION_RULES.forEach(rule => {
      try {
        if (!rule.validator(doc)) {
          if (rule.severity === 'error') {
            results.errors.push(`Failed rule: ${rule.name}`);
          } else {
            results.warnings.push(`Failed rule: ${rule.name}`);
          }
        }
      } catch (ruleError) {
        results.errors.push(`Error applying rule '${rule.name}': ${ruleError.message}`);
      }
    });

  } catch (e) {
    results.errors.push(`Failed to parse SVG: ${e.message}`);
  }

  return results;
}

/**
 * Main execution function
 */
function main() {
  console.log(chalk.blue.bold('SAP-GitHub Integration Playbook - SVG Validation'));
  console.log(chalk.blue('======================================================'));
  
  // Get all SVG files
  let svgFiles = [];
  SVG_PATHS.forEach(pattern => {
    const files = glob.sync(pattern);
    svgFiles = [...svgFiles, ...files];
  });

  totalFiles = svgFiles.length;
  console.log(chalk.blue(`Found ${totalFiles} SVG files to validate\n`));

  if (totalFiles === 0) {
    console.log(chalk.yellow('No SVG files found. Check the paths in the script configuration.'));
    return;
  }

  // Validate each SVG file
  svgFiles.forEach(file => {
    const results = validateSvg(file);
    
    if (results.errors.length > 0) {
      filesWithErrors++;
      console.log(chalk.red(`✖ ${file}`));
      results.errors.forEach(error => {
        console.log(chalk.red(`  - ${error}`));
      });
      
      if (results.warnings.length > 0) {
        results.warnings.forEach(warning => {
          console.log(chalk.yellow(`  - ${warning}`));
        });
      }
      console.log('');
    } else if (results.warnings.length > 0) {
      filesWithWarnings++;
      console.log(chalk.yellow(`⚠ ${file}`));
      results.warnings.forEach(warning => {
        console.log(chalk.yellow(`  - ${warning}`));
      });
      console.log('');
    } else {
      validFiles++;
    }
  });

  // Print summary
  console.log(chalk.blue('======================================================'));
  console.log(chalk.blue.bold('Validation Summary:'));
  console.log(chalk.blue(`Total SVG files: ${totalFiles}`));
  console.log(chalk.green(`Valid files: ${validFiles}`));
  console.log(chalk.yellow(`Files with warnings: ${filesWithWarnings}`));
  console.log(chalk.red(`Files with errors: ${filesWithErrors}`));
  console.log(chalk.blue('======================================================'));
  
  if (filesWithErrors > 0) {
    console.log(chalk.red.bold('SVG validation found errors that need to be fixed.'));
    process.exit(1);
  } else if (filesWithWarnings > 0) {
    console.log(chalk.yellow.bold('SVG validation completed with warnings. Consider addressing them for better compatibility.'));
    process.exit(0);
  } else {
    console.log(chalk.green.bold('All SVG files passed validation!'));
    process.exit(0);
  }
}

// Execute the script
main();