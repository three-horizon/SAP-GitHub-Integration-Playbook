#!/usr/bin/env node
/**
 * SAP-GitHub Integration - ABAP Code Quality Validator
 * 
 * This script analyzes ABAP code for quality issues and coding standards compliance.
 * It can be integrated into CI/CD pipelines to enforce quality gates.
 * 
 * Usage:
 *   node abap-quality-check.js --source=<directory> --output=<report-file>
 *                             [--rules=<rules-config>] [--threshold=<score>]
 *                             [--format=<json|html|text>] [--detail-level=<1-3>]
 * 
 * Author: SAP-GitHub Integration Team
 * Version: 1.0.0
 */

'use strict';

const fs = require('fs');
const path = require('path');
const util = require('util');
const { program } = require('commander');
const chalk = require('chalk');

// Default rules for ABAP code quality
const DEFAULT_RULES = {
  // Naming conventions
  "naming": {
    "class_prefix": {
      "pattern": "^Z[A-Z0-9_]+$",
      "message": "Class names should start with 'Z' followed by uppercase letters",
      "severity": "error"
    },
    "method_naming": {
      "pattern": "^[A-Z][A-Z0-9_]*$",
      "message": "Method names should be in uppercase with underscores",
      "severity": "warning"
    },
    "variable_naming": {
      "pattern": "^[GLT][VSTX]_[A-Z0-9_]+$",
      "message": "Variable names should follow SAP naming convention (e.g., LV_, GT_, etc.)",
      "severity": "warning"
    }
  },
  
  // Code structure
  "structure": {
    "method_length": {
      "max_lines": 100,
      "message": "Methods should not exceed {max_lines} lines",
      "severity": "warning"
    },
    "class_length": {
      "max_lines": 1000,
      "message": "Classes should not exceed {max_lines} lines",
      "severity": "warning"
    },
    "nesting_depth": {
      "max_depth": 4,
      "message": "Nesting depth should not exceed {max_depth} levels",
      "severity": "warning"
    }
  },
  
  // Obsolete statements
  "obsolete": {
    "forbidden_statements": {
      "patterns": ["REFRESH", "ADD", "SUBTRACT", "MULTIPLY", "DIVIDE", "COMPUTE", "MOVE"],
      "message": "Avoid using obsolete statements like {token}",
      "severity": "warning"
    }
  },
  
  // Documentation
  "documentation": {
    "class_documentation": {
      "required": true,
      "message": "Classes should have documentation",
      "severity": "warning"
    },
    "method_documentation": {
      "required": true,
      "message": "Public methods should have documentation",
      "severity": "warning"
    }
  },
  
  // Security
  "security": {
    "sql_injection": {
      "patterns": ["INTO\\s+TABLE.*WHERE.*'.*'", "SELECT.*WHERE.*=\\s*'"],
      "message": "Potential SQL injection detected",
      "severity": "error"
    },
    "direct_authority_check": {
      "patterns": ["AUTHORITY-CHECK"],
      "message": "Direct authority checks should be encapsulated in methods",
      "severity": "warning"
    }
  }
};

// Initialize command line arguments
program
  .version('1.0.0')
  .description('ABAP Code Quality Validator')
  .requiredOption('--source <directory>', 'Source directory containing ABAP code')
  .option('--output <file>', 'Output file for the report', 'abap-quality-report.json')
  .option('--rules <file>', 'JSON file with rules configuration')
  .option('--threshold <score>', 'Quality threshold (0-100)', '80')
  .option('--format <format>', 'Output format (json, html, text)', 'json')
  .option('--detail-level <level>', 'Detail level (1-3)', '2')
  .parse(process.argv);

const options = program.opts();
const sourceDir = options.source;
const outputFile = options.output;
const threshold = parseInt(options.threshold, 10);
const format = options.format;
const detailLevel = parseInt(options.detailLevel, 10);

// Statistics for quality tracking
const stats = {
  totalFiles: 0,
  validFiles: 0,
  totalIssues: 0,
  errorCount: 0,
  warningCount: 0,
  qualityScore: 0,
  startTime: Date.now(),
  endTime: 0,
  executionTime: 0,
  fileDetails: []
};

/**
 * Load rules from file or use defaults
 * @returns {Object} Rules configuration
 */
function loadRules() {
  if (options.rules) {
    try {
      const rulesContent = fs.readFileSync(options.rules, 'utf8');
      return JSON.parse(rulesContent);
    } catch (error) {
      console.error(chalk.red(`Error loading rules file: ${error.message}`));
      console.log(chalk.yellow('Using default rules instead'));
      return DEFAULT_RULES;
    }
  }
  return DEFAULT_RULES;
}

/**
 * Find all ABAP files in a directory recursively
 * @param {string} dir Directory to search
 * @returns {string[]} List of file paths
 */
function findAbapFiles(dir) {
  const files = [];
  
  function traverseDir(currentDir) {
    const entries = fs.readdirSync(currentDir, { withFileTypes: true });
    
    for (const entry of entries) {
      const fullPath = path.join(currentDir, entry.name);
      
      if (entry.isDirectory()) {
        traverseDir(fullPath);
      } else if (entry.isFile() && isAbapFile(entry.name)) {
        files.push(fullPath);
      }
    }
  }
  
  traverseDir(dir);
  return files;
}

/**
 * Check if a file is an ABAP file based on extension
 * @param {string} filename Filename to check
 * @returns {boolean} True if the file is an ABAP file
 */
function isAbapFile(filename) {
  const ext = path.extname(filename).toLowerCase();
  return ext === '.abap' || 
         filename.indexOf('.clas.') > -1 || 
         filename.indexOf('.prog.') > -1 || 
         filename.indexOf('.fugr.') > -1 || 
         filename.indexOf('.intf.') > -1;
}

/**
 * Check a file against quality rules
 * @param {string} filePath Path to the file
 * @param {Object} rules Rules to check against
 * @returns {Object} Check results
 */
function checkFile(filePath, rules) {
  console.log(chalk.blue(`Checking file: ${filePath}`));
  
  const result = {
    file: filePath,
    relativeFile: path.relative(sourceDir, filePath),
    issues: [],
    errorCount: 0,
    warningCount: 0,
    linesOfCode: 0
  };
  
  try {
    const content = fs.readFileSync(filePath, 'utf8');
    const lines = content.split('\n');
    result.linesOfCode = lines.length;
    
    // Check naming conventions
    if (rules.naming) {
      checkNamingConventions(content, rules.naming, result);
    }
    
    // Check structure
    if (rules.structure) {
      checkStructure(content, lines, rules.structure, result);
    }
    
    // Check for obsolete statements
    if (rules.obsolete) {
      checkObsoleteStatements(content, rules.obsolete, result);
    }
    
    // Check documentation
    if (rules.documentation) {
      checkDocumentation(content, rules.documentation, result);
    }
    
    // Check security
    if (rules.security) {
      checkSecurity(content, rules.security, result);
    }
    
    return result;
  } catch (error) {
    console.error(chalk.red(`Error checking file ${filePath}: ${error.message}`));
    result.issues.push({
      line: 0,
      column: 0,
      message: `File could not be processed: ${error.message}`,
      severity: 'error',
      rule: 'file-access'
    });
    result.errorCount++;
    return result;
  }
}

/**
 * Check naming conventions in the file
 * @param {string} content File content
 * @param {Object} rules Naming rules
 * @param {Object} result Result object to update
 */
function checkNamingConventions(content, rules, result) {
  // Check class naming
  if (rules.class_prefix) {
    const classMatches = content.match(/CLASS\s+(\w+)\s+DEFINITION/g);
    if (classMatches) {
      for (const match of classMatches) {
        const className = match.replace(/CLASS\s+(\w+)\s+DEFINITION.*/, '$1').trim();
        const regex = new RegExp(rules.class_prefix.pattern);
        
        if (!regex.test(className)) {
          addIssue(result, {
            line: findLineNumber(content, match),
            column: 1,
            message: `${rules.class_prefix.message}: ${className}`,
            severity: rules.class_prefix.severity,
            rule: 'naming.class_prefix'
          });
        }
      }
    }
  }
  
  // Check method naming
  if (rules.method_naming) {
    const methodMatches = content.match(/METHOD\s+(\w+)\s*\./g);
    if (methodMatches) {
      for (const match of methodMatches) {
        const methodName = match.replace(/METHOD\s+(\w+)\s*\..*/, '$1').trim();
        const regex = new RegExp(rules.method_naming.pattern);
        
        if (!regex.test(methodName)) {
          addIssue(result, {
            line: findLineNumber(content, match),
            column: 1,
            message: `${rules.method_naming.message}: ${methodName}`,
            severity: rules.method_naming.severity,
            rule: 'naming.method_naming'
          });
        }
      }
    }
  }
  
  // Check variable naming
  if (rules.variable_naming) {
    const variableMatches = content.match(/DATA:?\s+(\w+)[\s,]/g);
    if (variableMatches) {
      for (const match of variableMatches) {
        const variableName = match.replace(/DATA:?\s+(\w+)[\s,].*/, '$1').trim();
        const regex = new RegExp(rules.variable_naming.pattern);
        
        if (!regex.test(variableName)) {
          addIssue(result, {
            line: findLineNumber(content, match),
            column: 1,
            message: `${rules.variable_naming.message}: ${variableName}`,
            severity: rules.variable_naming.severity,
            rule: 'naming.variable_naming'
          });
        }
      }
    }
  }
}

/**
 * Check code structure issues
 * @param {string} content File content
 * @param {string[]} lines File content as lines
 * @param {Object} rules Structure rules
 * @param {Object} result Result object to update
 */
function checkStructure(content, lines, rules, result) {
  // Check method length
  if (rules.method_length) {
    const methodStartPattern = /METHOD\s+(\w+)/g;
    const methodEndPattern = /ENDMETHOD/g;
    
    let methodStart = methodStartPattern.exec(content);
    while (methodStart) {
      const methodName = methodStart[1];
      const startPos = methodStart.index;
      const startLine = findLineNumber(content, content.substring(startPos, startPos + methodStart[0].length));
      
      // Find the matching ENDMETHOD
      let methodEndStr = 'ENDMETHOD';
      let endPos = content.indexOf(methodEndStr, startPos);
      if (endPos === -1) {
        // Try with a dot
        methodEndStr = 'ENDMETHOD.';
        endPos = content.indexOf(methodEndStr, startPos);
      }
      
      if (endPos !== -1) {
        const endLine = findLineNumber(content, content.substring(endPos, endPos + methodEndStr.length));
        const methodLength = endLine - startLine;
        
        if (methodLength > rules.method_length.max_lines) {
          addIssue(result, {
            line: startLine,
            column: 1,
            message: rules.method_length.message.replace('{max_lines}', rules.method_length.max_lines) +
                     ` Method '${methodName}' has ${methodLength} lines.`,
            severity: rules.method_length.severity,
            rule: 'structure.method_length'
          });
        }
      }
      
      methodStart = methodStartPattern.exec(content);
    }
  }
  
  // Check class length
  if (rules.class_length && result.linesOfCode > rules.class_length.max_lines) {
    addIssue(result, {
      line: 1,
      column: 1,
      message: rules.class_length.message.replace('{max_lines}', rules.class_length.max_lines) +
               ` This file has ${result.linesOfCode} lines.`,
      severity: rules.class_length.severity,
      rule: 'structure.class_length'
    });
  }
  
  // Check nesting depth
  if (rules.nesting_depth) {
    const nestingKeywords = ['IF', 'LOOP', 'DO', 'WHILE', 'CASE', 'TRY'];
    let currentDepth = 0;
    let maxDepth = 0;
    let depthByLine = [];
    
    for (let i = 0; i < lines.length; i++) {
      const line = lines[i].trim().toUpperCase();
      
      // Increase depth for nesting keywords
      for (const keyword of nestingKeywords) {
        if (line.startsWith(keyword + ' ') || line === keyword) {
          currentDepth++;
          break;
        }
      }
      
      // Store current depth for this line
      depthByLine[i] = currentDepth;
      if (currentDepth > maxDepth) {
        maxDepth = currentDepth;
      }
      
      // Decrease depth for ending keywords
      if (line.startsWith('END') || line.startsWith('ENDIF') || 
          line.startsWith('ENDLOOP') || line.startsWith('ENDTRY') ||
          line.startsWith('ENDCASE') || line.startsWith('ENDWHILE') ||
          line.startsWith('ENDDO')) {
        currentDepth--;
      }
    }
    
    if (maxDepth > rules.nesting_depth.max_depth) {
      // Find the first occurrence of the maximum depth
      let maxDepthLine = 0;
      for (let i = 0; i < depthByLine.length; i++) {
        if (depthByLine[i] === maxDepth) {
          maxDepthLine = i + 1; // Lines are 1-based
          break;
        }
      }
      
      addIssue(result, {
        line: maxDepthLine,
        column: 1,
        message: rules.nesting_depth.message.replace('{max_depth}', rules.nesting_depth.max_depth) +
                 ` Found nesting depth of ${maxDepth}.`,
        severity: rules.nesting_depth.severity,
        rule: 'structure.nesting_depth'
      });
    }
  }
}

/**
 * Check for obsolete ABAP statements
 * @param {string} content File content
 * @param {Object} rules Obsolete statement rules
 * @param {Object} result Result object to update
 */
function checkObsoleteStatements(content, rules, result) {
  if (rules.forbidden_statements) {
    const patterns = rules.forbidden_statements.patterns;
    
    for (const statement of patterns) {
      const regex = new RegExp(`\\b${statement}\\b`, 'g');
      let match;
      
      while ((match = regex.exec(content)) !== null) {
        addIssue(result, {
          line: findLineNumber(content, match[0], match.index),
          column: 1,
          message: rules.forbidden_statements.message.replace('{token}', match[0]),
          severity: rules.forbidden_statements.severity,
          rule: 'obsolete.forbidden_statements'
        });
      }
    }
  }
}

/**
 * Check documentation completeness
 * @param {string} content File content
 * @param {Object} rules Documentation rules
 * @param {Object} result Result object to update
 */
function checkDocumentation(content, rules, result) {
  // Check class documentation
  if (rules.class_documentation && rules.class_documentation.required) {
    const classMatches = content.match(/CLASS\s+(\w+)\s+DEFINITION/g);
    if (classMatches) {
      for (const match of classMatches) {
        const className = match.replace(/CLASS\s+(\w+)\s+DEFINITION.*/, '$1').trim();
        const classPos = content.indexOf(match);
        
        // Check if there's a comment block before the class definition
        const prevContent = content.substring(0, classPos).trim();
        const hasDocumentation = prevContent.endsWith('*/') || 
                                 content.substring(classPos - 100, classPos).includes('"') ||
                                 content.substring(classPos - 100, classPos).includes('*');
        
        if (!hasDocumentation) {
          addIssue(result, {
            line: findLineNumber(content, match),
            column: 1,
            message: `${rules.class_documentation.message}: ${className}`,
            severity: rules.class_documentation.severity,
            rule: 'documentation.class_documentation'
          });
        }
      }
    }
  }
  
  // Check method documentation
  if (rules.method_documentation && rules.method_documentation.required) {
    const methodMatches = content.match(/^\s*METHOD\s+(\w+)\s*\./gm);
    if (methodMatches) {
      for (const match of methodMatches) {
        const methodName = match.replace(/^\s*METHOD\s+(\w+)\s*\..*/, '$1').trim();
        const methodPos = content.indexOf(match);
        
        // Check if there's a comment block before the method
        const prevContent = content.substring(Math.max(0, methodPos - 100), methodPos).trim();
        const hasDocumentation = prevContent.endsWith('*/') || 
                                 prevContent.includes('"') ||
                                 prevContent.includes('*');
        
        if (!hasDocumentation) {
          addIssue(result, {
            line: findLineNumber(content, match),
            column: 1,
            message: `${rules.method_documentation.message}: ${methodName}`,
            severity: rules.method_documentation.severity,
            rule: 'documentation.method_documentation'
          });
        }
      }
    }
  }
}

/**
 * Check for security issues
 * @param {string} content File content
 * @param {Object} rules Security rules
 * @param {Object} result Result object to update
 */
function checkSecurity(content, rules, result) {
  // Check for SQL injection
  if (rules.sql_injection) {
    for (const pattern of rules.sql_injection.patterns) {
      const regex = new RegExp(pattern, 'gi');
      let match;
      
      while ((match = regex.exec(content)) !== null) {
        addIssue(result, {
          line: findLineNumber(content, match[0], match.index),
          column: 1,
          message: rules.sql_injection.message,
          severity: rules.sql_injection.severity,
          rule: 'security.sql_injection'
        });
      }
    }
  }
  
  // Check for direct authority checks
  if (rules.direct_authority_check) {
    for (const pattern of rules.direct_authority_check.patterns) {
      const regex = new RegExp(`\\b${pattern}\\b`, 'g');
      let match;
      
      while ((match = regex.exec(content)) !== null) {
        addIssue(result, {
          line: findLineNumber(content, match[0], match.index),
          column: 1,
          message: rules.direct_authority_check.message,
          severity: rules.direct_authority_check.severity,
          rule: 'security.direct_authority_check'
        });
      }
    }
  }
}

/**
 * Add an issue to the result object
 * @param {Object} result Result object
 * @param {Object} issue Issue to add
 */
function addIssue(result, issue) {
  result.issues.push(issue);
  
  if (issue.severity === 'error') {
    result.errorCount++;
  } else if (issue.severity === 'warning') {
    result.warningCount++;
  }
}

/**
 * Find the line number for a given string in the content
 * @param {string} content File content
 * @param {string} searchString String to search for
 * @param {number} startIndex Optional start index
 * @returns {number} Line number (1-based)
 */
function findLineNumber(content, searchString, startIndex = 0) {
  const index = startIndex || content.indexOf(searchString);
  if (index === -1) return 1;
  
  const lines = content.substring(0, index).split('\n');
  return lines.length;
}

/**
 * Calculate the quality score based on issues
 * @param {Object} fileStats File statistics
 * @returns {number} Quality score (0-100)
 */
function calculateQualityScore(fileStats) {
  if (fileStats.totalFiles === 0) return 0;
  
  // Each error reduces the score more than warnings
  const errorPenalty = 5;
  const warningPenalty = 1;
  
  // Calculate base score (100 points max)
  let score = 100;
  
  // Apply penalties for errors and warnings
  const errorPenaltyTotal = Math.min(fileStats.errorCount * errorPenalty, 100);
  const warningPenaltyTotal = Math.min(fileStats.warningCount * warningPenalty, 50);
  
  score -= errorPenaltyTotal;
  score -= warningPenaltyTotal;
  
  return Math.max(0, Math.round(score));
}

/**
 * Generate the report in the specified format
 * @param {Object} stats Analysis statistics
 * @param {string} format Output format
 * @returns {string} Formatted report
 */
function generateReport(stats, format) {
  switch (format.toLowerCase()) {
    case 'json':
      return JSON.stringify(stats, null, 2);
      
    case 'html':
      return generateHtmlReport(stats);
      
    case 'text':
    default:
      return generateTextReport(stats);
  }
}

/**
 * Generate a text report
 * @param {Object} stats Analysis statistics
 * @returns {string} Text report
 */
function generateTextReport(stats) {
  let report = '';
  
  report += '======================================\n';
  report += 'ABAP Code Quality Analysis Report\n';
  report += '======================================\n\n';
  
  report += `Analysis Date: ${new Date().toISOString()}\n`;
  report += `Execution Time: ${stats.executionTime.toFixed(2)} seconds\n\n`;
  
  report += 'Summary:\n';
  report += `---------\n`;
  report += `Files Analyzed: ${stats.totalFiles}\n`;
  report += `Total Issues: ${stats.totalIssues}\n`;
  report += `  - Errors: ${stats.errorCount}\n`;
  report += `  - Warnings: ${stats.warningCount}\n`;
  report += `Quality Score: ${stats.qualityScore}/100\n\n`;
  
  report += `Quality Result: ${stats.qualityScore >= threshold ? 'PASS' : 'FAIL'}\n`;
  report += `Quality Threshold: ${threshold}\n\n`;
  
  // Add file details if requested
  if (detailLevel >= 2) {
    report += 'Files with issues:\n';
    report += '-----------------\n';
    
    for (const file of stats.fileDetails) {
      if (file.issues.length > 0) {
        report += `\n${file.relativeFile}\n`;
        report += '  Issues: ' + file.issues.length + ' (Errors: ' + file.errorCount + 
                ', Warnings: ' + file.warningCount + ')\n';
        
        if (detailLevel >= 3) {
          for (const issue of file.issues) {
            const severity = issue.severity === 'error' ? 'ERROR' : 'WARNING';
            report += `  [${severity}] Line ${issue.line}: ${issue.message}\n`;
          }
        }
      }
    }
  }
  
  return report;
}

/**
 * Generate an HTML report
 * @param {Object} stats Analysis statistics
 * @returns {string} HTML report
 */
function generateHtmlReport(stats) {
  const passClass = stats.qualityScore >= threshold ? 'pass' : 'fail';
  
  let html = `
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>ABAP Code Quality Report</title>
    <style>
        body { font-family: Arial, sans-serif; margin: 0; padding: 20px; color: #333; }
        .header { border-bottom: 1px solid #ccc; padding-bottom: 10px; margin-bottom: 20px; }
        .summary { background-color: #f8f8f8; padding: 15px; border-radius: 5px; margin-bottom: 20px; }
        .pass { color: green; }
        .fail { color: red; }
        .files { margin-top: 30px; }
        .file { margin-bottom: 15px; padding: 10px; background-color: #f8f8f8; border-radius: 5px; }
        .file-header { cursor: pointer; }
        .file-content { display: none; padding-top: 10px; }
        .error { color: red; }
        .warning { color: orange; }
        table { border-collapse: collapse; width: 100%; }
        th, td { text-align: left; padding: 8px; border-bottom: 1px solid #ddd; }
        th { background-color: #f2f2f2; }
    </style>
</head>
<body>
    <div class="header">
        <h1>ABAP Code Quality Report</h1>
        <p>Analysis Date: ${new Date().toISOString()}</p>
        <p>Execution Time: ${stats.executionTime.toFixed(2)} seconds</p>
    </div>
    
    <div class="summary">
        <h2>Summary</h2>
        <table>
            <tr><td>Files Analyzed:</td><td>${stats.totalFiles}</td></tr>
            <tr><td>Total Issues:</td><td>${stats.totalIssues}</td></tr>
            <tr><td>Errors:</td><td>${stats.errorCount}</td></tr>
            <tr><td>Warnings:</td><td>${stats.warningCount}</td></tr>
            <tr><td>Quality Score:</td><td>${stats.qualityScore}/100</td></tr>
            <tr><td>Quality Threshold:</td><td>${threshold}</td></tr>
            <tr><td>Result:</td><td class="${passClass}">${stats.qualityScore >= threshold ? 'PASS' : 'FAIL'}</td></tr>
        </table>
    </div>
    
    <div class="files">
        <h2>Files with Issues</h2>`;
    
  for (const file of stats.fileDetails) {
    if (file.issues.length > 0) {
      html += `
        <div class="file">
            <div class="file-header" onclick="toggleFile('${file.relativeFile.replace(/'/g, "\\'")}')">
                <strong>${file.relativeFile}</strong> - 
                Issues: ${file.issues.length} (Errors: ${file.errorCount}, Warnings: ${file.warningCount})
            </div>
            <div id="${file.relativeFile.replace(/[\\/\\.]/g, '_')}" class="file-content">
                <table>
                    <tr>
                        <th>Severity</th>
                        <th>Line</th>
                        <th>Message</th>
                        <th>Rule</th>
                    </tr>`;
      
      for (const issue of file.issues) {
        html += `
                    <tr>
                        <td class="${issue.severity}">${issue.severity.toUpperCase()}</td>
                        <td>${issue.line}</td>
                        <td>${issue.message}</td>
                        <td>${issue.rule}</td>
                    </tr>`;
      }
      
      html += `
                </table>
            </div>
        </div>`;
    }
  }
    
  html += `
    </div>
    
    <script>
        function toggleFile(fileId) {
            const element = document.getElementById(fileId.replace(/[\\/\\.]/g, '_'));
            element.style.display = element.style.display === 'block' ? 'none' : 'block';
        }
    </script>
</body>
</html>`;
  
  return html;
}

/**
 * Main function
 */
async function main() {
  try {
    // Check if source directory exists
    if (!fs.existsSync(sourceDir)) {
      console.error(chalk.red(`Source directory not found: ${sourceDir}`));
      process.exit(1);
    }
    
    // Load rules
    const rules = loadRules();
    console.log(chalk.green(`Rules loaded successfully`));
    
    // Find ABAP files
    const files = findAbapFiles(sourceDir);
    console.log(chalk.green(`Found ${files.length} ABAP files to analyze`));
    
    stats.totalFiles = files.length;
    
    // Process each file
    for (const file of files) {
      const result = checkFile(file, rules);
      stats.fileDetails.push(result);
      stats.validFiles++;
      
      stats.totalIssues += result.issues.length;
      stats.errorCount += result.errorCount;
      stats.warningCount += result.warningCount;
    }
    
    // Calculate final statistics
    stats.endTime = Date.now();
    stats.executionTime = (stats.endTime - stats.startTime) / 1000;
    stats.qualityScore = calculateQualityScore(stats);
    
    // Generate and save the report
    const report = generateReport(stats, format);
    fs.writeFileSync(outputFile, report);
    
    console.log(chalk.green(`\nAnalysis complete!`));
    console.log(`Files analyzed: ${chalk.bold(stats.totalFiles)}`);
    console.log(`Issues found: ${chalk.bold(stats.totalIssues)} (${chalk.red(stats.errorCount)} errors, ${chalk.yellow(stats.warningCount)} warnings)`);
    console.log(`Quality score: ${chalk.bold(stats.qualityScore)}/100 (threshold: ${threshold})`);
    console.log(`Report saved to: ${chalk.bold(outputFile)}`);
    
    if (stats.qualityScore < threshold) {
      console.log(chalk.red(`\nQuality check failed. Score ${stats.qualityScore} is below threshold ${threshold}.`));
      process.exit(1);
    } else {
      console.log(chalk.green(`\nQuality check passed. Score ${stats.qualityScore} meets threshold ${threshold}.`));
    }
  } catch (error) {
    console.error(chalk.red(`Error: ${error.message}`));
    console.error(error.stack);
    process.exit(1);
  }
}

// Run the main function
main(); 