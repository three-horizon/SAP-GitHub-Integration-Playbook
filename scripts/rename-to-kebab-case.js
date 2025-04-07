#!/usr/bin/env node

/**
 * rename-to-kebab-case.js
 * 
 * This script renames all markdown files to kebab-case format.
 * 
 * Usage: node rename-to-kebab-case.js [directory]
 * 
 * If no directory is specified, it processes all files in the docs directory.
 */

const fs = require('fs');
const path = require('path');
const util = require('util');
const readdir = util.promisify(fs.readdir);
const stat = util.promisify(fs.stat);
const rename = util.promisify(fs.rename);
const exec = util.promisify(require('child_process').exec);

// Colors for console output
const colors = {
  reset: '\x1b[0m',
  red: '\x1b[31m',
  green: '\x1b[32m',
  yellow: '\x1b[33m',
  blue: '\x1b[34m'
};

// Convert to kebab-case
function toKebabCase(str) {
  return str
    .toLowerCase()
    .replace(/([a-z])([A-Z])/g, '$1-$2') // Convert camelCase to kebab-case
    .replace(/[\s_]+/g, '-') // Convert spaces and underscores to hyphens
    .replace(/[^a-z0-9-]/g, '') // Remove all non-alphanumeric characters except hyphens
    .replace(/-+/g, '-') // Replace multiple hyphens with a single one
    .replace(/^-|-$/g, ''); // Remove leading and trailing hyphens
}

// Main function
async function main() {
  try {
    const targetDir = process.argv[2] || 'docs';
    console.log(`${colors.blue}Processing files in ${targetDir}${colors.reset}`);
    
    const files = await getAllMarkdownFiles(targetDir);
    console.log(`${colors.blue}Found ${files.length} Markdown files${colors.reset}`);
    
    let renamed = 0;
    for (const file of files) {
      if (await renameFile(file)) {
        renamed++;
      }
    }
    
    console.log(`${colors.green}Completed! Renamed ${renamed} files.${colors.reset}`);
  } catch (err) {
    console.error(`${colors.red}Error: ${err.message}${colors.reset}`);
    process.exit(1);
  }
}

// Get all markdown files recursively
async function getAllMarkdownFiles(dir) {
  const files = [];
  
  async function traverse(currentDir) {
    const entries = await readdir(currentDir, { withFileTypes: true });
    
    for (const entry of entries) {
      const fullPath = path.join(currentDir, entry.name);
      
      if (entry.isDirectory()) {
        // Skip .git directory
        if (entry.name !== '.git') {
          await traverse(fullPath);
        }
      } else if (entry.isFile() && entry.name.endsWith('.md')) {
        // Skip README.md and index.md files
        if (entry.name !== 'README.md' && entry.name !== 'index.md') {
          files.push(fullPath);
        }
      }
    }
  }
  
  await traverse(dir);
  return files;
}

// Rename file to kebab-case
async function renameFile(filePath) {
  const dir = path.dirname(filePath);
  const oldName = path.basename(filePath);
  const baseName = path.basename(filePath, '.md');
  
  // Skip if file is already kebab-case
  if (baseName === toKebabCase(baseName)) {
    console.log(`${colors.yellow}Skipping ${filePath} - already kebab-case${colors.reset}`);
    return false;
  }
  
  const newBaseName = toKebabCase(baseName);
  const newName = `${newBaseName}.md`;
  const newPath = path.join(dir, newName);
  
  console.log(`${colors.blue}Renaming: ${oldName} → ${newName}${colors.reset}`);
  
  try {
    // Use git mv for renaming to preserve history
    await exec(`git mv "${filePath}" "${newPath}"`);
    console.log(`${colors.green}Renamed: ${filePath} → ${newPath}${colors.reset}`);
    
    // Update references in all markdown files
    await updateReferences(oldName, newName);
    
    return true;
  } catch (err) {
    console.error(`${colors.red}Error renaming ${filePath}: ${err.message}${colors.reset}`);
    return false;
  }
}

// Update references to the renamed file in all markdown files
async function updateReferences(oldName, newName) {
  try {
    // Find all references to the old filename
    const { stdout } = await exec(`grep -l "${oldName}" $(find docs -name "*.md" -type f)`);
    
    if (!stdout.trim()) {
      return; // No references found
    }
    
    const filesToUpdate = stdout.trim().split('\n');
    console.log(`${colors.blue}Updating references in ${filesToUpdate.length} files${colors.reset}`);
    
    for (const file of filesToUpdate) {
      try {
        // Update references using sed
        await exec(`sed -i '' "s/${oldName}/${newName}/g" "${file}"`);
        console.log(`${colors.green}Updated references in ${file}${colors.reset}`);
      } catch (err) {
        console.error(`${colors.red}Error updating references in ${file}: ${err.message}${colors.reset}`);
      }
    }
  } catch (err) {
    // If grep doesn't find any files, it returns exit code 1, which is not an error in this context
    if (!err.message.includes('No such file or directory')) {
      console.log(`${colors.yellow}No references to update for ${oldName}${colors.reset}`);
    }
  }
}

main(); 