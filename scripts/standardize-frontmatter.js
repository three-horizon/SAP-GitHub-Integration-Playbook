#!/usr/bin/env node

/**
 * standardize-frontmatter.js
 * 
 * This script adds or updates front matter in markdown files.
 * 
 * Usage: node standardize-frontmatter.js [directory]
 * 
 * If no directory is specified, it processes all files in the docs directory.
 */

const fs = require('fs');
const path = require('path');
const util = require('util');
const readdir = util.promisify(fs.readdir);
const stat = util.promisify(fs.stat);
const readFile = util.promisify(fs.readFile);
const writeFile = util.promisify(fs.writeFile);

// Colors for console output
const colors = {
  reset: '\x1b[0m',
  red: '\x1b[31m',
  green: '\x1b[32m',
  yellow: '\x1b[33m',
  blue: '\x1b[34m'
};

// Main function
async function main() {
  try {
    const targetDir = process.argv[2] || 'docs';
    console.log(`${colors.blue}Processing Markdown files in ${targetDir}${colors.reset}`);
    
    const files = await getAllMarkdownFiles(targetDir);
    console.log(`${colors.blue}Found ${files.length} Markdown files${colors.reset}`);
    
    for (const file of files) {
      await processFile(file);
    }
    
    console.log(`${colors.green}Completed! All files processed.${colors.reset}`);
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
        files.push(fullPath);
      }
    }
  }
  
  await traverse(dir);
  return files;
}

// Process a single file
async function processFile(filePath) {
  try {
    // Skip certain files
    if (filePath.includes('node_modules') || filePath.includes('.git')) {
      return;
    }
    
    const content = await readFile(filePath, 'utf8');
    
    // Check if already has front matter
    if (content.startsWith('---\n')) {
      console.log(`${colors.yellow}Skipping ${filePath} - already has front matter${colors.reset}`);
      return;
    }
    
    // Generate title from filename or first heading
    let title = path.basename(filePath, '.md')
      .replace(/-/g, ' ')
      .split(' ')
      .map(word => word.charAt(0).toUpperCase() + word.slice(1))
      .join(' ');
    
    // Check for H1 heading
    const headingMatch = content.match(/^# (.+)$/m);
    if (headingMatch && headingMatch[1]) {
      title = headingMatch[1];
    }
    
    // Create front matter
    const frontMatter = `---
layout: default
title: ${title}
---

`;
    
    // Add front matter
    const newContent = frontMatter + content;
    await writeFile(filePath, newContent, 'utf8');
    
    console.log(`${colors.green}Updated ${filePath}${colors.reset}`);
  } catch (err) {
    console.error(`${colors.red}Error processing ${filePath}: ${err.message}${colors.reset}`);
  }
}

main(); 