/**
 * SVG Fix Script for GitHub Pages
 * 
 * This script enhances SVG display by providing fallback mechanisms
 * and fixing common SVG rendering issues across browsers.
 */

document.addEventListener('DOMContentLoaded', function() {
    // Select all SVG images
    const svgImages = document.querySelectorAll('img[src$=".svg"]');
    
    // Process each SVG image
    svgImages.forEach(function(img) {
        // Get the current source
        const src = img.getAttribute('src');
        
        // Check if loading failed
        img.onerror = function() {
            console.log('SVG failed to load:', src);
            
            // Try to load from GitHub Pages URL if not already using it
            if (!src.includes('three-horizon.github.io')) {
                const ghPagesUrl = 'https://three-horizon.github.io/SAP-GitHub-Integration-Playbook/' + 
                                   src.replace(/^\.\//, '');
                console.log('Trying GitHub Pages URL:', ghPagesUrl);
                img.src = ghPagesUrl;
            } 
            // Try to load from GitHub Raw if not already
            else if (!src.includes('raw.githubusercontent.com')) {
                const ghRawUrl = 'https://raw.githubusercontent.com/three-horizon/SAP-GitHub-Integration-Playbook/main/' + 
                                 src.replace(/^\.\//, '').replace(/^https:\/\/three-horizon\.github\.io\/SAP-GitHub-Integration-Playbook\//, '');
                console.log('Trying GitHub Raw URL:', ghRawUrl);
                img.src = ghRawUrl;
            }
        };
        
        // Add CSS class for SVG-specific styling
        img.classList.add('svg-image');
    });
    
    // Log success
    console.log('SVG fix script loaded and processed', svgImages.length, 'SVG images');
}); 