# Prompting Guide for Claude 3.7 Sonnet

This guide provides detailed instructions for using Claude 3.7 Sonnet to convert the existing SAP-GitHub Integration Playbook into a tutorial-focused format, implementing the reorganization plan. This prompt is structured to maximize Claude's abilities to transform complex technical documentation into user-friendly tutorials while ensuring all non-tutorial content is removed.

## Main Prompt

```
I need to transform the SAP-GitHub Integration Playbook into a tutorial-focused format following a specific reorganization plan. Please help me convert technical documentation into user-friendly, step-by-step tutorials with a coherent narrative flow.

# Reorganization Context
I'm restructuring the SAP-GitHub Integration Playbook to:
1. Focus only on technical implementation
2. Create an intuitive tutorial flow from basics to advanced scenarios
3. Provide clear, step-by-step guidance with visuals
4. Remove business content (ROI, executive overviews)
5. Standardize the format across all tutorials

# New Structure
The new structure follows this progression:
- 1-overview/ - Technical understanding of the integration
- 2-prerequisites/ - Required setup before implementation
- 3-setup-guide/ - Step-by-step component setup
- 4-workflow-tutorials/ - Practical workflow implementation
- 5-advanced-scenarios/ - System-specific adaptations
- 6-troubleshooting/ - Problem resolution

# Current Task
I need help with [SPECIFIC SECTION], converting it from reference documentation to a practical tutorial format. The source material is from [SOURCE FILES].

# Specific Requirements
- Create a clear introduction that explains what users will accomplish
- Break down complex procedures into numbered steps
- Add notes and warnings at appropriate points
- Include placeholder indicators for screenshots and diagrams
- Ensure each tutorial has verification steps
- Add troubleshooting tips for common issues
- End with clear "next steps" that maintain the narrative flow

# Document Format
Please use this structure for each tutorial:
1. Title (using markdown H1)
2. Brief introduction explaining the goal and context
3. Prerequisites specific to this tutorial
4. Visual overview (placeholder for diagram)
5. Numbered implementation steps with explanations
6. Verification steps to confirm successful implementation
7. Common issues and solutions
8. Next steps in the implementation journey

# Example of Desired Output
[Include a short example showing the desired format and style]

# Source Material Notes
The existing material is comprehensive but needs:
- Simplification of complex technical concepts
- More explicit step-by-step guidance
- Addition of verification steps
- Better narrative flow between sections
- Removal of business-focused content (ROI, executive overviews, case studies)
- Removal of theoretical architectural discussions not directly tied to implementation

Please transform the content to match these requirements while maintaining technical accuracy. Be sure to exclude any content focused on business value rather than technical implementation.
```

## Sequential Flow Requirements
Each tutorial must maintain a clear sequential flow where:

1. Every step explicitly builds on previous steps
2. Each tutorial section connects logically to the next section 
3. Navigation between tutorials shows a clear progression
4. The entire implementation follows a cohesive journey

Please ensure:
- Each step begins with a clear reference to the previous step's outcome
- Steps are numbered sequentially throughout the entire tutorial
- Each section ends with a transition to the next section
- Include phrases like "Now that we've completed X, we can proceed to Y"
- Add clear "Next Steps" sections that explicitly tell users what to do next
- Use consistent terminology across connected tutorials
- Reference specific filenames, configuration values, and settings from previous steps

# Example of Strong Sequential Flow
```markdown
## Step 3: Configure Repository Settings

Now that we've created our GitHub repository in Step 2, we need to configure its settings for SAP integration.

6. In your GitHub repository (created in the previous step), navigate to **Settings**
7. Click on **Branches** in the left navigation menu
8. Under "Branch protection rules", click **Add rule**
9. In the "Branch name pattern" field, enter `main` (the same branch we created earlier)
10. Enable the following options:
    - "Require pull request reviews before merging"
    - "Require status checks to pass before merging"
    
Once you've configured these branch protection rules, you've completed the GitHub setup phase. In the next section, we'll connect this repository to your SAP system using the API Management configuration we'll set up in Step 4.

## Next Steps
Proceed to [Step 4: Configuring API Management](../3-setup-guide/middleware/apim-deployment.md) to create the secure connection between your SAP system and the GitHub repository you just configured.
```

Notice how this example:
- Explicitly references the previous step ("Now that we've created our GitHub repository in Step 2...")
- Continues sequential numbering from previous sections (starting at 6 rather than 1)
- References specific values from earlier steps ("the same branch we created earlier")
- Clearly indicates the next logical step in the process
- Provides a direct link to the next tutorial in the sequence

## Customizing the Prompt for Specific Sections

When processing individual sections, customize the "Current Task" portion with specific information:

### For Overview Section

```
# Current Task
I need help with the Overview section, converting architectural documentation into a practical explanation of how the integration works. The source material is from `/docs/documentation/architecture/overview.md` and `/docs/documentation/architecture/index.md`.

# Specific Requirements
- Simplify complex architectural concepts
- Focus on how components interact rather than design principles
- Include placeholders for key diagrams showing component interaction
- Explain the technical flow without business value discussions
```

### For Setup Guide Section

```
# Current Task
I need help with the SAP System Setup section, converting implementation references into step-by-step tutorials. The source material is from `/docs/documentation/guides/sap-setup/` and `/docs/getting-started/installation.md`.

# Specific Requirements
- Break down installation into clear, numbered steps
- Add placeholders for SAP GUI screenshots at key configuration points
- Include command examples with expected output
- Add verification steps after each major configuration
- Include troubleshooting tips for common installation issues
```

### For Workflow Tutorials Section

```
# Current Task
I need help with the Basic Development Workflow tutorial, creating a practical guide to the development process. The source material is from `/docs/getting-started/quick-start.md` and `/docs/documentation/guides/workflows/`.

# Specific Requirements
- Create a narrative that follows code from creation to deployment
- Include placeholders for workflow diagrams showing the process
- Add detailed explanations of each step in the development cycle
- Include example commands and responses
- Add verification checkpoints throughout the workflow
```

### For Advanced Scenarios Section

```
# Current Task
I need help with the ABAP Systems integration scenario, adapting system-specific guides into practical tutorials. The source material is from `/docs/documentation/guides/erp-abap/`.

# Specific Requirements
- Create separate tutorials for systems with and without CHARM
- Include system-specific configuration steps
- Add placeholders for architecture diagrams showing integration points
- Include example configurations specific to ABAP systems
- Add system-specific troubleshooting tips
```

### For Troubleshooting Section

```
# Current Task
I need help with the Connectivity Troubleshooting section, converting operations documentation into practical problem-resolution guides. The source material is from `/docs/documentation/operations/maintenance/troubleshooting-integration.md`.

# Specific Requirements
- Organize issues by symptom rather than component
- Create a diagnostic flowchart placeholder
- Include specific error messages and their meanings
- Provide step-by-step resolution procedures
- Add verification steps to confirm issue resolution
```

## Working with Visuals

Since Claude can't create or view images directly, use these instructions for visual placeholders:

```
# Visual Content Guidelines
For each diagram or screenshot placeholder:

1. Use this format for diagram placeholders:
   [DIAGRAM: Brief description of diagram content]
   
   For example:
   [DIAGRAM: Component interaction showing data flow between SAP, API Management, and GitHub]

2. Use this format for screenshot placeholders:
   [SCREENSHOT: Description of the UI being shown]
   
   For example:
   [SCREENSHOT: abapGit installation screen in SAP GUI with required fields highlighted]

3. Include detailed descriptions of what should be in each visual
4. Note important elements that should be highlighted
5. Indicate approximately where in the tutorial each visual should appear
```

## For Code Examples

Provide these instructions for handling code examples:

```
# Code Example Guidelines
When including code examples:

1. Use proper markdown code blocks with language specification
2. Include comments explaining key parts of the code
3. For configuration files, include complete examples rather than snippets
4. For commands, include expected output where helpful
5. For long configurations, include only the essential parts with comments indicating omitted sections
```

## Final Notes

- Claude 3.7 Sonnet works best with clearly defined tasks and examples
- Provide sample materials when available to guide the style and tone
- Break large sections into smaller chunks for better results
- Review and refine Claude's output for technical accuracy
- Be specific about the narrative flow you want between sections
