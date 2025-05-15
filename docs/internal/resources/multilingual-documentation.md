# Guide for Multilingual Documentation

This guide provides standards for translating and maintaining multilingual documentation for the SAP-GitHub Integration Playbook.

## Supported Languages

Currently, we support documentation in:
- English (primary)
- German
- Portuguese
- Spanish 
- French

## File Structure

For multilingual documentation:

1. **English Content**: stored in the main documentation path
2. **Translations**: stored in language-specific subfolders

Example:
```
docs/documentation/
├── guide.md                 # English version
├── de/                      # German translations
│   └── guide.md
├── pt/                      # Portuguese translations
│   └── guide.md
```

## Translation Process

1. **Create English content first** - This serves as the source of truth
2. **Keep structure consistent** - Maintain the same structure, headings, and examples
3. **Use glossary** - Refer to the technical glossary for standard translations
4. **Preserve code examples** - Don't translate code (unless in comments)
5. **Review process** - Each translation must be reviewed by at least one native speaker

## Language Switching

Include a language selector at the top of each document:

```markdown
> Also available in: [Deutsch](de/guide.md) | [Português](pt/guide.md) | [Español](es/guide.md) | [Français](fr/guide.md)
```

## Translation Tips

- Focus on technical accuracy over literal translation
- Preserve technical terms in English when standard in the industry
- Include regional variations in parentheses when relevant
- Use tools like terminology databases and translation memory
