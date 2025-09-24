# Git Commit Instructions

All commit messages in Renovatio must follow the Conventional Commits specification and be written in English. This ensures clarity, traceability, and consistency across all modules and teams.

## Format

```
<type>[optional scope]: <short description>

[optional body]
[optional footer]
```

- **type**: feat, fix, refactor, test, docs, chore, build, ci, perf, style
- **scope**: (optional) module, package, or feature (e.g. core, cobol, java, shared, web)
- **short description**: concise summary of the change (max 80 chars)
- **body**: (optional) more detailed explanation, motivation, context
- **footer**: (optional) issues closed, breaking changes, migration notes

## Examples

```
feat(core): add MCP-compliant migration report aggregation

fix(cobol): handle copybook parsing edge case for nested groups

refactor(shared): improve DTO mapping performance

docs(web): update OpenAPI documentation for new endpoints

test(java): add unit tests for LanguageProviderRegistry

chore: update Maven dependencies and plugins
```

## Rules
- Use English for all commit messages.
- Use present tense: "add", "fix", "update" (not "added", "fixed", "updated").
- Reference issues in the footer if relevant: "Closes #123".
- For breaking changes, add "BREAKING CHANGE:" in the footer.
- Keep the short description concise and informative.
- Group related changes in a single commit when possible.
- Avoid generic messages like "update code" or "misc changes".

## Business context
- Always relate commits to the MCP architecture, modularity, and interoperability goals of Renovatio.
- Document changes that affect MCP compliance, API schemas, or migration/refactor logic.

For more details, see https://www.conventionalcommits.org/en/v1.0.0/
