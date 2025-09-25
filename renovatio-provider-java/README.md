# Renovatio Java Provider

OpenRewrite-powered Java refactoring and migration provider for Renovatio.

## Overview

`renovatio-provider-java` integrates OpenRewrite into the Renovatio platform, providing comprehensive Java refactoring and migration capabilities:

- **OpenRewrite integration**: Full access to OpenRewrite's recipe ecosystem
- **Java analysis**: Static analysis, dependency analysis, and code metrics
- **Refactoring automation**: Apply recipes for modernization and improvement
- **Migration support**: Upgrade Java versions, frameworks, and libraries
- **Plan/Apply workflow**: Safe refactoring with preview and rollback capabilities

## Available Tools

### Core Operations
- `java_discover`: Inspect workspace structure and identify Java projects
- `java_analyze`: Analyze Java source code with OpenRewrite
- `java_plan`: Plan refactoring operations based on goals
- `java_apply`: Apply OpenRewrite recipes to source code
- `java_diff`: Generate diffs showing refactoring changes
- `java_review`: Summarize refactoring outcomes and impact

### Utilities
- `java_format`: Format code and remove unused imports
- `java_test`: Run project tests after refactoring
- `java_metrics`: Collect code quality metrics
- `java_recipe_list`: List available OpenRewrite recipes
- `java_recipe_describe`: Get detailed recipe information
- `java_pipeline`: Execute preset modernization pipelines

## Example Usage

### Analyze Java Project
```bash
curl -X POST http://localhost:8080/ \
  -H "Content-Type: application/json" \
  -d '{
    "jsonrpc": "2.0",
    "id": "1",
    "method": "tools/call", 
    "params": {
      "name": "java_analyze",
      "arguments": {
        "workspacePath": "/path/to/java/project",
        "nql": "FIND classes WHERE annotation EXISTS @Deprecated"
      }
    }
  }'
```

### Apply Recipe
```bash
curl -X POST http://localhost:8080/ \
  -H "Content-Type: application/json" \
  -d '{
    "jsonrpc": "2.0",
    "id": "2", 
    "method": "tools/call",
    "params": {
      "name": "java_apply",
      "arguments": {
        "workspacePath": "/path/to/java/project",
        "recipes": ["org.openrewrite.java.format.AutoFormat"],
        "dryRun": false
      }
    }
  }'
```

## Supported Recipes

The provider supports all OpenRewrite recipes including:

- **Java version migration**: Java 8 → 11 → 17 → 21
- **Framework upgrades**: Spring Boot, JUnit, etc.
- **Code formatting**: Google Style, Spring Style, etc.  
- **Security fixes**: Common vulnerability patterns
- **Performance improvements**: Best practices application
- **Modernization**: Lambda expressions, streams, records

## Configuration

Configure in `application.yml`:

```yaml
renovatio:
  java:
    openrewrite:
      recipes-path: "classpath:recipes/java"
      default-style: "google"
    analysis:
      include-test-sources: true
      max-file-size: "10MB"
```

## Dependencies

Built on:
- **OpenRewrite**: Recipe execution engine
- **JavaPoet**: Code generation utilities  
- **Maven/Gradle**: Build tool integration
- **JGit**: Git operations for diff generation