# Renovatio JCL Provider

JCL (Job Control Language) parsing and modernization provider for converting mainframe batch jobs to modern workflows.

## Overview

`renovatio-provider-jcl` provides capabilities for parsing, analyzing, and converting JCL scripts to modern workflow systems:

- **JCL parsing**: Parse and analyze JCL scripts with ANTLR4
- **AST extraction**: Build abstract syntax trees from JCL programs
- **Modernization**: Convert JCL jobs to Shell, GitHub Actions, Spring Batch, or Airflow
- **Dependency analysis**: Map job dependencies and data flows
- **Template-based generation**: Flexible code generation with customizable templates

## Available Tools

### Analysis
- `jcl_analyze`: Parse JCL scripts and extract structure
- `jcl_dependencies`: Analyze job dependencies and data flows
- `jcl_metrics`: Calculate complexity and migration metrics

### Migration  
- `jcl_to_shell`: Convert JCL jobs to shell scripts
- `jcl_to_github_actions`: Generate GitHub Actions workflows
- `jcl_to_spring_batch`: Create Spring Batch job configurations
- `jcl_to_airflow`: Generate Apache Airflow DAGs

## Example Usage

### Analyze JCL Script
```bash
curl -X POST http://localhost:8080/ \
  -H "Content-Type: application/json" \
  -d '{
    "jsonrpc": "2.0",
    "id": "1",
    "method": "tools/call",
    "params": {
      "name": "jcl_analyze", 
      "arguments": {
        "workspacePath": "/path/to/jcl/scripts",
        "includeDatasets": true,
        "includeDependencies": true
      }
    }
  }'
```

### Convert to GitHub Actions
```bash
curl -X POST http://localhost:8080/ \
  -H "Content-Type: application/json" \
  -d '{
    "jsonrpc": "2.0",
    "id": "2",
    "method": "tools/call",
    "params": {
      "name": "jcl_to_github_actions",
      "arguments": {
        "jclPath": "/path/to/script.jcl",
        "outputPath": ".github/workflows/",
        "includeEnvironmentVars": true
      }
    }
  }'
```

## Supported JCL Features

- **Job statements**: Job definition and parameters
- **Exec statements**: Program execution steps  
- **DD statements**: Dataset definitions and allocations
- **Condition codes**: Job step conditional execution
- **Cataloged procedures**: PROC invocation and overrides
- **Symbolic parameters**: Parameter substitution
- **JCL comments**: Documentation preservation

## Target Platforms

### Shell Scripts
- Bash/Zsh compatible syntax
- Environment variable handling
- Exit code management
- Logging and error handling

### GitHub Actions
- Workflow YAML generation
- Job dependencies and conditions
- Artifact handling
- Environment configuration

### Spring Batch
- Job configuration XML/Java
- Step definitions and flow
- Tasklet implementations
- Job parameters and context

### Apache Airflow  
- DAG Python definitions
- Task dependencies
- Operator configurations
- Connection and variable management

## Configuration

```yaml
renovatio:
  jcl:
    parser:
      strict-mode: false
      include-comments: true
    generation:
      default-target: "shell"
      template-path: "classpath:templates/jcl"
```