# Renovatio MCP Tools - Quick Reference

## Server URL
```
http://localhost:8181/
```

## Required Headers
```
Content-Type: application/json
```

## Basic Request Format
```json
{
  "jsonrpc": "2.0",
  "id": "unique-id",
  "method": "method-name",
  "params": { /* parameters */ }
}
```

## Essential Commands

### 1. Initialize Connection
```json
{
  "jsonrpc": "2.0",
  "id": "1",
  "method": "initialize",
  "params": {}
}
```

### 2. List Available Tools
```json
{
  "jsonrpc": "2.0",
  "id": "2",
  "method": "tools/list",
  "params": {}
}
```

### 3. Call a Tool
```json
{
  "jsonrpc": "2.0",
  "id": "3",
  "method": "tools/call",
  "params": {
    "name": "tool-name",
    "arguments": { /* tool-specific args */ }
  }
}
```

## Java Analysis Tools

### Analyze Java Code
```json
{
  "jsonrpc": "2.0",
  "id": "4",
  "method": "tools/call",
  "params": {
    "name": "java_analyze",
    "arguments": {
      "scope": "/path/to/java/project",
      "nql": "FIND ALL CLASSES"
    }
  }
}
```

### Calculate Metrics
```json
{
  "jsonrpc": "2.0",
  "id": "5",
  "method": "tools/call",
  "params": {
    "name": "java_metrics",
    "arguments": {
      "scope": "/path/to/java/project",
      "nql": "CALCULATE METRICS FOR ALL CLASSES"
    }
  }
}
```

### Create Transformation Plan
```json
{
  "jsonrpc": "2.0",
  "id": "6",
  "method": "tools/call",
  "params": {
    "name": "java_plan",
    "arguments": {
      "scope": "/path/to/java/project",
      "nql": "MIGRATE FROM JAVA 8 TO JAVA 17"
    }
  }
}
```

### Apply Transformations
```json
{
  "jsonrpc": "2.0",
  "id": "7",
  "method": "tools/call",
  "params": {
    "name": "java_apply",
    "arguments": {
      "scope": "/path/to/java/project",
      "nql": "APPLY MIGRATION PLAN"
    }
  }
}
```

### Generate Diff
```json
{
  "jsonrpc": "2.0",
  "id": "8",
  "method": "tools/call",
  "params": {
    "name": "java_diff",
    "arguments": {
      "scope": "/path/to/java/project",
      "nql": "SHOW CHANGES"
    }
  }
}
```

## Common Tools

### Index Repository
```json
{
  "jsonrpc": "2.0",
  "id": "9",
  "method": "tools/call",
  "params": {
    "name": "common_index",
    "arguments": {
      "repoId": "/path/to/project"
    }
  }
}
```

### Search Repository
```json
{
  "jsonrpc": "2.0",
  "id": "10",
  "method": "tools/call",
  "params": {
    "name": "common_search",
    "arguments": {
      "repoId": "/path/to/project",
      "query": "search-term",
      "path": "src/main/java"
    }
  }
}
```

### Compile Natural Language to NQL
```json
{
  "jsonrpc": "2.0",
  "id": "11",
  "method": "tools/call",
  "params": {
    "name": "nql_compile",
    "arguments": {
      "question": "Find all classes with more than 10 methods",
      "context": "Code quality analysis"
    }
  }
}
```

## curl Examples

### Start Server
```bash
cd renovatio-mcp-server
SERVER_PORT=8181 mvn spring-boot:run
```

### Test Connection
```bash
curl -X POST -H "Content-Type: application/json" \
  -d '{"jsonrpc": "2.0", "id": "1", "method": "initialize", "params": {}}' \
  http://localhost:8181/
```

### List Tools
```bash
curl -X POST -H "Content-Type: application/json" \
  -d '{"jsonrpc": "2.0", "id": "2", "method": "tools/list", "params": {}}' \
  http://localhost:8181/ | jq
```

### Analyze Java Project
```bash
curl -X POST -H "Content-Type: application/json" \
  -d '{
    "jsonrpc": "2.0",
    "id": "3",
    "method": "tools/call",
    "params": {
      "name": "java_analyze",
      "arguments": {
        "scope": "/tmp/my-java-project",
        "nql": "FIND ALL CLASSES"
      }
    }
  }' \
  http://localhost:8181/ | jq
```

## Expected Response Format
```json
{
  "id": "request-id",
  "result": {
    "content": [
      {
        "text": "response-content",
        "type": "text|object"
      }
    ]
  },
  "error": null,
  "jsonrpc": "2.0"
}
```

## Available Tools Summary

| Tool | Description | Key Parameters |
|------|-------------|----------------|
| `nql_compile` | Convert natural language to NQL | `question`, `context` |
| `common_index` | Index repository for search | `repoId` |
| `common_search` | Search indexed repository | `repoId`, `query`, `path` |
| `java_analyze` | Analyze Java code structure | `scope`, `nql` |
| `java_plan` | Create transformation plan | `scope`, `nql` |
| `java_apply` | Apply transformations | `scope`, `nql` |
| `java_diff` | Generate semantic diff | `scope`, `nql` |
| `java_metrics` | Calculate code metrics | `scope`, `nql` |

## Common NQL Queries

- `FIND ALL CLASSES`
- `CALCULATE METRICS FOR ALL CLASSES`
- `MIGRATE FROM JAVA 8 TO JAVA 17`
- `APPLY MIGRATION PLAN`
- `SHOW CHANGES`

## Troubleshooting

### Server Not Running
**Error**: Connection refused
**Solution**: Start server with `mvn spring-boot:run`

### Invalid Path
**Error**: `success=false`
**Solution**: Use absolute path to Java project

### Tool Not Found
**Error**: Method not found
**Solution**: Check tool name spelling (use underscore, not camelCase)

### NQL Not Implemented
**Info**: `NQL operation not yet implemented`
**Note**: NQL is in development, use basic queries