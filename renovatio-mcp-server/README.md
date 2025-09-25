# Renovatio MCP Server

Model Content Protocol (MCP) server implementation for Renovatio, providing JSON-RPC 2.0 compliant access to all refactoring and migration tools.

## Overview

`renovatio-mcp-server` is a Spring Boot application that exposes the Renovatio core engine through the MCP protocol:

- **Full MCP 2025-06-18 compliance**: Implements all required MCP methods
- **JSON-RPC 2.0**: Standards-compliant protocol implementation
- **HTTP and STDIO transport**: Multiple transport options for different use cases
- **Auto-discovery**: Dynamic tool discovery from registered language providers
- **OpenAPI documentation**: Swagger UI for API exploration

## Features

### MCP Protocol Support
- `initialize`: Protocol handshake and capability negotiation
- `tools/list`: List all available tools
- `tools/call`: Execute a specific tool
- `tools/describe`: Get detailed tool descriptions
- `prompts/list`, `prompts/get`: Prompt management
- `resources/list`, `resources/read`: Resource access
- `workspace/list`, `workspace/describe`: Workspace operations

### Transport Options
- **HTTP**: REST-like HTTP API on port 8080
- **STDIO**: Standard input/output for direct integration

## Quick Start

### Start HTTP Server
```bash
cd renovatio-mcp-server
mvn spring-boot:run
```

Server starts on `http://localhost:8080/`

### Example MCP Calls

Initialize connection:
```bash
curl -X POST http://localhost:8080/ \
  -H "Content-Type: application/json" \
  -d '{"jsonrpc": "2.0", "id": "1", "method": "initialize", "params": {}}'
```

List available tools:
```bash
curl -X POST http://localhost:8080/ \
  -H "Content-Type: application/json" \
  -d '{"jsonrpc": "2.0", "id": "2", "method": "tools/list"}'
```

Execute a Java analysis:
```bash
curl -X POST http://localhost:8080/ \
  -H "Content-Type: application/json" \
  -d '{
    "jsonrpc": "2.0",
    "id": "3", 
    "method": "tools/call",
    "params": {
      "name": "java_analyze",
      "arguments": {
        "workspacePath": "/path/to/java/project",  
        "nql": "FIND classes WHERE name LIKE *Service"
      }
    }
  }'
```

## Configuration

Key configuration options in `application.yml`:

```yaml
server:
  port: 8080

mcp:
  server:
    name: "Renovatio MCP Server"
    version: "1.0.0"
    protocol-version: "2025-06-18"
  capabilities:
    tools: true
    prompts: true
    resources: true
```

## Integration

### VS Code MCP Extension
```json
{
  "servers": {
    "renovatio": {
      "command": "java",
      "args": ["-jar", "renovatio-mcp-server.jar"],
      "env": {
        "SERVER_PORT": "8080"
      }
    }
  }
}
```

### GitHub Copilot Workspace
The server is compatible with GitHub Copilot Workspace MCP integration.

## Documentation

- **OpenAPI/Swagger UI**: [http://localhost:8080/swagger-ui/index.html](http://localhost:8080/swagger-ui/index.html)
- **MCP Specification**: [https://modelcontentprotocol.io/specification/2025-06-18/](https://modelcontentprotocol.io/specification/2025-06-18/)