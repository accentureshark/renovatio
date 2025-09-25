# Renovatio Architecture

Renovatio follows a clean separation between the MCP protocol implementation and the core migration engine, enabling
both standalone usage and MCP client integration.

## Architecture Overview

```
┌─────────────────────┐
│   MCP Clients       │
│   (VS Code, etc.)   │
└──────────┬──────────┘
           │ JSON-RPC 2.0
           │
┌──────────▼──────────┐
│  renovatio-mcp-     │
│  server             │
│  ┌─────────────────┐│
│  │ MCP Protocol    ││
│  │ Implementation  ││
│  └─────────────────┘│
└──────────┬──────────┘
           │
┌──────────▼──────────┐
│  renovatio-core     │
│  (Core Engine)      │
│  ┌─────────────────┐│
│  │ Language        ││
│  │ Provider        ││
│  │ Registry        ││
│  └─────────────────┘│
└──────────┬──────────┘
           │
    ┌──────┴──────┐
    │             │
┌───▼──┐    ┌─────▼──────┐
│ Java │    │   COBOL    │
│ Prov │    │ Provider   │
└──────┘    └────────────┘
```

## Module Structure

### renovatio-shared

- Common interfaces and domain models
- Protocol-agnostic abstractions
- Shared utilities and DTOs

### renovatio-core

- Core migration engine (protocol-agnostic)
- Language provider registry
- Tool orchestration and execution
- Business logic for migration operations

### renovatio-mcp-server

- Complete MCP protocol implementation
- JSON-RPC 2.0 server
- Spring Boot application
- Exposes core engine via MCP tools

### Language Providers

#### renovatio-provider-java

- OpenRewrite integration
- Java refactoring and migration tools
- Recipe discovery and execution

#### renovatio-provider-cobol

- COBOL parsing and analysis
- COBOL-to-Java migration
- Code generation capabilities

## Design Principles

1. **Protocol Separation**: Core engine is completely independent of MCP
2. **Extensibility**: New language providers can be easily added
3. **Standards Compliance**: Full MCP and JSON-RPC 2.0 compliance
4. **Modularity**: Clean separation of concerns across modules
5. **Simplicity**: Focus on core migration capabilities

## Usage Patterns

### As MCP Server

Connect MCP clients to `renovatio-mcp-server` for tool-based interactions.

### As Library

Use `renovatio-core` directly in applications that need migration capabilities without MCP protocol overhead.
