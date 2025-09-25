# Renovatio - MCP Server for Code Migration and Refactoring

**Renovatio** is a Model Content Protocol (MCP) server for automated code migration and refactoring, based on OpenRewrite concepts. It provides tools for migrating and upgrading COBOL and Java code with extensibility for additional languages.

---

## Architecture

Renovatio is organized as a multi-module Maven project with a clean separation between the MCP protocol implementation and the core migration engine:

```
renovatio/
├── renovatio-shared/         # Common interfaces and domain models
├── renovatio-core/           # Core migration logic (protocol-agnostic)
├── renovatio-provider-java/  # Java provider (OpenRewrite integration)
├── renovatio-provider-cobol/ # COBOL provider (parsing and migration)
└── renovatio-mcp-server/     # MCP protocol server implementation
```

---

## Module Responsibilities

### renovatio-shared
Common interfaces, domain models, and utilities shared across all modules.

### renovatio-core
Core migration logic, tool registry, and orchestration services independent of any protocol.

### renovatio-provider-java
Java language provider with OpenRewrite integration for Java refactoring and migration.

### renovatio-provider-cobol
COBOL language provider with parsing capabilities and Java code generation for COBOL-to-Java migration.

### renovatio-mcp-server
MCP protocol implementation that exposes the core migration capabilities as MCP tools following JSON-RPC 2.0 specification.

---

## Technology Stack

- **Java 17+**: Core platform
- **Spring Boot**: Dependency injection and configuration
- **Maven**: Build and dependency management
- **OpenRewrite**: Java refactoring engine
- **MCP (Model Content Protocol)**: Tool exposure protocol
- **JSON-RPC 2.0**: Communication protocol

---

## Quick Start

1. Build the project:
```bash
mvn clean compile
```

2. Run the MCP server:
```bash
java -jar renovatio-mcp-server/target/renovatio-mcp-server-*.jar
```

3. Connect MCP clients to the server to access migration tools.

---

## MCP Integration

Renovatio implements the Model Content Protocol specification, making it compatible with MCP clients like VS Code extensions and Copilot Workspace. All tools are exposed following MCP standards with proper JSON-RPC 2.0 messaging.

**Renovatio** – Focused MCP server for code migration and modernization.
