# Renovatio - MCP Server for Code Migration and Refactoring

**Renovatio** is a Model Content Protocol (MCP) server for automated code migration and refactoring, based on
OpenRewrite concepts. It provides tools for migrating and upgrading COBOL and Java code with extensibility for
additional languages.

---

## Architecture

Renovatio is organized as a multi-module Maven project with a clean separation between the MCP protocol implementation
and the core migration engine:

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

MCP protocol implementation that exposes the core migration capabilities as MCP tools following JSON-RPC 2.0
specification.

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

Renovatio implements the Model Content Protocol specification, making it compatible with MCP clients like VS Code
extensions and Copilot Workspace. All tools are exposed following MCP standards with proper JSON-RPC 2.0 messaging.

### Language selection (Java/Cobol) from MCP clients

Clients can request tools for a specific language by passing a `language` parameter. This helps surface only the
relevant tools for the chosen language:

- During `initialize`:

```json
{
  "jsonrpc": "2.0",
  "id": "1",
  "method": "initialize",
  "params": {
    "language": "cobol"
  }
}
```

- When listing tools:

```json
{
  "jsonrpc": "2.0",
  "id": "2",
  "method": "tools/list",
  "params": {
    "language": "java"
  }
}
```

If `language` is omitted, all tools from all registered providers are returned.

### Published COBOL tools

The COBOL provider now exposes the following MCP tools:

- `cobol.analyze` — Analyze COBOL sources (parsing, AST, dependencies)
- `cobol.metrics` — Collect high-level COBOL metrics (files, lines, copybooks)
- `cobol.plan` — Create migration plan from COBOL to Java
- `cobol.apply` — Apply a previously created migration plan
- `cobol.diff` — Generate diff for the last migration run
- `cobol.migrate_copybook` — Generate Java artifacts from a COBOL copybook (template-based)
- `cobol.migrate_db2` — Generate JPA code from embedded DB2 EXEC SQL in COBOL programs

All provider tools accept a `workspacePath` parameter (added automatically by the server-side adapter when not present
in the schema) to point to the workspace directory.

---

**Renovatio** – Focused MCP server for code migration and modernization.
