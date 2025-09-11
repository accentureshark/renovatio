# Renovatio Architecture - MCP Server and Core Engine Separation

This document describes the new modular architecture where MCP logic has been completely separated from the core engine.

## Architecture Overview

```
┌─────────────────────┐    ┌─────────────────────┐    ┌─────────────────────┐
│   MCP Clients       │    │   HTTP Clients      │    │   Library Users     │
│   (VS Code, etc.)   │    │   (Web Apps)        │    │   (Direct Usage)    │
└──────────┬──────────┘    └──────────┬──────────┘    └──────────┬──────────┘
           │                          │                          │
           │ JSON-RPC 2.0             │ REST API                 │ Direct Calls
           │                          │                          │
┌──────────▼──────────┐    ┌──────────▼──────────┐    ┌──────────▼──────────┐
│  renovatio-mcp-     │    │  renovatio-web      │    │     Your App        │
│  server             │    │                     │    │                     │
│  ┌─────────────────┐│    │                     │    │                     │
│  │ MCP Protocol    ││    │                     │    │                     │
│  │ Implementation  ││    │                     │    │                     │
│  └─────────────────┘│    │                     │    │                     │
└──────────┬──────────┘    └──────────┬──────────┘    └──────────┬──────────┘
           │                          │                          │
           │                          │                          │
           └──────────────────────────┼──────────────────────────┘
                                      │
                           ┌──────────▼──────────┐
                           │  renovatio-core     │
                           │  (Pure Engine)      │
                           │  ┌─────────────────┐│
                           │  │ Language        ││
                           │  │ Provider        ││
                           │  │ Registry        ││
                           │  └─────────────────┘│
                           └──────────┬──────────┘
                                      │
                    ┌─────────────────┼─────────────────┐
                    │                 │                 │
         ┌──────────▼──────────┐ ┌───▼───┐ ┌──────────▼──────────┐
         │ renovatio-provider- │ │ ...   │ │ renovatio-provider- │
         │ java                │ │       │ │ cobol               │
         │ (OpenRewrite)       │ │       │ │ (ANTLR4)            │
         └─────────────────────┘ └───────┘ └─────────────────────┘
```

## Module Structure

### 🎯 renovatio-core (Pure Engine)

**Purpose**: Protocol-agnostic refactoring and migration engine

**Key Features**:
- ✅ Zero MCP dependencies
- ✅ Protocol-agnostic Tool and Recipe abstractions  
- ✅ Language provider registry with dynamic tool generation
- ✅ Can be used as Maven dependency or standalone library
- ✅ Unified recipe format for all languages

**Usage as Library**:
```xml
<dependency>
    <groupId>org.shark.renovatio</groupId>
    <artifactId>renovatio-core</artifactId>
    <version>0.0.1-SNAPSHOT</version>
</dependency>
```

**Example Usage**:
```java
// Use core engine directly in your application
LanguageProviderRegistry registry = new LanguageProviderRegistry();

// Get available tools (protocol-agnostic)
List<Tool> tools = registry.generateTools();

// Execute operations
Map<String, Object> result = registry.routeToolCall("java.analyze", arguments);
```

### 🚀 renovatio-mcp-server (MCP Protocol Implementation)

**Purpose**: Full MCP specification implementation that exposes the core engine

**Key Features**:
- ✅ Full MCP 2025-06-18 specification compliance
- ✅ Serves on root path "/" for maximum client compatibility
- ✅ All MCP methods: initialize, tools/*, prompts/*, resources/*, etc.
- ✅ JSON-RPC 2.0 compliant with proper error handling
- ✅ Spring Boot application with health checks and monitoring

**Supported MCP Methods**:
- `initialize` - Establish protocol version and capabilities
- `ping` - Connectivity test
- `tools/list` - List all available tools
- `tools/call` - Execute a specific tool
- `tools/describe` - Get detailed tool information
- `capabilities` - Server capabilities
- `server/info` - Server information
- `content/read`, `content/write` - File operations
- `workspace/list`, `workspace/describe` - Workspace operations
- `prompts/list`, `prompts/get` - Prompt management
- `resources/list`, `resources/read` - Resource access

**Starting the MCP Server**:
```bash
cd renovatio-mcp-server
mvn spring-boot:run
```

The server will start on port 8080 and serve MCP requests at `http://localhost:8080/`.

### 🔗 renovatio-shared (Common Abstractions)

**Purpose**: Protocol-agnostic interfaces and utilities

**Key Components**:
- `Tool` interface - Universal tool definition
- `Recipe` interface - Unified recipe format
- `BasicTool` implementation - Concrete tool implementation
- Domain models - Shared data structures

### 🛠️ Language Providers

**Purpose**: Language-specific implementation plugins

**Supported Languages**:
- **Java**: Via OpenRewrite recipes
- **COBOL**: Via ANTLR4 parsers
- **Extensible**: Easy to add new languages

## Usage Examples

### 1. Using Core Engine as Library

```java
@Component
public class MyRefactoringService {
    
    private final LanguageProviderRegistry coreEngine;
    
    public MyRefactoringService() {
        this.coreEngine = new LanguageProviderRegistry();
    }
    
    public List<String> getSupportedLanguages() {
        return new ArrayList<>(coreEngine.getSupportedLanguages());
    }
    
    public Map<String, Object> refactorCode(String language, String operation, Map<String, Object> params) {
        String toolName = language + "." + operation;
        return coreEngine.routeToolCall(toolName, params);
    }
}
```

### 2. MCP Client Integration

```json
// MCP Client Configuration
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

### 3. HTTP API Usage

```bash
# Initialize MCP session
curl -X POST http://localhost:8080/ \
  -H "Content-Type: application/json" \
  -d '{
    "jsonrpc": "2.0",
    "id": 1,
    "method": "initialize",
    "params": {
      "protocolVersion": "2025-06-18"
    }
  }'

# List available tools  
curl -X POST http://localhost:8080/ \
  -H "Content-Type: application/json" \
  -d '{
    "jsonrpc": "2.0",
    "id": 2,
    "method": "tools/list"
  }'

# Execute a tool
curl -X POST http://localhost:8080/ \
  -H "Content-Type: application/json" \
  -d '{
    "jsonrpc": "2.0",
    "id": 3,
    "method": "tools/call",
    "params": {
      "name": "java.analyze",
      "arguments": {
        "nql": "FIND classes WHERE name LIKE *Service",
        "scope": "src/main/java"
      }
    }
  }'
```

## Configuration

### Core Engine Configuration

The core engine is configured through the `LanguageProviderRegistry`:

```yaml
# application.yml (if using Spring Boot)
renovatio:
  providers:
    java:
      enabled: true
      recipes-path: "classpath:recipes/java"
    cobol:
      enabled: true
      recipes-path: "classpath:recipes/cobol"
  recipes:
    format: "unified"
    base-path: "classpath:recipes"
```

### MCP Server Configuration

```yaml
# renovatio-mcp-server/src/main/resources/application.yml
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
    content:
      read: true
      write: true
    workspace:
      list: true
      describe: true
```

## Benefits of This Architecture

### ✅ **Separation of Concerns**
- Core engine is pure business logic
- MCP server is pure protocol implementation
- Easy to maintain and test each component

### ✅ **Multiple Usage Patterns**
- **Library**: Include core as Maven dependency
- **MCP Server**: Full MCP protocol compliance
- **REST API**: Traditional HTTP endpoints
- **Embedded**: Use in any application

### ✅ **Protocol Agnostic**
- Core engine doesn't know about MCP
- Easy to add other protocols (GraphQL, gRPC, etc.)
- Future-proof architecture

### ✅ **Unified Recipe Format**
- Same recipe interface for all languages
- Java via OpenRewrite
- COBOL via ANTLR4
- Consistent experience across languages

### ✅ **MCP Compliance**
- Full compliance with MCP specification
- Serves at root path for maximum compatibility
- Proper JSON-RPC 2.0 implementation
- Complete error handling

## Testing

### Core Engine Tests
```bash
cd renovatio-core
mvn test
```

### MCP Server Tests  
```bash
cd renovatio-mcp-server
mvn test
```

### Integration Tests
```bash
# From project root
mvn verify
```

## Development

### Adding New Language Providers

1. Create new module: `renovatio-provider-<language>`
2. Implement `LanguageProvider` interface
3. Define language-specific recipes
4. Register with `LanguageProviderRegistry`

### Extending MCP Functionality

1. Add new methods to `McpProtocolService`
2. Update `McpCapabilities` 
3. Add corresponding tests
4. Update documentation

This architecture provides maximum flexibility while maintaining clean separation between protocol concerns and business logic.