# Renovatio Core

The core engine of Renovatio - a protocol-agnostic refactoring and migration platform.

## Overview

`renovatio-core` is the heart of the Renovatio platform, providing:

- **Protocol-agnostic architecture**: No dependencies on MCP or any specific protocol
- **Language provider registry**: Dynamic registration and discovery of language providers  
- **Tool orchestration**: Unified interface for executing refactoring and migration tools
- **Provider lifecycle management**: Registration, initialization, and execution coordination

## Key Components

### LanguageProviderRegistry
Central registry that manages all language providers and routes tool calls to the appropriate provider.

```java
LanguageProviderRegistry registry = new LanguageProviderRegistry();
List<Tool> tools = registry.generateTools();
Map<String, Object> result = registry.routeToolCall("java.analyze", arguments);
```

### Service Layer
- `MigrationReportService`: Handles migration reporting and analytics
- `ReportAccessService`: Manages access to migration reports

## Usage as Library

Add to your Maven project:

```xml
<dependency>
    <groupId>org.shark.renovatio</groupId>
    <artifactId>renovatio-core</artifactId>
    <version>0.0.1-SNAPSHOT</version>
</dependency>
```

Use directly in your application:

```java
@Component
public class MyRefactoringService {
    private final LanguageProviderRegistry coreEngine;
    
    public MyRefactoringService() {
        this.coreEngine = new LanguageProviderRegistry();
    }
    
    public Map<String, Object> refactorCode(String language, String operation, Map<String, Object> params) {
        String toolName = language + "." + operation;
        return coreEngine.routeToolCall(toolName, params);
    }
}
```

## Architecture Principles

- **Separation of concerns**: Core logic is independent of any protocol (MCP, REST, GraphQL)
- **Extensibility**: Easy to add new language providers
- **Testability**: All components are testable in isolation
- **Performance**: Efficient tool routing and execution