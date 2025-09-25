# Renovatio Shared

Common domain models, DTOs, and utilities shared across all Renovatio modules.

## Overview

`renovatio-shared` provides the foundational abstractions and data structures used throughout the Renovatio platform:

- **Domain models**: Core entities like `Tool`, `Workspace`, `AnalyzeResult`, etc.
- **SPI interfaces**: Service Provider Interface for language providers
- **NQL support**: Natural Query Language parsing and compilation
- **Utility classes**: Common utilities shared across modules

## Key Components

### Domain Models
- `Tool`: Represents a refactoring or migration tool
- `Workspace`: Represents a project workspace
- `AnalyzeResult`, `ApplyResult`, `PlanResult`: Result types for different operations
- `MigrationReport`: Comprehensive migration reporting structure

### SPI (Service Provider Interface)
- `LanguageProvider`: Interface that language providers must implement
- `LanguageProvider.Capabilities`: Enum defining provider capabilities (ANALYZE, PLAN, APPLY, etc.)

### NQL (Natural Query Language)
- `NqlQuery`: Represents a parsed NQL query
- `NqlParserService`: Service for parsing and compiling NQL queries
- `NqlCompileResult`: Result of NQL compilation

## Usage

This module is automatically included as a dependency in all other Renovatio modules:

```xml
<dependency>
    <groupId>org.shark.renovatio</groupId>
    <artifactId>renovatio-shared</artifactId>
    <version>0.0.1-SNAPSHOT</version>
</dependency>
```

## Language Provider Implementation

To create a new language provider, implement the `LanguageProvider` interface:

```java
@Component
public class MyLanguageProvider implements LanguageProvider {
    @Override
    public String getLanguage() {
        return "mylang";
    }
    
    @Override
    public Set<Capabilities> getCapabilities() {
        return Set.of(Capabilities.ANALYZE, Capabilities.PLAN, Capabilities.APPLY);
    }
    
    @Override
    public AnalyzeResult analyze(Workspace workspace, NqlQuery query, Scope scope) {
        // Implementation
    }
    
    // Other capability methods...
}
```