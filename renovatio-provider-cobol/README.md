# COBOL to Java Migration Provider

## Overview

The COBOL Provider is a comprehensive extension to Renovatio that adds capabilities for migrating COBOL applications to Java. It implements all the architectural components suggested in the requirements, providing a robust and scalable solution for COBOL modernization.

## Features

### 🔍 COBOL Analysis & Parsing
- **Pluggable parsers** with ProLeap (default) and Koopa dialects seleccionables via perfiles de build
- **AST extraction** for COBOL programs, data divisions, and procedure divisions
- **Symbol detection** for data items, paragraphs, sections, and program structures
- **Dependency analysis** across COBOL programs
- **CICS command detection** for identifying transactional calls

### ☕ Java Code Generation
- **JavaPoet integration** for type-safe Java code generation
- **DTO generation** from COBOL data structures with proper type mapping
- **Service interface generation** with business logic templates
- **REST controller generation** with OpenAPI documentation
- **MapStruct mapper generation** for seamless data transformation

### 📊 Migration Planning & Execution
- **Plan/Apply pattern** for controlled migration execution
- **Dry-run capabilities** for safe migration testing
- **Step-by-step migration** with rollback capabilities
- **Progress tracking** and execution monitoring

### 🔍 Indexing & Search
- **Apache Lucene integration** for fast symbol and code search
- **Full-text search** across COBOL programs
- **Symbol occurrence tracking** for impact analysis
- **Cross-reference analysis** for dependency mapping

### 📈 Code Metrics & Analysis
- **Complexity analysis** including cyclomatic complexity
- **Code quality metrics** (lines of code, file counts, etc.)
- **Migration complexity assessment** for effort estimation
- **Dependency analysis** for migration planning

### 🤖 LLM Integration (Optional)
- **LangChain4j integration** for AI-assisted migration
- **Natural language to NQL translation** for intuitive queries
- **Migration advice** and best practice suggestions
- **Business logic explanation** for better understanding

### 🛡️ Resilience & Monitoring
- **Resilience4j integration** with circuit breakers, retries, and timeouts
- **Micrometer metrics** with Prometheus export
- **Health checks** and operational monitoring
- **Graceful degradation** with fallback mechanisms

### 🔧 Template-based Code Generation
- **Freemarker templates** for sophisticated code generation
- **Customizable templates** for different target frameworks
- **Multi-file generation** with proper package structure
- **Test code generation** for migration validation
- **REST endpoint generation** for CICS transactions

### 🔌 CICS Integration
- **Configurable CICS service** with real or mock implementations via `renovatio.cics.*` properties
- **Automatic endpoint scaffolding** for detected transactions

#### Manual invocation example

Once a controller has been generated and the CICS service is configured for
real connectivity (`renovatio.cics.mock=false`), transactions can be tested
manually.  The generated endpoints follow the pattern
`POST /api/cics/<transaction>` using lower-case transaction names.

Example:

```bash
curl -X POST \
  -H "Content-Type: application/json" \
  -d '{"account":"12345"}' \
  http://localhost:8080/api/cics/tran1
```

The request above is routed through `RealCicsService`, which maps the
`TRAN1` transaction to the corresponding Zowe CICS/JCICS REST endpoint.

## Dialect Profiles

Different COBOL dialects can be selected at build time.

### Maven

- ProLeap (predeterminado):
  ```bash
  mvn -pl renovatio-provider-cobol -Pproleap test
  ```
- Koopa:
  ```bash
  mvn -pl renovatio-provider-cobol -Pkoopa test
  ```

### Gradle

Usando la propiedad `dialect`:
```bash
gradle :renovatio-provider-cobol:test             # ProLeap
gradle :renovatio-provider-cobol:test -Pdialect=koopa
```

Para ejemplos de árboles de sintaxis generados consulta [AST_EXAMPLES.md](AST_EXAMPLES.md).

## Architecture

```
renovatio-provider-cobol/
├── src/main/java/org/shark/renovatio/provider/cobol/
│   ├── CobolLanguageProvider.java          # Main provider implementation
│   ├── domain/                             # COBOL domain models
│   │   ├── CobolProgram.java               # COBOL program representation
│   │   └── CobolDataItem.java              # COBOL data structures
│   ├── service/                            # Core services
│   │   ├── CobolParsingService.java        # COBOL parsing and analysis
│   │   ├── JavaGenerationService.java     # Java code generation
│   │   ├── MigrationPlanService.java      # Migration planning
│   │   ├── IndexingService.java           # Lucene-based indexing
│   │   ├── MetricsService.java            # Code metrics calculation
│   │   ├── TemplateCodeGenerationService.java # Template-based generation
│   │   ├── LlmIntegrationService.java     # LLM integration
│   │   └── ResilientMigrationService.java # Resilient operations
│   └── infrastructure/                     # Configuration and MCP integration
│       ├── CobolProviderConfiguration.java # Spring configuration
│       └── CobolMcpToolsProvider.java     # MCP tools integration
└── src/main/resources/
    └── application-cobol.yml              # Configuration settings
```

## Capabilities

The provider implements all `LanguageProvider` capabilities:

- ✅ **ANALYZE**: Extract COBOL program structure and metadata
- ✅ **PLAN**: Create detailed migration execution plans
- ✅ **APPLY**: Execute migration plans with dry-run support
- ✅ **DIFF**: Generate semantic and unified diffs
- ✅ **STUBS**: Generate Java interface stubs and DTOs
- ✅ **METRICS**: Calculate code quality and complexity metrics

## MCP Tools Integration

The provider exposes the following MCP tools:

### `cobol.analyze`
Analyzes COBOL programs and extracts structural information.

**Parameters:**
- `workspacePath` (required): Path to COBOL workspace
- `query`: Analysis query
- `includeMetrics`: Include code metrics in analysis

### `cobol.generate.stubs`
Generates Java interface stubs from COBOL programs.

**Parameters:**
- `workspacePath` (required): Path to COBOL workspace
- `targetPackage`: Java package for generated code
- `generateTests`: Generate test classes

### `cobol.migration.plan`
Creates a migration plan for COBOL to Java transformation.

**Parameters:**
- `workspacePath` (required): Path to COBOL workspace
- `migrationStrategy`: Migration strategy (full, incremental, hybrid)
- `targetFramework`: Target Java framework

### `cobol.db2.migrate`
Generates JPA entity and repository classes from embedded DB2 `EXEC SQL` blocks.

**Parameters:**
- `workspacePath` (required): Path to COBOL workspace
- `program` (required): COBOL program file containing SQL statements

### `cobol.migration.apply`
Applies a migration plan to transform COBOL to Java.

**Parameters:**
- `planId` (required): Migration plan ID
- `dryRun`: Execute as dry run
- `outputPath`: Output path for generated Java code

### `cobol.metrics`
Calculates code metrics for COBOL programs.

**Parameters:**
- `workspacePath` (required): Path to COBOL workspace
- `includeComplexity`: Include cyclomatic complexity
- `includeDependencies`: Include dependency analysis

### `cobol.diff`
Generates diff for migration changes.

**Parameters:**
- `runId` (required): Migration run ID
- `format`: Diff format (unified, semantic, both)

## Usage Examples

### Basic COBOL Analysis
```bash
curl -X POST -H "Content-Type: application/json" \
  -d '{
    "jsonrpc": "2.0",
    "id": "1",
    "method": "tools/call",
    "params": {
      "name": "cobol.analyze",
      "arguments": {
        "workspacePath": "/path/to/cobol/project",
        "includeMetrics": true
      }
    }
  }' \
  http://localhost:8181/
```

### Generate Java Stubs
```bash
curl -X POST -H "Content-Type: application/json" \
  -d '{
    "jsonrpc": "2.0",
    "id": "2",
    "method": "tools/call",
    "params": {
      "name": "cobol.generate.stubs",
      "arguments": {
        "workspacePath": "/path/to/cobol/project",
        "targetPackage": "com.example.cobol.migrated",
        "generateTests": true
      }
    }
  }' \
  http://localhost:8181/
```

### Create Migration Plan
```bash
curl -X POST -H "Content-Type: application/json" \
  -d '{
    "jsonrpc": "2.0",
    "id": "3",
    "method": "tools/call",
    "params": {
      "name": "cobol.migration.plan",
      "arguments": {
        "workspacePath": "/path/to/cobol/project",
        "migrationStrategy": "incremental",
        "targetFramework": "spring-boot"
      }
    }
  }' \
  http://localhost:8181/
```

## Configuration

The provider can be configured through `application-cobol.yml`:

```yaml
renovatio:
  cobol:
    parser:
      max-file-size: 10MB
      parallel-processing: true
    generation:
      target-package: org.shark.renovatio.generated.cobol
      generate-tests: true
    migration:
      default-strategy: incremental
      backup-original: true
  llm:
    enabled: false  # Enable for AI assistance
    provider: openai
    model: gpt-3.5-turbo
```

## Implementation Status

### ✅ Completed Features
- Core COBOL provider implementation
- Basic COBOL parsing with pattern matching
- Java code generation with JavaPoet
- Migration planning and execution
- Lucene-based indexing and search
- Code metrics calculation
- Resilience4j integration
- MCP tools integration
- Template-based code generation
- Configuration and monitoring setup

### 🚧 Future Enhancements
- ProLeap COBOL parser integration for production-grade parsing
- ANTLR4 grammar for NQL validation
- GnuCOBOL baseline integration for test validation
- Advanced template customization
- Real-time migration monitoring dashboard
- Integration with external code repositories

## Testing

The provider includes comprehensive unit tests:

- `CobolLanguageProviderTest`: Tests core provider functionality
- `JavaGenerationServiceTest`: Tests Java code generation with sample COBOL

Run tests with:
```bash
mvn test -pl renovatio-provider-cobol
```

## Contributing

When contributing to the COBOL provider:

1. Follow the existing architecture patterns
2. Add unit tests for new functionality
3. Update documentation for new features
4. Ensure backward compatibility with existing MCP tools
5. Follow the minimal change principle

## Integration with Renovatio

The COBOL provider integrates seamlessly with the existing Renovatio architecture:

- **Provider Registry**: Automatically registered as a language provider
- **MCP Protocol**: Exposes tools through the existing MCP infrastructure
- **Natural Query Language**: Supports NQL queries for COBOL operations
- **Workspace Management**: Works with existing workspace abstractions
- **Monitoring**: Integrates with existing metrics and health check endpoints

This implementation provides a solid foundation for COBOL to Java migration while maintaining the architectural principles and patterns established in Renovatio.