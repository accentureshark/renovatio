# agents.md

## Project Overview

**Renovatio** is a modular, extensible platform for automated refactoring, migration, and code analysis, designed for enterprise legacy modernization. It is fully compliant with the Model Content Protocol (MCP) standard, ensuring seamless interoperability with MCP clients and automation tools. Renovatio currently supports Java (via OpenRewrite) and COBOL (via specialized parsers and code generation), and is architected for rapid adoption of additional languages and tools.

---

## MCP Compliance

- **Strict adherence** to MCP and JSON-RPC 2.0 for all APIs and tools.
- **Input/output schemas** for every endpoint and tool are MCP-compliant, enabling robust automation and integration with MCP clients (e.g., VS Code, Copilot Workspace).
- **Consistent data exchange** and process orchestration, ensuring predictable, reliable automation.

---

## Key Technologies & Tools

- **Java 17+**: Core backend and provider modules.
- **Spring Boot**: REST API, dependency injection, configuration management.
- **Maven**: Build and dependency management.
- **OpenRewrite**: Automated, rule-based Java refactoring and migration.
- **ProLeap/Koopa**: COBOL parsing and analysis.
- **MapStruct**: DTO mapping.
- **Freemarker**: Template-based code generation (COBOL, Java, others).
- **Apache Lucene**: Search and indexing.
- **DB2, JPA/Hibernate**: Database migration and persistence.
- **Zowe/JCICS**: CICS integration for mainframe interoperability.
- **Shell, GitHub Actions, Spring Batch, Airflow**: JCL conversion and batch processing.
- **JUnit 5, RestAssured**: Unit, integration, and REST API testing.
- **OpenAPI/Swagger**: API documentation and discovery.

---

## Architecture

- **Modular**: 
  - `renovatio-core`: Core logic, shared utilities, MCP abstractions.
  - `renovatio-provider-java`: Java refactoring/migration (OpenRewrite integration).
  - `renovatio-provider-cobol`: COBOL parsing, analysis, and code generation.
  - `renovatio-shared`: Shared models, DTOs, and utilities.
  - `renovatio-web`: REST API and web layer.
- **Layered**: 
  - Controller (REST/MCP)
  - Service
  - Repository
  - Model/Entity
- **Extensible**: 
  - New languages and tools can be added as MCP modules.
  - Configuration is managed via `application.yml`.

---

## OpenRewrite Integration

- **OpenRewrite** is the core engine for Java code refactoring and migration.
- Provides automated, rule-based transformations for Java source code, supporting large-scale, safe, and repeatable modernization.
- Exposed via MCP-compliant APIs, enabling LLM agents and automation tools to trigger and orchestrate refactorings.
- Supports custom recipes and integration with other Java analysis tools.

---

## COBOL Support

- **ProLeap/Koopa**: Used for COBOL parsing and AST generation.
- **Code generation**: Freemarker templates enable automated COBOL-to-Java migration and legacy code modernization.
- **Extensible**: Designed to support additional legacy languages and mainframe technologies.

---

## Flexibility & Extensibility

- **Language-agnostic**: Architecture allows for rapid addition of new language providers and tools.
- **Tooling**: New refactoring, migration, or analysis tools can be integrated as MCP-compliant modules.
- **Configuration**: All modules and endpoints are configurable via `application.yml`.
- **MCP-first**: All new features and modules must be MCP-compliant by design.

---

## Business Context

- **Unified APIs** for refactoring, migration, and code generation, enabling enterprise-scale legacy modernization.
- **Enterprise focus**: Designed for large, complex codebases and mainframe environments.
- **Interoperability**: MCP compliance ensures compatibility with a wide range of clients and automation tools.
- **Automated migration, refactoring, and code analysis** for Java, COBOL, and future languages.

---

## Coding Guidelines

- **English** for comments, documentation, and identifiers.
- **Clean, modular, and well-documented code**.
- **Meaningful commit messages** (see `git-commit-instructions.md`).
- **Tests** for all new features (JUnit 5, RestAssured).
- **MCP-compliant** input/output for all endpoints and tools.
- **Documentation**: Update README and relevant docs for all new features.

---

## For LLM Agents

- **All APIs and tools** are discoverable and documented via OpenAPI/Swagger.
- **MCP schemas** are available for all endpoints and tools.
- **Extensibility**: Agents can propose or implement new language providers or tools as MCP modules.
- **Automation**: Designed for integration with LLM-driven workflows and code automation agents.
- **Traceability**: All transformations, migrations, and analyses are logged and auditable.

---

## Further Documentation

- See `README.md` for getting started and usage.
- See `.github/copilot-instructions.md` for coding standards.
- See `git-commit-instructions.md` for commit message guidelines.
- All configuration is managed via `application.yml`.
- Each module contains its own `README.md` for module-specific details.

---

**Renovatio** is the foundation for automated, MCP-compliant legacy modernization, ready for integration with LLM agents and enterprise automation pipelines.

