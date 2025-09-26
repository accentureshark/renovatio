# Renovatio – Copilot & Agent Coding Instructions

Renovatio is a multi-language refactoring and migration platform, fully compatible with the Model Content Protocol (MCP) standard. It provides advanced refactoring and migration tools for Java (via OpenRewrite) and COBOL (via specialized parsers and code generation), enabling legacy application modernization and automated code refactoring. All APIs and tools are MCP-compliant and use JSON-RPC 2.0.

## Technology Stack

- **Java 17+** (core language)
- **Spring Boot** (REST API, dependency injection, configuration management)
- **Maven** (build, dependency management)
- **OpenRewrite** (Java refactoring and code analysis)
- **ProLeap/Koopa** (COBOL parsing)
- **MapStruct** (DTO mapping)
- **Lombok** (automatic generation of DTOs and entity boilerplate)
- **Freemarker** (template-based code generation)
- **Apache Lucene** (search and indexing)
- **DB2, JPA/Hibernate** (database migration and ORM)
- **Zowe/JCICS** (CICS integration)
- **Shell, GitHub Actions, Spring Batch, Airflow** (JCL conversion and automation)
- **JUnit 5 & RestAssured** (testing, including integration and REST endpoint tests)
- **OpenAPI/Swagger** (API documentation)

## Architecture & Design

- **Modular structure:**
  - `renovatio-core`: Core logic and shared services
  - `renovatio-provider-cobol`: COBOL language provider and migration tools
  - `renovatio-provider-java`: Java language provider and refactoring tools
  - `renovatio-shared`: Shared models, DTOs, and utilities
  - `renovatio-web`: Web and API layer
- **Layered design:**
  - Controller (REST/MCP endpoints)
  - Service (business logic)
  - Repository (data access)
  - Model/Entity (domain models)
- **Extensible:** New languages and tools can be added as MCP modules.
- **Configuration:** Managed via `application.yml` for all modules.
- **MCP Compliance:** All endpoints and tools follow MCP schemas for input/output.
- **Interoperability:** Designed for integration with MCP clients (VS Code, Copilot Workspace, etc.).

## Coding Guidelines

- Use English for all comments, documentation, and identifier names.
- Follow Java and Spring Boot best practices (naming, dependency injection, exception handling).
- Write modular, clean, and well-documented code.
- Use meaningful commit messages (see `git-commit-instructions.md`).
- Maintain and write tests for all new features (JUnit 5, RestAssured for REST endpoints).
- Ensure all code and endpoints are MCP-compliant for input/output schemas.
- Prefer configuration via `application.yml`.
- Document any new tool, endpoint, or module in the README and relevant documentation.

## Business Context

- Renovatio provides unified APIs for refactoring, migration, and code generation to modernize legacy applications.
- Supports Java and COBOL, with extensibility for additional languages and tools.
- Enables automated migration, refactoring, and code analysis for enterprise applications.
- Designed for seamless interoperability with MCP clients and agent-based workflows.

## When using GitHub Copilot or other agents:

- Follow the existing code style and conventions.
- Use English for all comments and documentation.
- Ensure code is clean, modular, and well-documented.
- Write meaningful commit messages (see `git-commit-instructions.md`).
- Prefer configuration via `application.yml`.
- If you add new features, update the documentation (README, docs).
- Ensure all new code and endpoints are MCP-compliant.

---

For more details on agent interoperability and best practices, see [agents.md](https://agents.md/).
