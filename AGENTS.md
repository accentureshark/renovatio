# AGENTS.md – Renovatio Agent & Automation Integration Guide

This document provides guidance for integrating, extending, and automating Renovatio using agents, LLMs, and external tools. It is designed for developers, platform engineers, and automation agents who wish to leverage Renovatio's MCP-compliant APIs and modular architecture for advanced code refactoring, migration, and modernization workflows.

## Overview

Renovatio is a multi-language refactoring and migration platform, fully compatible with the Model Content Protocol (MCP). It exposes a rich set of tools and APIs for Java and COBOL, and is designed for extensibility, automation, and interoperability with agent-based systems, LLMs, and developer platforms.

## Technology Stack

- **Java 17+** (core language)
- **Spring Boot** (REST API, dependency injection, configuration management)
- **Maven** (build, dependency management)
- **OpenRewrite** (Java refactoring and code analysis)
- **ProLeap/Koopa** (COBOL parsing)
- **MapStruct** (DTO mapping)
- **Freemarker** (template-based code generation)
- **Apache Lucene** (search and indexing)
- **DB2, JPA/Hibernate** (database migration and ORM)
- **Zowe/JCICS** (CICS integration)
- **Shell, GitHub Actions, Spring Batch, Airflow** (JCL conversion and automation)
- **JUnit 5 & RestAssured** (testing, including integration and REST endpoint tests)
- **OpenAPI/Swagger** (API documentation)

## Architecture & Extensibility

- **Modular:** Each language and provider is a separate module (core, provider-cobol, provider-java, shared, web).
- **Layered:** Controller (REST/MCP), Service, Repository, Model/Entity.
- **MCP-Compliant:** All APIs and tools use MCP schemas for input/output, supporting JSON-RPC 2.0.
- **Extensible:** New languages, tools, and automation agents can be added as MCP modules.
- **Configuration:** Managed via `application.yml`.
- **Interoperability:** Designed for integration with MCP clients, LLMs, CI/CD, and agent-based platforms.

## Agent & Automation Integration

- **API-first:** All functionality is exposed via REST and JSON-RPC endpoints, documented with OpenAPI/Swagger.
- **Tool Discovery:** Agents can list, describe, and invoke all available tools dynamically via the MCP API.
- **Input/Output Schemas:** All tools provide machine-readable schemas for arguments and results, enabling safe automation.
- **Idempotency & Safety:** Tools are designed for safe, repeatable automation. Mutating tools can be restricted via configuration.
- **Observability:** Logging, metrics, and health endpoints are available for monitoring and orchestration.
- **Testing:** Integration and system tests are provided (JUnit 5, RestAssured) to validate agent workflows.

## Best Practices for Agents

- Always use the `tools/list` and `tools/describe` endpoints to discover and understand available tools and their schemas.
- Use English for all automation scripts, comments, and documentation.
- Prefer configuration via `application.yml` and environment variables for agent-specific settings.
- Ensure all automation and agent workflows are MCP-compliant and respect input/output schemas.
- Document any new agent, integration, or automation workflow in the project documentation.
- Use the provided health and metrics endpoints to monitor agent-driven operations.

## Example Agent Workflow

1. **Discover tools:** Call `tools/list` to enumerate available tools.
2. **Describe tool:** Call `tools/describe` with the tool name to get its input schema.
3. **Prepare arguments:** Build arguments according to the schema (e.g., workspacePath, planId, etc.).
4. **Invoke tool:** Call `tools/call` with the tool name and arguments.
5. **Process results:** Parse the result or error, and take further actions as needed.
6. **Monitor:** Use health and metrics endpoints for observability.

## References

- [Model Content Protocol (MCP)](https://modelcontent.dev/)
- [agents.md best practices](https://agents.md/)
- [OpenRewrite](https://docs.openrewrite.org/)
- [ProLeap/Koopa](https://github.com/uwol/proleap-cobol-parser)
- [Spring Boot](https://spring.io/projects/spring-boot)

---

For further details, see the main README, architecture documentation, and [copilot-instructions.md].

