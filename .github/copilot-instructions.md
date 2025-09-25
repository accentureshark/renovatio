# Copilot Instructions

This project is Renovatio, a multi-language refactoring and migration platform fully compatible with the Model Content
Protocol (MCP) standard. Renovatio exposes advanced refactoring and migration tools for Java (via OpenRewrite) y COBOL (
via parsers especializados y generación de código), facilitando la modernización de aplicaciones legacy y la
refactorización automatizada de código. Todas las APIs y herramientas cumplen con MCP y JSON-RPC 2.0.

## Tecnologías y herramientas clave

- Java 17+
- Spring Boot (API REST, inyección de dependencias, configuración)
- Maven (build, gestión de dependencias)
- OpenRewrite (refactorización Java)
- ProLeap/Koopa (parsing COBOL)
- MapStruct (mapeo DTO)
- Freemarker (generación de código basada en plantillas)
- Apache Lucene (búsqueda e indexación)
- DB2, JPA/Hibernate (migración de base de datos)
- Zowe/JCICS (integración CICS)
- Shell, GitHub Actions, Spring Batch, Airflow (conversión JCL)
- JUnit 5 & RestAssured (testing)
- OpenAPI/Swagger (documentación de API)

## Arquitectura

- Modular: renovatio-core, renovatio-provider-cobol, renovatio-provider-java, renovatio-shared, renovatio-web.
- Por capas: Controller (REST/MCP), Service, Repository, Model/Entity.
- Extensible: nuevos lenguajes y herramientas pueden agregarse como módulos MCP.
- Configuración gestionada vía `application.yml`.
- Todos los endpoints y herramientas siguen los esquemas MCP para entrada/salida.

## Guía de codificación

- Usa inglés para comentarios, documentación y nombres de identificadores.
- Sigue buenas prácticas de Java y Spring Boot (naming, inyección de dependencias, manejo de excepciones).
- Escribe código modular, limpio y bien documentado.
- Usa mensajes de commit significativos (ver `git-commit-instructions.md`).
- Mantén y escribe tests para nuevas funcionalidades (JUnit 5, RestAssured para endpoints REST).
- Asegura que todo el código y endpoints cumplan el estándar MCP para los esquemas de entrada/salida.
- Prefiere la configuración vía `application.yml`.
- Documenta cualquier nueva herramienta, endpoint o módulo en el README y documentación relevante.

## Contexto de negocio

- Renovatio provee APIs unificadas de refactorización, migración y generación de código para modernización de
  aplicaciones legacy.
- Soporta Java y COBOL, con extensibilidad para otros lenguajes y herramientas.
- Diseñado para interoperabilidad con clientes MCP (VS Code, Copilot Workspace, etc.).
- Permite migración automatizada, refactorización y análisis de código para aplicaciones empresariales.

## Al usar GitHub Copilot:

- Sigue el estilo y convenciones existentes del código.
- Usa inglés para comentarios y documentación.
- Asegura que el código sea limpio, modular y bien documentado.
- Escribe mensajes de commit significativos (ver `git-commit-instructions.md`).
- Prefiere la configuración vía `application.yml`.
- Si agregas nuevas funcionalidades, actualiza la documentación (README, docs).
- Asegura que todo el código y endpoints nuevos sean MCP-compliant.
