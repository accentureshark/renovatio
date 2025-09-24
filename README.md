# Renovatio - Plataforma de Refactorización y Migración Multi-Lenguaje (MCP-Compliant)

**Renovatio** es una plataforma modular y extensible para la modernización y refactorización automatizada de aplicaciones legacy, compatible con el estándar Model Content Protocol (MCP) y JSON-RPC 2.0. Provee herramientas avanzadas para Java (OpenRewrite), COBOL (parsing, generación de código, migración) y JCL (traducción a workflows modernos).

---

## Arquitectura General

Renovatio está organizado como un proyecto multi-módulo Maven, siguiendo una arquitectura por capas y basada en proveedores (providers):

```
renovatio/
├── renovatio-shared/         # Modelos, DTOs y utilidades compartidas
├── renovatio-core/           # Núcleo de lógica de negocio y orquestación (protocol-agnostic)
├── renovatio-provider-java/  # Proveedor Java (OpenRewrite)
├── renovatio-provider-cobol/ # Proveedor COBOL (parsing, migración)
├── renovatio-provider-jcl/   # Proveedor JCL (parsing, conversión)
├── renovatio-mcp-server/     # Servidor MCP (implementación de protocolo)
├── renovatio-agent/          # Agente de ejecución distribuida/batch
├── renovatio-client/         # Cliente MCP standalone
└── ...
```

---

## Responsabilidad de cada módulo Maven

### renovatio-shared
- **Responsabilidad:** Modelos de dominio, DTOs, utilidades, interfaces base y parser NQL compartidos por todos los módulos.
- **Incluye:** Entidades MCP, contratos SPI, lógica de validación, parser ANTLR4 para NQL.

### renovatio-core
- **Responsabilidad:** Lógica de negocio central, orquestación de herramientas, registro de providers, rutinas de análisis y migración independientes de protocolo.
- **Incluye:** Servicios de aplicación, registro de herramientas, rutinas de análisis, integración con Lucene, lógica de orquestación Plan/Apply.

### renovatio-provider-java
- **Responsabilidad:** Proveedor de lenguaje Java, integración con OpenRewrite, exposición de recetas y herramientas MCP para Java.
- **Incluye:** Análisis estático, refactorización, migración de versiones, métricas, integración con MapStruct, generación de herramientas MCP dinámicas.

### renovatio-provider-cobol
- **Responsabilidad:** Proveedor COBOL, parsing avanzado, extracción de AST, generación de código Java, planificación y ejecución de migración.
- **Incluye:** Parsing con ProLeap/Koopa, generación de DTOs, servicios, controladores REST, integración con DB2, CICS, copybooks, métricas y resiliencia.

### renovatio-provider-jcl
- **Responsabilidad:** Parsing y traducción de scripts JCL a shell, GitHub Actions, Spring Batch o Airflow.
- **Incluye:** Parser JCL, generación de AST, herramientas MCP para conversión de workflows batch.

### renovatio-mcp-server
- **Responsabilidad:** Exposición de APIs MCP (JSON-RPC 2.0), documentación OpenAPI, controladores, manejo de errores y configuración Spring Boot. Implementación completa del protocolo MCP que expone el motor core.

### renovatio-agent
- **Responsabilidad:** Ejecución distribuida o en segundo plano de jobs de migración/refactorización, integración con JGit, monitoreo y reporting.

### renovatio-client
- **Responsabilidad:** Cliente MCP standalone para invocación de herramientas y pruebas de integración.

- **Incluye:** Servidor Spring Boot, controladores MCP, implementación de especificación MCP 2025-06-18, integración con renovatio-core y todos los providers.

---

## Tecnologías y Tooling

- **Java 17+**
- **Spring Boot 3.2.x**
- **Maven** (multi-módulo)
- **OpenRewrite** (refactorización Java)
- **ProLeap/Koopa** (parsing COBOL)
- **MapStruct** (mapeo DTOs)
- **Freemarker** (plantillas de generación de código)
- **Apache Lucene** (búsqueda e indexación)
- **DB2, JPA/Hibernate** (migración de base de datos)
- **Zowe/JCICS** (integración CICS)
- **Shell, GitHub Actions, Spring Batch, Airflow** (conversión JCL)
- **JUnit 5, RestAssured** (testing)
- **OpenAPI/Swagger** (documentación)
- **Resilience4j, Micrometer, JGit, ANTLR4**

---

## Protocolos y Cumplimiento MCP

- **MCP 2025-06-18**: Cumplimiento total de la especificación, incluyendo todos los métodos (`initialize`, `tools/list`, `tools/call`, `prompts/list`, `resources/read`, etc.).
- **JSON-RPC 2.0**: Todas las respuestas y errores siguen el estándar.
- **Esquemas de entrada/salida**: Cada herramienta expone su `inputSchema` y `outputSchema` para autodescubrimiento y generación dinámica de clientes.
- **Prompts y recursos**: Listado y acceso a prompts y recursos del servidor.
- **Workspaces y contenido**: Métodos para listar, describir y manipular workspaces y archivos.
- **Documentación OpenAPI**: Swagger UI disponible para exploración interactiva.

---

## Ejemplo de Estructura de Proyecto

```
renovatio/
├── renovatio-shared/        # Modelos, DTOs, NQL, utilidades
├── renovatio-core/          # Lógica de negocio, orquestación, registro de herramientas (protocol-agnostic)
├── renovatio-provider-java/ # Refactorización y análisis Java (OpenRewrite)
├── renovatio-provider-cobol/# Parsing, migración y generación desde COBOL
├── renovatio-provider-jcl/  # Parsing y conversión de JCL
├── renovatio-mcp-server/    # Servidor MCP, integración de todos los módulos
├── renovatio-agent/         # Ejecución distribuida, integración JGit
├── renovatio-client/        # Cliente MCP standalone
└── ...
```

---

## Cumplimiento y Extensibilidad

- **Extensible**: Nuevos lenguajes y herramientas pueden agregarse como módulos MCP.
- **Configuración centralizada**: Preferencia por `application.yml` y variables de entorno.
- **Testing y calidad**: JUnit 5, RestAssured, cobertura de tests para nuevas funcionalidades.
- **Documentación**: README, OpenAPI, y documentación modular por cada provider.

---

## Ejemplo de Uso MCP (JSON-RPC 2.0)

### Inicializar conexión MCP

```bash
curl -X POST -H "Content-Type: application/json" \
  -d '{"jsonrpc": "2.0", "id": "1", "method": "initialize", "params": {}}' \
  http://localhost:8181/
```

### Listar herramientas disponibles

```bash
curl -X POST -H "Content-Type: application/json" \
  -d '{"jsonrpc": "2.0", "id": "2", "method": "tools/list", "params": {}}' \
  http://localhost:8181/
```

### Ejecutar herramienta Java (OpenRewrite)

```bash
curl -X POST -H "Content-Type: application/json" \
  -d '{
    "jsonrpc": "2.0",
    "id": "3",
    "method": "tools/call",
    "params": {
      "name": "org.openrewrite.java.format.AutoFormat",
      "arguments": {
        "sourceCode": "public class Test{private int x=0;public void test(){System.out.println(\"Hello\");}}"
      }
    }
  }' \
  http://localhost:8181/
```

### Analizar programa COBOL

```bash
curl -X POST -H "Content-Type: application/json" \
  -d '{
    "jsonrpc": "2.0",
    "id": "4",
    "method": "tools/call",
    "params": {
      "name": "cobol.analyze",
      "arguments": {
        "workspacePath": "/path/to/cobol/project",
        "includeMetrics": true,
        "query": "FIND DATA-ITEMS WHERE USAGE IS COMP-3"
      }
    }
  }' \
  http://localhost:8181/
```

---

## Configuración y Variables de Entorno

- `SERVER_PORT`: Puerto del servidor (por defecto 8181)
- `SPRING_PROFILES_ACTIVE`: Perfil activo de Spring
- `RENOVATIO_COBOL_PARSER_MAX_FILE_SIZE`: Tamaño máximo de archivo COBOL (por defecto 10MB)
- `RENOVATIO_COBOL_GENERATION_TARGET_PACKAGE`: Paquete Java objetivo por defecto
- `RENOVATIO_COBOL_MIGRATION_DEFAULT_STRATEGY`: Estrategia de migración por defecto (incremental)

---

## Documentación y Recursos

- **OpenAPI/Swagger UI**: [http://localhost:8181/swagger-ui/index.html](http://localhost:8181/swagger-ui/index.html)
- **MCP Spec**: [Model Content Protocol](https://modelcontentprotocol.io/specification/2025-06-18/)
- **Documentación modular**: Ver README y docs en cada módulo provider.

---

**Renovatio** – Plataforma unificada para refactorización multi-lenguaje y modernización de aplicaciones legacy, 100% MCP-compliant.

### 🌟 Casos de Uso Principales

- **Modernización de Mainframe**: Migración completa de aplicaciones COBOL a arquitecturas Java modernas
- **Refactorización Enterprise**: Actualización de aplicaciones Java legacy a versiones modernas
- **Análisis de Código Legacy**: Comprensión profunda de aplicaciones complejas antes de migración
- **Automatización DevOps**: Integración en pipelines CI/CD para refactorización continua
- **Evaluación de Migración**: Análisis de complejidad y estimación de esfuerzo para proyectos de modernización

### 🎯 Para Quién es Renovatio

- **Arquitectos de Software**: Planificación y diseño de migraciones complejas
- **Desarrolladores Senior**: Herramientas avanzadas para refactorización y modernización
- **Equipos DevOps**: Automatización de procesos de migración y refactorización
- **CTOs y Gerentes Técnicos**: Visibilidad y control sobre proyectos de modernización
- **Consultores de Migración**: Herramientas profesionales para evaluación y ejecución