# Renovatio - Plataforma Multi-Lenguaje de Refactorización con MCP

**Renovatio** es una plataforma completa de refactorización multi-lenguaje completamente compatible con el Protocolo de Contenido de Modelo (MCP, Model Content Protocol). Expone funcionalidades avanzadas de refactorización y migración para **Java** (mediante OpenRewrite) y **COBOL** (mediante parsers especializados y generación de código), facilitando la modernización de aplicaciones legacy y la refactorización automatizada de código.

## Características

### ✅ Protocolo MCP y Compatibilidad
- **Compatibilidad total con MCP (Model Content Protocol)**: Implementa el estándar MCP versión `2025-06-18` y el protocolo JSON-RPC 2.0 completo.
- **API REST y MCP**: Soporta tanto endpoints REST tradicionales como el protocolo MCP en la raíz (`/`).
- **Documentación OpenAPI**: Interfaz Swagger UI para explorar la API.
- **Manejo de errores robusto**: Códigos de error apropiados y mensajes informativos.
- **Esquemas de herramientas**: Definiciones completas de entrada/salida para cada herramienta.
- **Prompts y Recursos MCP**: Listado y acceso a prompts y recursos del servidor.

### ☕ Refactorización Java (OpenRewrite)
- **Exposición completa de OpenRewrite**: Todas las recetas de OpenRewrite están disponibles como herramientas MCP.
- **Formato y limpieza automática**: Herramientas de formateo y optimización de código Java.
- **Migración de versiones**: Actualizaciones automáticas entre versiones de Java (8→11→17→21).
- **Mejoras de seguridad**: Detección y corrección de vulnerabilidades de seguridad.
- **Análisis estático**: Identificación de patrones problemáticos y optimizaciones.

### 🏢 Migración COBOL a Java
- **Análisis profundo de COBOL**: Parsing avanzado con soporte para parsers ProLeap/Koopa.
- **Generación de código Java**: DTOs, servicios, controladores REST con documentación OpenAPI y mappers MapStruct automáticos.
- **Planificación de migración**: Sistema Plan/Apply con capacidades de dry-run y rollback.
- **Búsqueda e indexación**: Integración con Apache Lucene para búsqueda de símbolos y análisis de dependencias.
- **Métricas de código**: Análisis de complejidad ciclomática y evaluación de la complejidad de migración.
- **Patrones de resistencia**: Circuit breakers, reintentos y monitoreo para operaciones robustas.
- **Generación basada en plantillas**: Sistema Freemarker para generación sofisticada de código.
- **Generación de modelos desde copybooks**: Adaptadores de datasets y modelos Java creados automáticamente a partir de copybooks COBOL.
- **Migración DB2**: Conversión de SQL embebido a APIs modernas JPA/Hibernate.
- **Detección y exposición de CICS**: Identificación de comandos `EXEC CICS`, generación de controladores REST e integración con cliente Zowe/JCICS.
- **Conversión de JCL**: Parser JCL que genera AST y traduce pasos a scripts shell, GitHub Actions, Spring Batch o Airflow.

## Estándar Model Content Protocol (MCP)

Renovatio implementa el estándar **Model Content Protocol (MCP)**, permitiendo interoperabilidad con clientes modernos como VS Code, Copilot Workspace, y cualquier cliente MCP. Soporta los métodos MCP principales:

- `initialize`, `shutdown`, `ping`, `restart`
- `tools/list`, `tools/call`, `tools/describe`
- `capabilities`, `server/info`
- `content/read`, `content/write`
- `workspace/list`, `workspace/describe`
- `prompts/list`, `prompts/get`
- `resources/list`, `resources/read`

Todas las respuestas siguen el formato JSON-RPC 2.0, asegurando compatibilidad y fácil integración.

## Interoperabilidad y Compatibilidad

Renovatio es compatible con cualquier cliente MCP, incluyendo:
- **Visual Studio Code** (con extensiones MCP)
- **Copilot Workspace**
- Herramientas de automatización y análisis que soporten MCP

### Ejemplo de configuración de cliente MCP (VS Code)

Agrega la siguiente configuración en tu cliente MCP (por ejemplo, VS Code):

```json
{
  "id": "renovatio-local",
  "name": "Renovatio - Refactor MCP Server (Local)",
  "description": "Refactor MCP Server for code analysis and transformation",
  "url": "http://localhost:8181/mcp/",
  "implemented": true,
  "prewarm": true
}
```

## Herramientas Disponibles

Renovatio expone más de 23 herramientas de refactorización y migración organizadas en categorías:

### ☕ Herramientas Java (OpenRewrite)

#### 🎨 Formato y Limpieza
- `org.openrewrite.java.format.AutoFormat` - Formatear código Java automáticamente
- `org.openrewrite.java.cleanup.UnnecessaryParentheses` - Eliminar paréntesis innecesarios
- `org.openrewrite.java.cleanup.EmptyBlock` - Eliminar bloques vacíos
- `org.openrewrite.java.cleanup.ExplicitInitialization` - Eliminar inicialización explícita a valores por defecto
- `org.openrewrite.java.cleanup.FinalizePrivateFields` - Finalizar campos privados no reasignados

#### 🔧 Mejoras de Código
- `org.openrewrite.java.cleanup.BigDecimalRoundingConstantsToEnums` - Reemplazar constantes BigDecimal con enums
- `org.openrewrite.java.cleanup.BooleanChecksNotInverted` - Reemplazar verificaciones booleanas invertidas
- `org.openrewrite.java.cleanup.CaseInsensitiveComparisonsDoNotChangeCase` - Usar métodos de comparación case-insensitive
- `org.openrewrite.java.cleanup.ChainStringBuilderAppendCalls` - Encadenar llamadas StringBuilder.append
- `org.openrewrite.java.cleanup.CovariantEquals` - Usar equals covariantes

#### 🚀 Migración de Versiones
- `org.openrewrite.java.migrate.Java8toJava11` - Migrar de Java 8 a Java 11
- `org.openrewrite.java.migrate.JavaVersion11` - Actualizar a Java 11
- `org.openrewrite.java.migrate.JavaVersion17` - Actualizar a Java 17
- `org.openrewrite.java.migrate.JavaVersion21` - Actualizar a Java 21

#### 🔒 Seguridad
- `org.openrewrite.java.security.FindJdbcUrl` - Encontrar URLs JDBC
- `org.openrewrite.java.security.FindSqlInjection` - Encontrar vulnerabilidades de inyección SQL
- `org.openrewrite.java.security.SecureRandomPrefersDefaultSeed` - Usar SecureRandom con semilla por defecto

### 🏢 Herramientas COBOL (Migración a Java)

#### 🔍 Análisis y Parsing
- `cobol.analyze` - Análisis profundo de programas COBOL con extracción de AST
  - Detección de símbolos (data items, párrafos, secciones)
  - Análisis de dependencias entre programas
  - Extracción de estructura de datos y lógica de procedimientos
  - Soporte para métricas de complejidad integradas

#### ☕ Generación de Código Java
- `cobol.generate.stubs` - Generación automática de código Java desde COBOL
  - **DTOs**: Generación de clases de datos con mapeo de tipos apropiado
  - **Interfaces de servicio**: Plantillas de lógica de negocio
  - **Controladores REST**: Endpoints HTTP con documentación OpenAPI
  - **Mappers MapStruct**: Transformación automática de datos
  - **Clases de prueba**: Generación de tests para validación de migración

#### 📋 Planificación y Ejecución de Migración
- `cobol.migration.plan` - Creación de planes de migración detallados
  - Estrategias: completa, incremental, híbrida
  - Evaluación de complejidad y estimación de esfuerzo
  - Análisis de dependencias para orden de migración
  - Soporte para múltiples frameworks objetivo (Spring Boot, Quarkus, etc.)

- `cobol.migration.apply` - Ejecución controlada de planes de migración
  - Capacidades de dry-run para pruebas seguras
  - Migración paso a paso con puntos de control
  - Capacidades de rollback para recuperación
  - Monitoreo de progreso y seguimiento de ejecución

#### 📊 Métricas y Análisis
- `cobol.metrics` - Cálculo completo de métricas de código
  - **Complejidad ciclomática**: Análisis de complejidad de procedimientos
  - **Métricas de calidad**: Líneas de código, conteo de archivos, etc.
  - **Evaluación de migración**: Estimación de complejidad de migración
  - **Análisis de dependencias**: Mapeo de referencias cruzadas

#### 🔍 Comparación y Diferencias
- `cobol.diff` - Generación de diferencias para cambios de migración
  - Diffs unificados para revisión de código
  - Diffs semánticos para análisis de cambios lógicos
  - Comparación antes/después de la migración
  - Análisis de impacto de cambios

#### 📑 Copybooks y Datasets
- `cobol.copybook.migrate` - Generación de modelos y artefactos Java desde copybooks COBOL

### 📑 Herramientas JCL
- `jcl.convert` - Conversión de pasos JCL a scripts shell, GitHub Actions, Spring Batch o Airflow

## Inicio Rápido

### Ejecutar el Servidor

```bash
SERVER_PORT=8181 mvn spring-boot:run
```

El servidor estará disponible en `http://localhost:8181`

### Documentación API

Visita `http://localhost:8181/swagger-ui/index.html` para explorar la API interactivamente.

## Uso de la API

### 1. Protocolo MCP (Recomendado)

#### Inicializar Conexión MCP
```bash
curl -X POST -H "Content-Type: application/json" \
  -d '{"jsonrpc": "2.0", "id": "1", "method": "initialize", "params": {}}' \
  http://localhost:8181/
```

#### Listar Herramientas Disponibles
```bash
curl -X POST -H "Content-Type: application/json" \
  -d '{"jsonrpc": "2.0", "id": "2", "method": "tools/list", "params": {}}' \
  http://localhost:8181/
```

#### Obtener Manifiesto CLI
Permite a clientes CLI generar subcomandos automáticamente.

```bash
curl -X POST -H "Content-Type: application/json" \
  -d '{"jsonrpc": "2.0", "id": "5", "method": "cli/manifest", "params": {}}' \
  http://localhost:8181/
```

La respuesta incluye un objeto `commands` con elementos que exponen los campos `name`, `description` e `inputSchema`:

```json
{
  "jsonrpc": "2.0",
  "id": "5",
  "result": {
    "commands": [
      {
        "name": "org.openrewrite.java.format.AutoFormat",
        "description": "Automatically format Java code",
        "inputSchema": {
          "type": "object",
          "properties": {
            "sourceCode": {
              "type": "string",
              "description": "Java source code to refactor"
            },
            "projectPath": {
              "type": "string",
              "description": "Path to the project (optional)"
            }
          },
          "required": ["sourceCode"]
        }
      }
    ]
  }
}
```

Los clientes pueden iterar sobre `commands` y utilizar cada `name`, `description` e `inputSchema` para generar subcomandos CLI.

#### Ejecutar Herramienta Java (OpenRewrite)
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

#### Analizar Programa COBOL
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

#### Generar Stubs Java desde COBOL
```bash
curl -X POST -H "Content-Type: application/json" \
  -d '{
    "jsonrpc": "2.0",
    "id": "5",
    "method": "tools/call",
    "params": {
      "name": "cobol.generate.stubs",
      "arguments": {
        "workspacePath": "/path/to/cobol/project",
        "targetPackage": "com.example.cobol.migrated",
        "generateTests": true,
        "targetFramework": "spring-boot"
      }
    }
  }' \
  http://localhost:8181/
```

#### Crear Plan de Migración COBOL
```bash
curl -X POST -H "Content-Type: application/json" \
  -d '{
    "jsonrpc": "2.0",
    "id": "6",
    "method": "tools/call",
    "params": {
      "name": "cobol.migration.plan",
      "arguments": {
        "workspacePath": "/path/to/cobol/project",
        "migrationStrategy": "incremental",
        "targetFramework": "spring-boot",
        "includeComplexityAnalysis": true
      }
    }
  }' \
  http://localhost:8181/
```

#### Aplicar Plan de Migración
```bash
curl -X POST -H "Content-Type: application/json" \
  -d '{
    "jsonrpc": "2.0",
    "id": "7",
    "method": "tools/call",
    "params": {
      "name": "cobol.migration.apply",
      "arguments": {
        "planId": "migration-plan-uuid-12345",
        "dryRun": true,
        "outputPath": "/path/to/java/output",
        "backupOriginal": true
      }
    }
  }' \
  http://localhost:8181/
```

#### Calcular Métricas de COBOL
```bash
curl -X POST -H "Content-Type: application/json" \
  -d '{
    "jsonrpc": "2.0",
    "id": "8",
    "method": "tools/call",
    "params": {
      "name": "cobol.metrics",
      "arguments": {
        "workspacePath": "/path/to/cobol/project",
        "includeComplexity": true,
        "includeDependencies": true,
        "generateReport": true
      }
    }
  }' \
  http://localhost:8181/
```

#### Listar Prompts
```bash
curl -X POST -H "Content-Type: application/json" \
  -d '{"jsonrpc": "2.0", "id": "4", "method": "prompts/list", "params": {}}' \
  http://localhost:8181/
```

#### Obtener Prompt
```bash
curl -X POST -H "Content-Type: application/json" \
  -d '{"jsonrpc": "2.0", "id": "5", "method": "prompts/get", "params": {"name": "format-java"}}' \
  http://localhost:8181/
```

#### Listar Recursos
```bash
curl -X POST -H "Content-Type: application/json" \
  -d '{"jsonrpc": "2.0", "id": "6", "method": "resources/list", "params": {}}' \
  http://localhost:8181/
```

#### Leer Recurso
```bash
curl -X POST -H "Content-Type: application/json" \
  -d '{"jsonrpc": "2.0", "id": "7", "method": "resources/read", "params": {"uri": "file://welcome.txt"}}' \
  http://localhost:8181/
```

### 2. API REST (Compatibilidad)

#### Listar Herramientas
```bash
curl http://localhost:8181/mcp/tools
```

#### Ejecutar Refactorización
```bash
curl -X POST -H "Content-Type: application/json" \
  -d '{
    "sourceCode": "public class Test { private String name = null; }", 
    "recipe": "org.openrewrite.java.cleanup.ExplicitInitialization"
  }' \
  http://localhost:8181/api/refactor
```

## Estructura del Proyecto

```
renovatio/
├── renovatio-shared/                   # Modelos y utilidades compartidas
│   ├── domain/                        # Modelos de dominio base
│   └── nql/                          # Natural Query Language
├── renovatio-core/                    # Núcleo de la plataforma
│   ├── application/                   # Servicios de aplicación
│   ├── domain/                       # Modelos de dominio principales
│   └── infrastructure/               # Controladores e infraestructura
├── renovatio-provider-java/           # Proveedor Java (OpenRewrite)
│   ├── service/                      # Servicios de refactorización Java
│   └── infrastructure/               # Integración MCP para Java
├── renovatio-provider-cobol/          # Proveedor COBOL (Migración a Java)
│   ├── domain/                       # Modelos de dominio COBOL
│   │   ├── CobolProgram.java         # Representación de programas COBOL
│   │   ├── CobolDataItem.java        # Estructuras de datos COBOL
│   │   └── CobolMcpTool.java         # Herramientas MCP COBOL
│   ├── service/                      # Servicios de migración COBOL
│   │   ├── CobolParsingService.java  # Parsing y análisis COBOL
│   │   ├── JavaGenerationService.java # Generación de código Java
│   │   ├── MigrationPlanService.java # Planificación de migración
│   │   ├── IndexingService.java      # Indexación Lucene
│   │   ├── MetricsService.java       # Cálculo de métricas
│   │   ├── TemplateCodeGenerationService.java # Generación basada en plantillas
│   │   └── ResilientMigrationService.java # Operaciones resilientes
│   └── infrastructure/               # Configuración e integración MCP
│       ├── CobolProviderConfiguration.java # Configuración Spring
│       └── CobolMcpToolsProvider.java # Proveedor de herramientas MCP
├── renovatio-provider-jcl/            # Proveedor JCL (traducción a shell/CI)
│   ├── service/                      # Parser y traductor JCL
│   └── infrastructure/               # Herramientas MCP para conversión
├── renovatio-agent/                   # Agente de ejecución
├── renovatio-web/                     # Aplicación web principal
│   ├── McpServerApplication.java      # Aplicación principal Spring Boot
│   ├── application/                   # Servicios de aplicación
│   │   ├── McpToolingService.java    # Servicio principal MCP
│   │   └── RefactorService.java      # Servicio de refactorización
│   ├── domain/                       # Modelos de dominio web
│   │   ├── mcp/                      # Modelos del protocolo MCP
│   │   │   ├── McpRequest.java
│   │   │   ├── McpResponse.java
│   │   │   ├── McpError.java
│   │   │   ├── McpTool.java
│   │   │   └── McpCapabilities.java
│   │   ├── RefactorRequest.java
│   │   ├── RefactorResponse.java
│   │   └── Tool.java
│   └── infrastructure/               # Controladores
│       ├── McpProtocolController.java # Controlador principal MCP
│       ├── McpController.java        # Controlador REST MCP
│       └── RefactorController.java   # Controlador de refactorización
└── src/main/resources/
    ├── templates/                    # Plantillas Freemarker para COBOL
    └── application.yml              # Configuración principal
```

## Especificación MCP

Este servidor implementa la especificación del Protocolo de Contenido de Modelo versión `2025-06-18` y es completamente compatible con:

- **JSON-RPC 2.0**: Protocolo de comunicación estándar
- **Herramientas con esquemas**: Definiciones completas de entrada/salida
- **Capacidades del servidor**: Exposición de funcionalidades soportadas
- **Manejo de errores**: Códigos de error estándar JSON-RPC

## Configuración

### Dependencias Principales

#### Framework Base
- **Spring Boot 3.2.5**: Framework base de la aplicación
- **Jackson 2.15.4**: Serialización JSON

#### Refactorización Java
- **OpenRewrite 8.21.0**: Motor de refactorización Java
- **SpringDoc OpenAPI**: Documentación automática de API

#### Migración COBOL
- **JavaPoet 1.13.0**: Generación de código Java type-safe
- **Freemarker 2.3.32**: Motor de plantillas para generación de código
- **MapStruct 1.5.5**: Generación automática de mappers
- **Apache Lucene 9.8.0**: Indexación y búsqueda de código
- **ANTLR4 4.13.1**: Parsing y validación de gramáticas
- **Resilience4j 2.1.0**: Patrones de resistencia (circuit breaker, retry, timeout)
- **Micrometer 1.12.1**: Métricas y monitoreo

#### Operaciones Git
- **JGit 6.7.0**: Operaciones Git programáticas

### Variables de Entorno

#### Configuración General
- `SERVER_PORT`: Puerto del servidor (por defecto 8181)
- `SPRING_PROFILES_ACTIVE`: Perfil activo de Spring

#### Configuración COBOL
- `RENOVATIO_COBOL_PARSER_MAX_FILE_SIZE`: Tamaño máximo de archivo COBOL (por defecto 10MB)
- `RENOVATIO_COBOL_GENERATION_TARGET_PACKAGE`: Paquete Java objetivo por defecto
- `RENOVATIO_COBOL_MIGRATION_DEFAULT_STRATEGY`: Estrategia de migración por defecto (incremental)

### Configuración Avanzada

#### application.yml Completo
```yaml
server:
  port: 8181

renovatio:
  cobol:
    parser:
      max-file-size: 10MB
      parallel-processing: true
      encoding: UTF-8
      dialect: IBM  # IBM, GNU, Micro Focus
    generation:
      target-package: org.shark.renovatio.generated.cobol
      generate-tests: true
      target-framework: spring-boot  # spring-boot, quarkus, jakarta-ee
      java-version: 17
    migration:
      default-strategy: incremental  # incremental, full, hybrid
      backup-original: true
      dry-run-default: true
      max-parallel-jobs: 4
    indexing:
      enabled: true
      index-path: ./cobol-index
      real-time-updates: true
    metrics:
      complexity-threshold: 10
      generate-reports: true
      export-format: json  # json, csv, xml
  
  resilience:
    circuit-breaker:
      failure-rate-threshold: 50
      wait-duration-in-open-state: 30s
    retry:
      max-attempts: 3
      wait-duration: 1s
    timeout:
      duration: 30s

management:
  endpoints:
    web:
      exposure:
        include: health,metrics,prometheus
  endpoint:
    health:
      show-details: always
```

## Desarrollo

### Compilar el Proyecto Completo
```bash
mvn clean compile
```

### Compilar Solo el Proveedor COBOL
```bash
mvn clean compile -pl renovatio-provider-cobol
```

### Compilar Solo el Proveedor JCL
```bash
mvn clean compile -pl renovatio-provider-jcl
```

### Ejecutar Tests
```bash
# Todos los tests
mvn test

# Solo tests de COBOL
mvn test -pl renovatio-provider-cobol

# Solo tests de Java/OpenRewrite
mvn test -pl renovatio-provider-java

# Solo tests de JCL
mvn test -pl renovatio-provider-jcl
```

### Ejecutar con Perfil COBOL Habilitado
```bash
SERVER_PORT=8181 mvn spring-boot:run -Dspring-boot.run.profiles=cobol
```

### Desarrollo Local con Hot Reload
```bash
# Terminal 1: Compilación automática
mvn compile -pl renovatio-provider-cobol -T 1C

# Terminal 2: Servidor con reload
mvn spring-boot:run -Dspring-boot.run.jvmArguments="-agentlib:jdwp=transport=dt_socket,server=y,suspend=n,address=5005"
```

### Generar Documentación
La documentación OpenAPI se genera automáticamente y está disponible en:
- Swagger UI: `http://localhost:8181/swagger-ui/index.html`
- OpenAPI JSON: `http://localhost:8181/v3/api-docs`
- Documentación COBOL específica: `http://localhost:8181/cobol/docs`

### Testing de Migración COBOL

#### Preparar Proyecto de Prueba
```bash
# Crear estructura de prueba
mkdir -p /tmp/cobol-test/{src,output}
cat > /tmp/cobol-test/src/CUSTOMER.cbl << 'EOF'
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CUSTOMER.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 CUSTOMER-RECORD.
          05 CUSTOMER-ID     PIC 9(6).
          05 CUSTOMER-NAME   PIC X(30).
          05 CUSTOMER-BALANCE PIC 9(8)V99 COMP-3.
       
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           MOVE 123456 TO CUSTOMER-ID
           MOVE "JOHN DOE" TO CUSTOMER-NAME
           MOVE 1234.56 TO CUSTOMER-BALANCE
           DISPLAY "Customer: " CUSTOMER-NAME
           STOP RUN.
EOF
```

#### Probar Análisis
```bash
curl -X POST -H "Content-Type: application/json" \
  -d '{
    "jsonrpc": "2.0",
    "id": "test-1",
    "method": "tools/call",
    "params": {
      "name": "cobol.analyze",
      "arguments": {
        "workspacePath": "/tmp/cobol-test/src",
        "includeMetrics": true
      }
    }
  }' \
  http://localhost:8181/ | jq .
```

#### Probar Generación de Stubs
```bash
curl -X POST -H "Content-Type: application/json" \
  -d '{
    "jsonrpc": "2.0",
    "id": "test-2",
    "method": "tools/call",
    "params": {
      "name": "cobol.generate.stubs",
      "arguments": {
        "workspacePath": "/tmp/cobol-test/src",
        "targetPackage": "com.example.customer",
        "generateTests": true
      }
    }
  }' \
  http://localhost:8181/ | jq .
```

## Contribuir

1. Fork el repositorio
2. Crea una rama para tu feature
3. Realiza tus cambios
4. Ejecuta los tests
5. Envía un pull request

## Roadmap

### ☕ Mejoras Java (OpenRewrite)
- [ ] Mejoras en la integración y cobertura de recetas del SDK oficial de OpenRewrite
- [ ] Soporte para recetas personalizadas
- [ ] Repositorios de recetas: posibilidad de agregar recetas propias y gestionarlas desde el servidor
- [ ] Análisis de impacto de refactorizaciones
- [ ] Integración con sistemas de control de versiones

### 🏢 Mejoras COBOL (Migración)
- [ ] **Integración ProLeap/Koopa**: Parser COBOL de nivel de producción para análisis completo
- [ ] **Soporte multi-dialecto**: IBM COBOL, GNU COBOL, Micro Focus COBOL
- [x] **Migración de copybooks**: Análisis y conversión de copybooks compartidos
- [x] **Migración de JCL**: Conversión de Job Control Language a scripts equivalentes
- [x] **Integración CICS**: Soporte para transacciones CICS y generación de equivalentes REST
- [x] **Migración de DB2**: Conversión de SQL embebido a JPA/Hibernate
- [ ] **Análisis de rendimiento**: Comparación de rendimiento antes/después de migración
- [ ] **Validación automática**: Generación de tests de equivalencia funcional
- [ ] **Plantillas personalizables**: Sistema de plantillas extensible para diferentes arquitecturas objetivo
- [ ] **Dashboard de migración**: Interfaz web para monitoreo en tiempo real
- [ ] **Integración CI/CD**: Plugins para Jenkins, GitLab CI, GitHub Actions
- [ ] **Reportes ejecutivos**: Dashboards de progreso para stakeholders

### 🔧 Mejoras de Plataforma
- [ ] **Métricas y monitoring avanzado**: Dashboards de Grafana, alertas Prometheus
- [ ] **Autenticación y autorización**: Integración OAuth2, RBAC
- [ ] **Soporte para múltiples lenguajes**: Expansión a C++, .NET, mainframe assembler
- [ ] **API GraphQL**: Interfaz alternativa para consultas complejas
- [ ] **Integración con repositorios**: GitHub, GitLab, Bitbucket
- [ ] **Cache distribuido**: Redis/Hazelcast para operaciones escalables
- [ ] **Procesamiento batch**: Migración de proyectos grandes en background

## Ventajas Técnicas

### 🏗️ Arquitectura Robusta
- **Plataforma multi-lenguaje**: Soporte nativo para Java y COBOL con arquitectura extensible
- **Patrón Provider**: Arquitectura modular que permite agregar nuevos lenguajes fácilmente
- **Procesamiento local**: Toda la refactorización y análisis se realiza localmente, garantizando privacidad y velocidad
- **Sin dependencias externas**: No requiere API keys ni servicios externos para funcionalidad básica

### ☕ Capacidades Java
- **SDK oficial de OpenRewrite**: Acceso completo a todas las recetas y herramientas del ecosistema OpenRewrite
- **Refactorización enterprise**: Soporte para proyectos Java de gran escala
- **Migración de versiones**: Automatización de actualizaciones entre versiones principales de Java

### 🏢 Capacidades COBOL Avanzadas
- **Migración integral**: No solo conversión de sintaxis, sino generación de arquitecturas Java modernas
- **Análisis semántico**: Comprensión profunda de la lógica de negocio COBOL
- **Generación inteligente**: DTOs, servicios, controladores REST y mappers automáticos
- **Patrones modernos**: Aplicación de mejores prácticas Java y patrones de diseño
- **Validación de migración**: Generación automática de tests para verificar equivalencia funcional

### 🔧 Operaciones Resilientes
- **Circuit breakers**: Protección contra fallos en cascada durante migraciones grandes
- **Reintentos inteligentes**: Recuperación automática de fallos transitorios
- **Timeouts configurables**: Prevención de operaciones que se cuelguen
- **Monitoreo en tiempo real**: Métricas Prometheus y health checks
- **Rollback seguro**: Capacidad de revertir migraciones parciales

### 🎯 Productividad del Desarrollador
- **Dry-run**: Simulación segura de migraciones antes de aplicar cambios
- **Migración incremental**: Modernización paso a paso sin big-bang
- **Búsqueda avanzada**: Índices Lucene para navegación rápida en código legacy
- **Plantillas personalizables**: Adaptación a estándares y arquitecturas específicas

## Licencia

Este proyecto está bajo la licencia MIT.

---

**Renovatio** - La plataforma definitiva para refactorización multi-lenguaje y modernización de aplicaciones legacy. Haciendo la refactorización de código Java y la migración COBOL accesible através del Protocolo de Contenido de Modelo.

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