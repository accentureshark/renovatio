# Renovatio - OpenRewrite MCP Server

**Renovatio** es un servidor completamente compatible con el Protocolo de Contenido de Modelo (MCP, Model Content Protocol) que expone toda la funcionalidad de OpenRewrite como herramientas MCP para refactorización automatizada de código Java.

## Características

- ✅ **Compatibilidad total con MCP (Model Content Protocol)**: Implementa el estándar MCP versión `2025-06-18` y el protocolo JSON-RPC 2.0 completo.
- ✅ **Exposición completa de OpenRewrite**: Todas las recetas de OpenRewrite están disponibles como herramientas MCP.
- ✅ **API REST y MCP**: Soporta tanto endpoints REST tradicionales como el protocolo MCP en la raíz (`/`).
- ✅ **Documentación OpenAPI**: Interfaz Swagger UI para explorar la API.
- ✅ **Manejo de errores robusto**: Códigos de error apropiados y mensajes informativos.
- ✅ **Esquemas de herramientas**: Definiciones completas de entrada/salida para cada herramienta.
- ✅ **Prompts y Recursos MCP**: Listado y acceso a prompts y recursos del servidor.

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

El servidor expone más de 17 herramientas de OpenRewrite organizadas en categorías:

### 🎨 Formato y Limpieza
- `org.openrewrite.java.format.AutoFormat` - Formatear código Java automáticamente
- `org.openrewrite.java.cleanup.UnnecessaryParentheses` - Eliminar paréntesis innecesarios
- `org.openrewrite.java.cleanup.EmptyBlock` - Eliminar bloques vacíos
- `org.openrewrite.java.cleanup.ExplicitInitialization` - Eliminar inicialización explícita a valores por defecto
- `org.openrewrite.java.cleanup.FinalizePrivateFields` - Finalizar campos privados no reasignados

### 🔧 Mejoras de Código
- `org.openrewrite.java.cleanup.BigDecimalRoundingConstantsToEnums` - Reemplazar constantes BigDecimal con enums
- `org.openrewrite.java.cleanup.BooleanChecksNotInverted` - Reemplazar verificaciones booleanas invertidas
- `org.openrewrite.java.cleanup.CaseInsensitiveComparisonsDoNotChangeCase` - Usar métodos de comparación case-insensitive
- `org.openrewrite.java.cleanup.ChainStringBuilderAppendCalls` - Encadenar llamadas StringBuilder.append
- `org.openrewrite.java.cleanup.CovariantEquals` - Usar equals covariantes

### 🚀 Migración de Versiones
- `org.openrewrite.java.migrate.Java8toJava11` - Migrar de Java 8 a Java 11
- `org.openrewrite.java.migrate.JavaVersion11` - Actualizar a Java 11
- `org.openrewrite.java.migrate.JavaVersion17` - Actualizar a Java 17
- `org.openrewrite.java.migrate.JavaVersion21` - Actualizar a Java 21

### 🔒 Seguridad
- `org.openrewrite.java.security.FindJdbcUrl` - Encontrar URLs JDBC
- `org.openrewrite.java.security.FindSqlInjection` - Encontrar vulnerabilidades de inyección SQL
- `org.openrewrite.java.security.SecureRandomPrefersDefaultSeed` - Usar SecureRandom con semilla por defecto

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

#### Ejecutar una Herramienta
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
src/main/java/org/shark/renovatio/
├── McpServerApplication.java          # Aplicación principal Spring Boot
├── application/                       # Servicios de aplicación
│   ├── McpToolingService.java        # Servicio principal MCP
│   └── RefactorService.java          # Servicio de refactorización
├── domain/                           # Modelos de dominio
│   ├── mcp/                          # Modelos del protocolo MCP
│   │   ├── McpRequest.java
│   │   ├── McpResponse.java
│   │   ├── McpError.java
│   │   ├── McpTool.java
│   │   └── McpCapabilities.java
│   ├── RefactorRequest.java
│   ├── RefactorResponse.java
│   └── Tool.java
└── infrastructure/                   # Controladores
    ├── McpProtocolController.java    # Controlador principal MCP
    ├── McpController.java            # Controlador REST MCP
    └── RefactorController.java       # Controlador de refactorización
```

## Especificación MCP

Este servidor implementa la especificación del Protocolo de Contenido de Modelo versión `2025-06-18` y es completamente compatible con:

- **JSON-RPC 2.0**: Protocolo de comunicación estándar
- **Herramientas con esquemas**: Definiciones completas de entrada/salida
- **Capacidades del servidor**: Exposición de funcionalidades soportadas
- **Manejo de errores**: Códigos de error estándar JSON-RPC

## Configuración

### Dependencias Principales

- **Spring Boot 3.2.5**: Framework base
- **OpenRewrite 8.21.0**: Motor de refactorización
- **SpringDoc OpenAPI**: Documentación automática de API
- **Jackson**: Serialización JSON

### Variables de Entorno

El servidor usa la configuración por defecto de Spring Boot. Puedes personalizar:

- `SERVER_PORT`: Puerto del servidor (por defecto 8181)
- `SPRING_PROFILES_ACTIVE`: Perfil activo de Spring

## Desarrollo

### Compilar
```bash
mvn clean compile
```

### Ejecutar Tests
```bash
mvn test
```

### Generar Documentación
La documentación OpenAPI se genera automáticamente y está disponible en `/swagger-ui/index.html`

## Contribuir

1. Fork el repositorio
2. Crea una rama para tu feature
3. Realiza tus cambios
4. Ejecuta los tests
5. Envía un pull request

## Roadmap

- [ ] Mejoras en la integración y cobertura de recetas del SDK oficial de OpenRewrite (todas las recetas disponibles localmente)
- [ ] Soporte para recetas personalizadas
- [ ] Repositorios de recetas: posibilidad de agregar recetas propias y gestionarlas desde el servidor
- [ ] Métricas y monitoring
- [ ] Autenticación y autorización
- [ ] Soporte para múltiples lenguajes além de Java

## Ventajas técnicas

- Renovatio utiliza el **SDK oficial de OpenRewrite**: No depende de servicios externos ni requiere API key.
- Toda la refactorización y análisis se realiza **localmente**, garantizando privacidad y velocidad.
- Acceso completo a todas las recetas y herramientas del ecosistema OpenRewrite.

## Licencia

Este proyecto está bajo la licencia MIT.

---

**Renovatio** - Haciendo la refactorización de código accesible através del Protocolo de Contenido de Modelo.