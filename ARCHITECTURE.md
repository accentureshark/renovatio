# Arquitectura Renovatio - Separación MCP Server y Core Engine

Este documento describe la arquitectura modular de Renovatio, donde la lógica MCP está completamente separada del motor central (core engine), permitiendo máxima flexibilidad, extensibilidad y cumplimiento estricto del estándar Model Content Protocol (MCP).

## Visión General de la Arquitectura

```
┌─────────────────────┐    ┌─────────────────────┐
│   Clientes MCP      │    │   Usuarios Library  │
│   (VS Code, etc.)   │    │   (Uso Directo)     │
└──────────┬──────────┘    └──────────┬──────────┘
           │                          │
           │ JSON-RPC 2.0             │ Llamadas directas
           │                          │
┌──────────▼──────────┐    ┌──────────▼──────────┐
│  renovatio-mcp-     │    │     Tu App         │
│  server             │    │                    │
│  ┌─────────────────┐│    │                    │
│  │ Protocolo MCP   ││    │                    │
│  │ Implementación  ││    │                    │
│  └─────────────────┘│    │                    │
└──────────┬──────────┘    └──────────┬──────────┘
           │                          │
           └──────────────────────────┼──────────┘
                                      │
                           ┌──────────▼──────────┐
                           │  renovatio-core     │
                           │  (Motor puro)       │
                           │  ┌─────────────────┐│
                           │  │ Registro de     ││
                           │  │ Proveedores     ││
                           │  │ de Lenguaje     ││
                           │  └─────────────────┘│
                           └──────────┬──────────┘
                                      │
                    ┌─────────────────┼─────────────────┐
                    │                 │                 │
         ┌──────────▼──────────┐ ┌───▼───┐ ┌──────────▼──────────┐
         │ renovatio-provider- │ │ ...   │ │ renovatio-provider- │
         │ java                │ │       │ │ cobol               │
         │ (OpenRewrite)       │ │       │ │ (ANTLR4)            │
         └─────────────────────┘ └───────┘ └─────────────────────┘
```

## Estructura de Módulos

### 🎯 renovatio-core (Motor Puro)

**Propósito**: Motor de refactorización y migración agnóstico de protocolo.

**Características Clave**:
- ✅ Sin dependencias MCP (protocol-agnostic)
- ✅ Abstracciones de Tool y Recipe independientes de protocolo
- ✅ Registro de proveedores de lenguaje con generación dinámica de herramientas
- ✅ Usable como dependencia Maven o librería standalone
- ✅ Formato de receta unificado para todos los lenguajes

**Ejemplo de uso como librería**:
```xml
<dependency>
    <groupId>org.shark.renovatio</groupId>
    <artifactId>renovatio-core</artifactId>
    <version>0.0.1-SNAPSHOT</version>
</dependency>
```

```java
// Uso directo del motor core en tu aplicación
LanguageProviderRegistry registry = new LanguageProviderRegistry();
List<Tool> tools = registry.generateTools();
Map<String, Object> result = registry.routeToolCall("java.analyze", arguments);
```

### 🚀 renovatio-mcp-server (Implementación Protocolo MCP)

**Propósito**: Implementación completa de la especificación MCP que expone el motor core.

**Características Clave**:
- ✅ Cumplimiento total MCP 2025-06-18
- ✅ Sirve en la raíz "/" para máxima compatibilidad
- ✅ Todos los métodos MCP: initialize, tools/*, prompts/*, resources/*, etc.
- ✅ JSON-RPC 2.0 con manejo de errores robusto
- ✅ Aplicación Spring Boot con health checks y monitoreo

**Métodos MCP soportados**:
- `initialize`, `ping`, `tools/list`, `tools/call`, `tools/describe`
- `capabilities`, `server/info`, `content/read`, `content/write`
- `workspace/list`, `workspace/describe`, `prompts/list`, `prompts/get`, `resources/list`, `resources/read`

**Inicio del servidor MCP**:
```bash
cd renovatio-mcp-server
mvn spring-boot:run
```
El servidor inicia en el puerto 8080 por defecto y atiende solicitudes MCP en `http://localhost:8080/`.

### 🔗 renovatio-shared (Abstracciones Comunes)

**Propósito**: Interfaces y utilidades agnósticas de protocolo.

**Componentes Clave**:
- Interfaz `Tool` - Definición universal de herramienta
- Interfaz `Recipe` - Formato de receta unificado
- Implementación `BasicTool` - Herramienta concreta
- Modelos de dominio - Estructuras de datos compartidas

### 🛠️ Proveedores de Lenguaje

**Propósito**: Plugins de implementación específica por lenguaje.

**Lenguajes soportados**:
- **Java**: Vía recetas OpenRewrite
- **COBOL**: Vía parsers ANTLR4
- **Extensible**: Fácil de agregar nuevos lenguajes

## Ejemplos de Uso

### 1. Uso del motor core como librería

```java
@Component
public class MiServicioRefactor {
    private final LanguageProviderRegistry coreEngine;
    public MiServicioRefactor() {
        this.coreEngine = new LanguageProviderRegistry();
    }
    public List<String> getSupportedLanguages() {
        return new ArrayList<>(coreEngine.getSupportedLanguages());
    }
    public Map<String, Object> refactorCode(String language, String operation, Map<String, Object> params) {
        String toolName = language + "." + operation;
        return coreEngine.routeToolCall(toolName, params);
    }
}
```

### 2. Integración Cliente MCP

```json
{
  "servers": {
    "renovatio": {
      "command": "java",
      "args": ["-jar", "renovatio-mcp-server.jar"],
      "env": {
        "SERVER_PORT": "8080"
      }
    }
  }
}
```

### 3. Uso HTTP API

```bash
# Inicializar sesión MCP
curl -X POST http://localhost:8080/ \
  -H "Content-Type: application/json" \
  -d '{
    "jsonrpc": "2.0",
    "id": 1,
    "method": "initialize",
    "params": {"protocolVersion": "2025-06-18"}
  }'

# Listar herramientas disponibles
curl -X POST http://localhost:8080/ \
  -H "Content-Type: application/json" \
  -d '{"jsonrpc": "2.0", "id": 2, "method": "tools/list"}'

# Ejecutar una herramienta
curl -X POST http://localhost:8080/ \
  -H "Content-Type: application/json" \
  -d '{
    "jsonrpc": "2.0",
    "id": 3,
    "method": "tools/call",
    "params": {
      "name": "java.analyze",
      "arguments": {
        "nql": "FIND classes WHERE name LIKE *Service",
        "scope": "src/main/java"
      }
    }
  }'
```

## Configuración

### Configuración del motor core

El motor core se configura a través de `LanguageProviderRegistry`:

```yaml
renovatio:
  providers:
    java:
      enabled: true
      recipes-path: "classpath:recipes/java"
    cobol:
      enabled: true
      recipes-path: "classpath:recipes/cobol"
  recipes:
    format: "unified"
    base-path: "classpath:recipes"
```

### Configuración del servidor MCP

```yaml
server:
  port: 8080
mcp:
  server:
    name: "Renovatio MCP Server"
    version: "1.0.0"
    protocol-version: "2025-06-18"
  capabilities:
    tools: true
    prompts: true
    resources: true
    content:
      read: true
      write: true
    workspace:
      list: true
      describe: true
```

## Beneficios de esta arquitectura

### ✅ Separación de responsabilidades
- El motor core es lógica de negocio pura
- El servidor MCP es implementación de protocolo
- Fácil de mantener y testear cada componente

### ✅ Múltiples patrones de uso
- **Librería**: Incluye core como dependencia Maven
- **Servidor MCP**: Cumplimiento total de protocolo MCP
- **REST API**: Endpoints HTTP tradicionales
- **Embebido**: Uso en cualquier aplicación

### ✅ Agnosticismo de protocolo
- El core no conoce MCP
- Fácil de agregar otros protocolos (GraphQL, gRPC, etc.)
- Arquitectura preparada para el futuro

### ✅ Formato de receta unificado
- Misma interfaz de receta para todos los lenguajes
- Java vía OpenRewrite
- COBOL vía ANTLR4
- Experiencia consistente entre lenguajes

### ✅ Cumplimiento MCP
- Cumplimiento total de la especificación MCP
- Sirve en la raíz para máxima compatibilidad
- Implementación JSON-RPC 2.0 robusta
- Manejo de errores completo

## Testing

### Tests del motor core
```bash
cd renovatio-core
mvn test
```

### Tests del servidor MCP
```bash
cd renovatio-mcp-server
mvn test
```

### Tests de integración
```bash
mvn verify
```

## Desarrollo

### Agregar nuevos proveedores de lenguaje
1. Crear nuevo módulo: `renovatio-provider-<lenguaje>`
2. Implementar la interfaz `LanguageProvider`
3. Definir recetas específicas del lenguaje
4. Registrar en `LanguageProviderRegistry`

### Extender funcionalidad MCP
1. Agregar nuevos métodos a `McpProtocolService`
2. Actualizar `McpCapabilities`
3. Agregar tests correspondientes
4. Actualizar documentación

Esta arquitectura provee máxima flexibilidad y separación limpia entre protocolo y lógica de negocio.
