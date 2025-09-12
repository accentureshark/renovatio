# Guía de Uso de Herramientas MCP en VS Code con Renovatio

Esta guía te muestra cómo usar las herramientas MCP (Model Content Protocol) del servidor Renovatio desde VS Code como cliente MCP.

## Configuración Inicial

### 1. Configurar el Servidor Renovatio

Primero, asegúrate de que el servidor Renovatio esté ejecutándose:

```bash
# Desde el directorio raíz del proyecto Renovatio
cd renovatio-mcp-server
SERVER_PORT=8181 mvn spring-boot:run
```

El servidor se iniciará en `http://localhost:8181` y mostrará las herramientas disponibles:

```
2025-09-12 19:51:02 - MCP Tools available on server startup:
2025-09-12 19:51:02 - - nql_compile: Compile natural language to NQL
2025-09-12 19:51:02 - - common_index: Index repository for search
2025-09-12 19:51:02 - - common_search: Search indexed repository
2025-09-12 19:51:02 - - java_analyze: Analyze code structure and extract information for java
2025-09-12 19:51:02 - - java_plan: Create execution plan for transformations for java
2025-09-12 19:51:02 - - java_apply: Apply transformation plan for java
2025-09-12 19:51:02 - - java_diff: Generate semantic diff for java
2025-09-12 19:51:02 - - java_metrics: Calculate code metrics for java
```

### 2. Configurar VS Code como Cliente MCP

Para configurar VS Code como cliente MCP para Renovatio, agrega la siguiente configuración en tu cliente MCP:

```json
{
  "id": "renovatio-local",
  "name": "Renovatio - Refactor MCP Server (Local)",
  "description": "Refactor MCP Server for code analysis and transformation",
  "url": "http://localhost:8181/",
  "implemented": true,
  "prewarm": true
}
```

## Herramientas Disponibles

### 1. `nql_compile` - Compilar Lenguaje Natural a NQL

**Descripción**: Convierte preguntas en lenguaje natural a consultas NQL (Natural Query Language).

**Parámetros**:
- `question` (requerido): Pregunta en lenguaje natural
- `context` (opcional): Contexto adicional

**Ejemplo de uso**:
```json
{
  "jsonrpc": "2.0",
  "id": "1",
  "method": "tools/call",
  "params": {
    "name": "nql_compile",
    "arguments": {
      "question": "Find all classes in Java code",
      "context": "I want to analyze Java classes in a project"
    }
  }
}
```

### 2. `common_index` - Indexar Repositorio para Búsqueda

**Descripción**: Indexa un repositorio para habilitar búsquedas rápidas.

**Parámetros**:
- `repoId` (requerido): ID del repositorio

**Ejemplo de uso**:
```json
{
  "jsonrpc": "2.0",
  "id": "2",
  "method": "tools/call",
  "params": {
    "name": "common_index",
    "arguments": {
      "repoId": "/path/to/your/project"
    }
  }
}
```

### 3. `common_search` - Buscar en Repositorio Indexado

**Descripción**: Busca en un repositorio previamente indexado.

**Parámetros**:
- `repoId` (requerido): ID del repositorio
- `query` (requerido): Consulta de búsqueda
- `path` (opcional): Filtro de ruta

**Ejemplo de uso**:
```json
{
  "jsonrpc": "2.0",
  "id": "3",
  "method": "tools/call",
  "params": {
    "name": "common_search",
    "arguments": {
      "repoId": "/path/to/your/project",
      "query": "Calculator",
      "path": "src/main/java"
    }
  }
}
```

### 4. `java_analyze` - Analizar Estructura de Código Java

**Descripción**: Analiza la estructura del código Java y extrae información detallada.

**Parámetros**:
- `scope` (opcional): Ámbito de la operación (directorio o archivo)
- `nql` (opcional): Consulta NQL específica

**Ejemplo de uso**:
```json
{
  "jsonrpc": "2.0",
  "id": "4",
  "method": "tools/call",
  "params": {
    "name": "java_analyze",
    "arguments": {
      "scope": "/path/to/your/java/project",
      "nql": "FIND ALL CLASSES"
    }
  }
}
```

### 5. `java_plan` - Crear Plan de Ejecución para Transformaciones

**Descripción**: Crea un plan de ejecución para transformaciones de código Java.

**Parámetros**:
- `scope` (opcional): Ámbito de la operación
- `nql` (opcional): Consulta NQL para las transformaciones

**Ejemplo de uso**:
```json
{
  "jsonrpc": "2.0",
  "id": "5",
  "method": "tools/call",
  "params": {
    "name": "java_plan",
    "arguments": {
      "scope": "/path/to/your/java/project",
      "nql": "MIGRATE FROM JAVA 8 TO JAVA 17"
    }
  }
}
```

### 6. `java_apply` - Aplicar Plan de Transformación

**Descripción**: Aplica un plan de transformación previamente creado.

**Parámetros**:
- `scope` (opcional): Ámbito de la operación
- `nql` (opcional): Consulta NQL para las transformaciones

**Ejemplo de uso**:
```json
{
  "jsonrpc": "2.0",
  "id": "6",
  "method": "tools/call",
  "params": {
    "name": "java_apply",
    "arguments": {
      "scope": "/path/to/your/java/project",
      "nql": "APPLY PLAN_ID_12345"
    }
  }
}
```

### 7. `java_diff` - Generar Diferencias Semánticas

**Descripción**: Genera diferencias semánticas para código Java.

**Parámetros**:
- `scope` (opcional): Ámbito de la operación
- `nql` (opcional): Consulta NQL para la comparación

**Ejemplo de uso**:
```json
{
  "jsonrpc": "2.0",
  "id": "7",
  "method": "tools/call",
  "params": {
    "name": "java_diff",
    "arguments": {
      "scope": "/path/to/your/java/project",
      "nql": "COMPARE BEFORE AND AFTER TRANSFORMATION"
    }
  }
}
```

### 8. `java_metrics` - Calcular Métricas de Código

**Descripción**: Calcula métricas de código para proyectos Java.

**Parámetros**:
- `scope` (opcional): Ámbito de la operación
- `nql` (opcional): Consulta NQL para las métricas

**Ejemplo de uso**:
```json
{
  "jsonrpc": "2.0",
  "id": "8",
  "method": "tools/call",
  "params": {
    "name": "java_metrics",
    "arguments": {
      "scope": "/path/to/your/java/project",
      "nql": "CALCULATE METRICS FOR ALL CLASSES"
    }
  }
}
```

## Flujo de Trabajo Típico

### 1. Análisis de Código Java

Para analizar un proyecto Java:

1. **Inicializar conexión MCP**:
```json
{
  "jsonrpc": "2.0",
  "id": "1",
  "method": "initialize",
  "params": {}
}
```

2. **Indexar el repositorio**:
```json
{
  "jsonrpc": "2.0",
  "id": "2",
  "method": "tools/call",
  "params": {
    "name": "common_index",
    "arguments": {
      "repoId": "/path/to/your/java/project"
    }
  }
}
```

3. **Analizar estructura del código**:
```json
{
  "jsonrpc": "2.0",
  "id": "3",
  "method": "tools/call",
  "params": {
    "name": "java_analyze",
    "arguments": {
      "scope": "/path/to/your/java/project",
      "nql": "FIND ALL CLASSES"
    }
  }
}
```

4. **Calcular métricas**:
```json
{
  "jsonrpc": "2.0",
  "id": "4",
  "method": "tools/call",
  "params": {
    "name": "java_metrics",
    "arguments": {
      "scope": "/path/to/your/java/project",
      "nql": "CALCULATE COMPLEXITY METRICS"
    }
  }
}
```

### 2. Transformación de Código

Para transformar código Java:

1. **Crear plan de transformación**:
```json
{
  "jsonrpc": "2.0",
  "id": "5",
  "method": "tools/call",
  "params": {
    "name": "java_plan",
    "arguments": {
      "scope": "/path/to/your/java/project",
      "nql": "MIGRATE TO JAVA 17"
    }
  }
}
```

2. **Revisar diferencias antes de aplicar**:
```json
{
  "jsonrpc": "2.0",
  "id": "6",
  "method": "tools/call",
  "params": {
    "name": "java_diff",
    "arguments": {
      "scope": "/path/to/your/java/project",
      "nql": "PREVIEW CHANGES"
    }
  }
}
```

3. **Aplicar transformaciones**:
```json
{
  "jsonrpc": "2.0",
  "id": "7",
  "method": "tools/call",
  "params": {
    "name": "java_apply",
    "arguments": {
      "scope": "/path/to/your/java/project",
      "nql": "APPLY MIGRATION PLAN"
    }
  }
}
```

## Usando curl para Pruebas

También puedes probar las herramientas directamente con curl:

```bash
# Inicializar conexión
curl -X POST -H "Content-Type: application/json" \
  -d '{"jsonrpc": "2.0", "id": "1", "method": "initialize", "params": {}}' \
  http://localhost:8181/

# Listar herramientas disponibles
curl -X POST -H "Content-Type: application/json" \
  -d '{"jsonrpc": "2.0", "id": "2", "method": "tools/list", "params": {}}' \
  http://localhost:8181/

# Analizar código Java
curl -X POST -H "Content-Type: application/json" \
  -d '{
    "jsonrpc": "2.0",
    "id": "3",
    "method": "tools/call",
    "params": {
      "name": "java_analyze",
      "arguments": {
        "scope": "/path/to/your/project",
        "nql": "FIND ALL CLASSES"
      }
    }
  }' \
  http://localhost:8181/
```

## Notas Importantes

1. **Protocolo MCP**: Renovatio implementa el estándar MCP versión `2025-06-18` con compatibilidad total JSON-RPC 2.0.

2. **NQL (Natural Query Language)**: Algunas herramientas utilizan NQL, que actualmente está en desarrollo. Usa la herramienta `nql_compile` para convertir lenguaje natural a consultas NQL.

3. **Scope Parameter**: El parámetro `scope` debe apuntar a un directorio válido que contenga código Java.

4. **Indexación**: Para búsquedas óptimas, asegúrate de indexar el repositorio primero usando `common_index`.

5. **Estado del Servidor**: Verifica que el servidor esté ejecutándose y muestre las 8 herramientas en los logs de inicio.

## Solución de Problemas

- **Herramientas no responden**: Verifica que el servidor esté ejecutándose en el puerto 8181.
- **Error de conexión**: Asegúrate de que la URL del cliente MCP sea `http://localhost:8181/`.
- **Resultados vacíos**: Verifica que la ruta especificada en `scope` sea válida y contenga código Java.
- **NQL no funciona**: La implementación de NQL está en desarrollo, usa consultas básicas por ahora.