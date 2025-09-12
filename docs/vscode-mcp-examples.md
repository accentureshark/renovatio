# VS Code MCP Client - Ejemplo Práctico con Renovatio

Este documento proporciona un ejemplo paso a paso para usar Renovatio como servidor MCP con VS Code como cliente.

## Configuración de VS Code para MCP

### 1. Instalar Extension MCP

Instala una extensión MCP compatible en VS Code. Algunas opciones incluyen:
- **MCP Client** (si está disponible en el marketplace)
- **REST Client** para hacer llamadas HTTP directas
- **Thunder Client** para pruebas de API

### 2. Configuración MCP en VS Code

Si usas una extensión MCP nativa, crea un archivo de configuración `.vscode/mcp-settings.json`:

```json
{
  "mcpServers": [
    {
      "id": "renovatio-local",
      "name": "Renovatio - Refactor MCP Server",
      "description": "Multi-language refactoring and migration platform",
      "url": "http://localhost:8181/",
      "protocol": "mcp",
      "version": "2025-06-18",
      "enabled": true,
      "autoConnect": true
    }
  ]
}
```

### 3. Configuración con REST Client

Si usas REST Client, crea un archivo `.vscode/mcp-renovatio.http`:

```http
### Configuración de variables
@baseUrl = http://localhost:8181
@contentType = application/json

### 1. Inicializar conexión MCP
POST {{baseUrl}}/
Content-Type: {{contentType}}

{
  "jsonrpc": "2.0",
  "id": "1",
  "method": "initialize",
  "params": {}
}

### 2. Listar herramientas disponibles
POST {{baseUrl}}/
Content-Type: {{contentType}}

{
  "jsonrpc": "2.0",
  "id": "2",
  "method": "tools/list",
  "params": {}
}

### 3. Analizar código Java
POST {{baseUrl}}/
Content-Type: {{contentType}}

{
  "jsonrpc": "2.0",
  "id": "3",
  "method": "tools/call",
  "params": {
    "name": "java_analyze",
    "arguments": {
      "scope": "/workspace/my-java-project",
      "nql": "FIND ALL CLASSES"
    }
  }
}

### 4. Indexar repositorio
POST {{baseUrl}}/
Content-Type: {{contentType}}

{
  "jsonrpc": "2.0",
  "id": "4",
  "method": "tools/call",
  "params": {
    "name": "common_index",
    "arguments": {
      "repoId": "/workspace/my-java-project"
    }
  }
}

### 5. Buscar en repositorio
POST {{baseUrl}}/
Content-Type: {{contentType}}

{
  "jsonrpc": "2.0",
  "id": "5",
  "method": "tools/call",
  "params": {
    "name": "common_search",
    "arguments": {
      "repoId": "/workspace/my-java-project",
      "query": "public class",
      "path": "src/main/java"
    }
  }
}

### 6. Calcular métricas
POST {{baseUrl}}/
Content-Type: {{contentType}}

{
  "jsonrpc": "2.0",
  "id": "6",
  "method": "tools/call",
  "params": {
    "name": "java_metrics",
    "arguments": {
      "scope": "/workspace/my-java-project",
      "nql": "CALCULATE METRICS FOR ALL CLASSES"
    }
  }
}

### 7. Compilar NQL
POST {{baseUrl}}/
Content-Type: {{contentType}}

{
  "jsonrpc": "2.0",
  "id": "7",
  "method": "tools/call",
  "params": {
    "name": "nql_compile",
    "arguments": {
      "question": "Find all Java classes with more than 10 methods",
      "context": "Code quality analysis"
    }
  }
}

### 8. Crear plan de transformación
POST {{baseUrl}}/
Content-Type: {{contentType}}

{
  "jsonrpc": "2.0",
  "id": "8",
  "method": "tools/call",
  "params": {
    "name": "java_plan",
    "arguments": {
      "scope": "/workspace/my-java-project",
      "nql": "MIGRATE FROM JAVA 8 TO JAVA 17"
    }
  }
}

### 9. Ver diferencias
POST {{baseUrl}}/
Content-Type: {{contentType}}

{
  "jsonrpc": "2.0",
  "id": "9",
  "method": "tools/call",
  "params": {
    "name": "java_diff",
    "arguments": {
      "scope": "/workspace/my-java-project",
      "nql": "SHOW CHANGES FOR MIGRATION"
    }
  }
}

### 10. Aplicar transformaciones
POST {{baseUrl}}/
Content-Type: {{contentType}}

{
  "jsonrpc": "2.0",
  "id": "10",
  "method": "tools/call",
  "params": {
    "name": "java_apply",
    "arguments": {
      "scope": "/workspace/my-java-project",
      "nql": "APPLY MIGRATION PLAN"
    }
  }
}
```

## Ejemplo Paso a Paso

### Paso 1: Iniciar el Servidor Renovatio

```bash
# Terminal en VS Code
cd path/to/renovatio/renovatio-mcp-server
SERVER_PORT=8181 mvn spring-boot:run
```

Verifica que veas este output:
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

### Paso 2: Probar la Conexión

En VS Code, abre el archivo `.vscode/mcp-renovatio.http` y ejecuta la primera petición:

```http
### 1. Inicializar conexión MCP
POST http://localhost:8181/
Content-Type: application/json

{
  "jsonrpc": "2.0",
  "id": "1",
  "method": "initialize",
  "params": {}
}
```

**Respuesta esperada**:
```json
{
  "id": "1",
  "result": {
    "capabilities": {
      "tools": {"listChanged": true},
      "prompts": {"listChanged": true},
      "resources": {"listChanged": true}
    },
    "serverInfo": {
      "name": "Renovatio MCP Server",
      "version": "1.0.0",
      "description": "Multi-language refactoring and migration platform with full MCP compliance"
    },
    "availableTools": [
      {
        "name": "nql_compile",
        "description": "Compile natural language to NQL",
        "inputSchema": {
          "type": "object",
          "properties": {
            "question": {"type": "string", "description": "Natural language question"},
            "context": {"type": "string", "description": "Additional context"}
          },
          "required": ["question"]
        }
      }
      // ... más herramientas
    ],
    "protocolVersion": "2025-06-18"
  },
  "error": null,
  "jsonrpc": "2.0"
}
```

### Paso 3: Listar Herramientas Disponibles

```http
### 2. Listar herramientas disponibles
POST http://localhost:8181/
Content-Type: application/json

{
  "jsonrpc": "2.0",
  "id": "2",
  "method": "tools/list",
  "params": {}
}
```

**Respuesta esperada**:
```json
{
  "id": "2",
  "result": {
    "tools": [
      {
        "name": "nql_compile",
        "description": "Compile natural language to NQL",
        "inputSchema": {
          "type": "object",
          "properties": {
            "question": {"type": "string", "description": "Natural language question"},
            "context": {"type": "string", "description": "Additional context"}
          },
          "required": ["question"]
        }
      },
      {
        "name": "java_analyze",
        "description": "Analyze code structure and extract information for java",
        "inputSchema": {
          "type": "object",
          "properties": {
            "scope": {"type": "string", "description": "Operation scope"},
            "nql": {"type": "string", "description": "NQL query"}
          }
        }
      }
      // ... más herramientas
    ]
  },
  "error": null,
  "jsonrpc": "2.0"
}
```

### Paso 4: Analizar Código Java

Cambia la ruta en el siguiente ejemplo para apuntar a tu proyecto Java:

```http
### 3. Analizar código Java
POST http://localhost:8181/
Content-Type: application/json

{
  "jsonrpc": "2.0",
  "id": "3",
  "method": "tools/call",
  "params": {
    "name": "java_analyze",
    "arguments": {
      "scope": "/Users/tu-usuario/workspace/mi-proyecto-java",
      "nql": "FIND ALL CLASSES"
    }
  }
}
```

### Paso 5: Indexar y Buscar

```http
### 4. Indexar repositorio
POST http://localhost:8181/
Content-Type: application/json

{
  "jsonrpc": "2.0",
  "id": "4",
  "method": "tools/call",
  "params": {
    "name": "common_index",
    "arguments": {
      "repoId": "/Users/tu-usuario/workspace/mi-proyecto-java"
    }
  }
}

### 5. Buscar en repositorio
POST http://localhost:8181/
Content-Type: application/json

{
  "jsonrpc": "2.0",
  "id": "5",
  "method": "tools/call",
  "params": {
    "name": "common_search",
    "arguments": {
      "repoId": "/Users/tu-usuario/workspace/mi-proyecto-java",
      "query": "Calculator",
      "path": "src/main/java"
    }
  }
}
```

### Paso 6: Calcular Métricas

```http
### 6. Calcular métricas
POST http://localhost:8181/
Content-Type: application/json

{
  "jsonrpc": "2.0",
  "id": "6",
  "method": "tools/call",
  "params": {
    "name": "java_metrics",
    "arguments": {
      "scope": "/Users/tu-usuario/workspace/mi-proyecto-java",
      "nql": "CALCULATE METRICS FOR ALL CLASSES"
    }
  }
}
```

## Flujo de Trabajo en VS Code

### Opción 1: Usar REST Client

1. **Instalar REST Client**: Ve a Extensions y busca "REST Client" por Huachao Mao.

2. **Crear archivo .http**: Crea un archivo llamado `renovatio-mcp.http` en tu workspace.

3. **Copiar ejemplos**: Copia los ejemplos HTTP de arriba.

4. **Ejecutar requests**: Haz click en "Send Request" arriba de cada petición HTTP.

### Opción 2: Usar Thunder Client

1. **Instalar Thunder Client**: Ve a Extensions y busca "Thunder Client".

2. **Crear Collection**: Crea una nueva colección llamada "Renovatio MCP".

3. **Agregar requests**: Crea requests para cada herramienta MCP.

4. **Configurar environment**: Crea un environment con `baseUrl = http://localhost:8181`.

### Opción 3: Usar Terminal Integrado

```bash
# En el terminal de VS Code
curl -X POST -H "Content-Type: application/json" \
  -d '{"jsonrpc": "2.0", "id": "1", "method": "initialize", "params": {}}' \
  http://localhost:8181/
```

## Consejos y Mejores Prácticas

1. **Verificar servidor**: Siempre verifica que el servidor Renovatio esté ejecutándose antes de hacer peticiones.

2. **Usar rutas absolutas**: En el parámetro `scope`, usa rutas absolutas para evitar problemas.

3. **Logging**: Revisa los logs del servidor para diagnosticar problemas.

4. **Iterativo**: Comienza con herramientas simples como `java_analyze` antes de usar `java_apply`.

5. **Backup**: Siempre haz backup de tu código antes de aplicar transformaciones.

## Solución de Problemas

### Error de Conexión
```
Error: connect ECONNREFUSED 127.0.0.1:8181
```
**Solución**: Verifica que el servidor Renovatio esté ejecutándose en el puerto 8181.

### Respuesta "success=false"
```json
{"success": false, "message": null}
```
**Solución**: Verifica que la ruta en `scope` sea válida y contenga código Java.

### Herramienta no encontrada
```json
{"error": {"code": -32601, "message": "Method not found"}}
```
**Solución**: Verifica que el nombre de la herramienta sea correcto (ej: `java_analyze`, no `javaAnalyze`).

### NQL no implementado
```json
{"text": "NQL operation 'compile' not yet implemented"}
```
**Solución**: La implementación de NQL está en desarrollo. Usa consultas básicas por ahora.

## Próximos Pasos

1. **Experimenta**: Prueba diferentes herramientas con tu código Java.
2. **Combina herramientas**: Usa `java_analyze` seguido de `java_metrics` para análisis completo.
3. **Automatiza**: Crea scripts para automatizar flujos de trabajo comunes.
4. **Contribuye**: Reporta bugs y sugiere mejoras al proyecto Renovatio.