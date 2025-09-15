# VS Code MCP Client - Prompts para Renovatio

## Configuración del Cliente MCP en VS Code

Primero, asegúrate de que tu archivo `mcp.json` esté configurado correctamente:

```json
{
  "mcpServers": {
    "renovatio": {
      "command": "java",
      "args": [
        "-jar", 
        "/home/faguero/accenture/renovatio/renovatio-mcp-server/target/renovatio-mcp-server-0.0.1-SNAPSHOT.jar"
      ],
      "env": {
        "JAVA_HOME": "/usr/lib/jvm/java-17-openjdk-amd64"
      }
    }
  }
}
```

## Herramientas Disponibles en Renovatio MCP Server

El servidor Renovatio expone las siguientes herramientas MCP:

### 🔍 Análisis de Código Java
- **java_analyze**: Analiza estructura y extrae información de código Java
- **java_metrics**: Calcula métricas de calidad y complejidad para código Java

### 📊 Planificación y Aplicación
- **java_plan**: Crea plan de ejecución para transformaciones Java
- **java_apply**: Aplica plan de transformación Java
- **java_diff**: Genera diff semántico para Java

### 🔧 Herramientas Comunes
- **nql_compile**: Compila lenguaje natural a NQL
- **common_index**: Indexa repositorio para búsqueda
- **common_search**: Busca en repositorio indexado

## Prompts Optimizados para VS Code MCP Client

### 1. 📊 Análisis de Métricas de Código Java

```
Usa la herramienta MCP java_metrics para calcular métricas de calidad y complejidad del proyecto Java en /home/faguero/accenture/renovatio/mcp-demo. 

Necesito obtener:
- Líneas de código (LOC)
- Número de clases y métodos
- Complejidad ciclomática
- Índice de mantenibilidad
- Ratios de comentarios
- Indicadores de calidad del código

Parámetros:
- workspacePath: "/home/faguero/accenture/renovatio/mcp-demo"
- nql: "calculate comprehensive Java code metrics including LOC, classes, methods, complexity, and quality indicators"
```

### 2. 🔍 Análisis Estructural de Código Java

```
Utiliza la herramienta java_analyze del servidor MCP Renovatio para analizar la estructura del código Java en /home/faguero/accenture/renovatio/mcp-demo.

Necesito información sobre:
- Clases, interfaces y enums encontrados
- Métodos y sus firmas
- Dependencias e imports
- Estructura del AST
- Símbolos definidos

Parámetros:
- workspacePath: "/home/faguero/accenture/renovatio/mcp-demo"
- nql: "analyze Java code structure, extract classes, methods, dependencies and AST information"
```

### 3. 📋 Planificación de Refactoring

```
Usa la herramienta java_plan del servidor MCP Renovatio para crear un plan de refactoring para el proyecto Java en /home/faguero/accenture/renovatio/mcp-demo.

Objetivo: Modernizar el código Java aplicando mejores prácticas y patrones actuales.

Parámetros:
- workspacePath: "/home/faguero/accenture/renovatio/mcp-demo"  
- nql: "create refactoring plan to modernize Java code with current best practices and patterns"
- scope: "/home/faguero/accenture/renovatio/mcp-demo"
```

### 4. 🔄 Aplicar Transformaciones

```
Utiliza la herramienta java_apply del servidor MCP Renovatio para aplicar las transformaciones del plan especificado.

Parámetros:
- planId: "[ID del plan generado previamente]"
- dryRun: true
- workspacePath: "/home/faguero/accenture/renovatio/mcp-demo"
```

### 5. 🔍 Búsqueda en Código

```
Usa las herramientas common_index y common_search del servidor MCP Renovatio para indexar y buscar en el repositorio.

Primero indexa:
- workspacePath: "/home/faguero/accenture/renovatio/mcp-demo"

Luego busca:
- query: "métodos que contienen 'http' o 'transport'"
- workspacePath: "/home/faguero/accenture/renovatio/mcp-demo"
```

## Prompt Genérico para Debugging

Si una herramienta no responde correctamente:

```
Verifica el estado del servidor MCP Renovatio y usa la herramienta java_metrics para diagnosticar problemas.

Pasos de troubleshooting:
1. Confirma que el servidor MCP esté ejecutándose
2. Verifica la ruta del workspace: /home/faguero/accenture/renovatio/mcp-demo
3. Prueba la conectividad MCP con una herramienta simple
4. Revisa los logs del servidor para errores

Si persisten problemas, ejecuta java_analyze como alternativa para verificar conectividad básica.
```

## Formato de Respuesta Esperado

Cuando uses las herramientas MCP, deberías recibir respuestas en formato JSON-RPC 2.0:

```json
{
  "jsonrpc": "2.0",
  "id": "request-id",
  "result": {
    "content": [
      {
        "type": "text",
        "text": "Resultados de la herramienta..."
      }
    ]
  }
}
```

Para métricas Java específicamente:
```json
{
  "success": true,
  "message": "Successfully calculated metrics for X Java files",
  "metrics": {
    "total_files": 6,
    "total_classes": 37,
    "total_methods": 91,
    "lines_of_code": 1389,
    "cyclomatic_complexity": 110.0,
    "maintainability_index": 85.5,
    "comment_ratio": 0.05,
    "avg_methods_per_class": 2.46
  },
  "type": "metrics"
}
```

## Troubleshooting

Si no recibes respuestas:

1. **Verifica el servidor**: Asegúrate de que el JAR del servidor MCP esté corriendo:
   ```bash
   ps aux | grep renovatio-mcp-server
   ```

2. **Revisa los logs**: Verifica que no haya errores en los logs del servidor MCP

3. **Prueba conectividad**: Usa una herramienta simple como `java_analyze` antes de `java_metrics`

4. **Verifica paths**: Confirma que el `workspacePath` sea correcto y accesible

## Ejemplo de Uso Completo

```
Necesito un análisis completo del proyecto Java en /home/faguero/accenture/renovatio/mcp-demo usando las herramientas MCP de Renovatio.

Por favor:

1. Usa java_metrics para obtener métricas de calidad:
   - workspacePath: "/home/faguero/accenture/renovatio/mcp-demo"
   - nql: "comprehensive Java metrics analysis"

2. Usa java_analyze para obtener estructura:
   - workspacePath: "/home/faguero/accenture/renovatio/mcp-demo" 
   - nql: "detailed structural analysis"

3. Si encuentras problemas de calidad, usa java_plan para sugerir mejoras:
   - workspacePath: "/home/faguero/accenture/renovatio/mcp-demo"
   - nql: "refactoring plan for code quality improvements"

Proporciona un resumen ejecutivo con recomendaciones basadas en los resultados.
```

Este prompt debería funcionar correctamente con el servidor MCP Renovatio que hemos configurado y corregido.
