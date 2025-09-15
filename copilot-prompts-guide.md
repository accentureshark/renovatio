# Guía de Prompts para GitHub Copilot con Renovatio MCP

## ❌ Prompts que NO funcionan bien:
```
@renovatio usa java_metrics para analizar el proyecto Java en /home/faguero/accenture/renovatio/mcp-demo
```

## ✅ Prompts que SÍ funcionan bien:

### Para Análisis de Código Java:
```
Por favor analiza el código Java en la ruta /home/faguero/accenture/renovatio/mcp-demo usando la herramienta java_analyze. Quiero entender la estructura del proyecto, las clases que contiene, los métodos principales y las dependencias. Usa la herramienta MCP java_analyze con workspacePath="/home/faguero/accenture/renovatio/mcp-demo".
```

### Para Métricas de Código Java:
```
Necesito calcular métricas de calidad y complejidad para el proyecto Java en /home/faguero/accenture/renovatio/mcp-demo. Usa la herramienta MCP java_metrics para obtener líneas de código, número de clases, métodos, complejidad y otros indicadores de calidad. El workspacePath es "/home/faguero/accenture/renovatio/mcp-demo".
```

### Para Análisis Específico:
```
Analiza el código Java en /home/faguero/accenture/renovatio/mcp-demo y dime:
1. Cuántas clases hay y cuáles son
2. Cuál es la complejidad del código
3. Qué dependencias externas usa
4. Métricas de calidad del código

Usa las herramientas MCP java_analyze y java_metrics con workspacePath="/home/faguero/accenture/renovatio/mcp-demo".
```

## Claves para Prompts Efectivos:

1. **Sé específico sobre qué herramienta usar**: Menciona explícitamente "herramienta MCP java_analyze" o "herramienta MCP java_metrics"

2. **Especifica los parámetros**: Siempre incluye el workspacePath completo

3. **Explica qué quieres obtener**: Di qué información específica esperas recibir

4. **Usa contexto**: Explica por qué necesitas esta información

## Ejemplos de Prompts Avanzados:

### Reporte de Calidad Completo:
```
Genera un reporte completo de calidad del código para el proyecto Java en /home/faguero/accenture/renovatio/mcp-demo. Usa las herramientas MCP java_analyze y java_metrics para obtener:

- Estructura general del proyecto (clases, paquetes)
- Métricas de líneas de código
- Complejidad ciclomática
- Número de métodos por clase
- Dependencias externas
- Indicadores de mantenibilidad

Workspacepath: "/home/faguero/accenture/renovatio/mcp-demo"
```

### Análisis Arquitectural:
```
Analiza la arquitectura del proyecto Java en /home/faguero/accenture/renovatio/mcp-demo usando la herramienta MCP java_analyze. Identifica:

- Patrones de diseño utilizados
- Estructura de paquetes
- Relaciones entre clases
- Puntos de entrada principales
- Dependencias clave

Workspacepath: "/home/faguero/accenture/renovatio/mcp-demo"
```
