# GitHub Copilot Agent Prompt - Renovatio MCP Integration

## Prompt Principal para GitHub Copilot desde VS Code

```
Como agente de GitHub Copilot, necesito que te conectes al servidor MCP de Renovatio para analizar y modernizar código Java legacy.

CONTEXTO:
- Soy desarrollador trabajando en modernización de aplicaciones Java legacy
- Tengo acceso al servidor MCP Renovatio con herramientas especializadas
- Necesito análisis automatizado, métricas de calidad y recomendaciones de refactoring

SERVIDOR MCP DISPONIBLE:
- Nombre: "renovatio"  
- Herramientas: java_analyze, java_metrics, java_plan, java_apply, java_diff
- Workspace: /home/faguero/accenture/renovatio/mcp-demo

TAREAS SOLICITADAS:

1. 📊 ANÁLISIS DE MÉTRICAS
   Usa java_metrics para obtener:
   - Líneas de código (LOC) total
   - Número de clases y métodos
   - Complejidad ciclomática por archivo
   - Índice de mantenibilidad
   - Ratios de comentarios vs código
   - Indicadores de calidad (cobertura potencial, code smells)

2. 🔍 ANÁLISIS ESTRUCTURAL  
   Usa java_analyze para identificar:
   - Patrones arquitectónicos actuales
   - Dependencias problemáticas
   - Clases con alta complejidad
   - Métodos candidatos a refactoring
   - Oportunidades de modernización

3. 📋 PLAN DE MODERNIZACIÓN
   Usa java_plan para crear:
   - Roadmap de refactoring priorizado
   - Estimaciones de esfuerzo
   - Identificación de riesgos
   - Recomendaciones de mejores prácticas modernas

4. 📈 REPORTE EJECUTIVO
   Proporciona:
   - Resumen de estado actual del código
   - Principales problemas identificados  
   - ROI estimado de modernización
   - Próximos pasos recomendados

PARÁMETROS TÉCNICOS:
- workspacePath: "/home/faguero/accenture/renovatio/mcp-demo"
- nql: "comprehensive analysis for Java legacy modernization"
- Enfoque: Enterprise Java applications, Spring Boot migration readiness
- Criterios: Performance, maintainability, security, scalability

FORMATO ESPERADO:
- Usa las herramientas MCP en secuencia lógica
- Correlaciona resultados entre herramientas
- Presenta findings de forma accionable
- Incluye métricas específicas y recomendaciones concretas

¿Puedes proceder con este análisis completo usando las herramientas MCP de Renovatio?
```

## Prompts Específicos por Funcionalidad

### 🔧 Prompt para Análisis Rápido de Calidad

```
GitHub Copilot, conecta con el servidor MCP "renovatio" y usa java_metrics para evaluar rápidamente la calidad del código Java en /home/faguero/accenture/renovatio/mcp-demo.

Necesito un dashboard executivo con:
- Health score general del código (1-10)
- Top 3 problemas críticos
- Recomendación inmediata más impactante
- Tiempo estimado para quick wins

Usa estos parámetros:
- workspacePath: "/home/faguero/accenture/renovatio/mcp-demo"
- nql: "executive quality dashboard with actionable insights"
```

### 🚀 Prompt para Plan de Migración a Spring Boot

```
Como agente Copilot, usa las herramientas MCP de Renovatio para crear un plan detallado de migración del código Java legacy en /home/faguero/accenture/renovatio/mcp-demo hacia Spring Boot moderno.

ANÁLISIS REQUERIDO:
1. java_analyze: Identificar patrones legacy incompatibles
2. java_metrics: Baseline de complejidad actual  
3. java_plan: Roadmap específico para Spring Boot migration

ENTREGABLES:
- Checklist de pre-requisitos técnicos
- Secuencia de refactoring por módulos
- Estimación de esfuerzo por fase
- Riesgos y mitigaciones específicas

workspacePath: "/home/faguero/accenture/renovatio/mcp-demo"
nql: "Spring Boot migration roadmap with risk assessment"
```

### 🔍 Prompt para Code Review Automatizado

```
Copilot, actúa como senior architect y usa el servidor MCP Renovatio para hacer un code review comprehensivo del proyecto Java.

PROCESO:
1. java_analyze: Arquitectura y patrones actuales
2. java_metrics: Hotspots de complejidad y calidad
3. java_diff: Comparación con best practices

CRITERIOS DE REVIEW:
- SOLID principles compliance
- Performance bottlenecks
- Security vulnerabilities patterns  
- Maintainability issues
- Test coverage gaps

OUTPUT: Pull Request-style review con:
- ✅ What's working well
- ⚠️ Issues to address  
- 🚨 Critical problems
- 💡 Improvement suggestions

workspacePath: "/home/faguero/accenture/renovatio/mcp-demo"
nql: "comprehensive code review with architectural recommendations"
```

### 📊 Prompt para Métricas de Progreso

```
GitHub Copilot, conéctate al MCP Renovatio y establece un baseline de métricas para tracking de progreso de modernización.

MÉTRICAS BASELINE:
- Complexity score actual
- Technical debt estimation  
- Code coverage potential
- Refactoring readiness index

TRACKING SETUP:
- KPIs para medir progreso
- Thresholds de calidad objetivo
- Frequency de re-evaluación recomendada

Usa java_metrics con:
workspacePath: "/home/faguero/accenture/renovatio/mcp-demo"  
nql: "establish modernization progress baseline with KPI tracking"

Genera un dashboard template que pueda usar en sprints futuros.
```

## Prompt para Troubleshooting de Conectividad MCP

```
Copilot, diagnóstica la conectividad con el servidor MCP Renovatio:

1. Verifica que el servidor "renovatio" esté disponible
2. Lista las herramientas MCP accesibles  
3. Ejecuta un test básico con java_analyze
4. Si hay problemas, proporciona pasos de solución

Parámetros de test:
- workspacePath: "/home/faguero/accenture/renovatio/mcp-demo"
- nql: "connectivity test and basic analysis"

Si la conexión falla, guíame paso a paso para:
- Verificar configuración mcp.json
- Restar el servidor MCP
- Validar paths y permisos
```

## Configuración de Contexto para GitHub Copilot

Para optimizar las respuestas, incluye este contexto en tus conversaciones con Copilot:

```
CONTEXT: Working with Renovatio MCP Server
- Language: Java enterprise applications
- Focus: Legacy modernization & refactoring  
- Tools: OpenRewrite, Spring Boot, Maven
- MCP Server: renovatio (java_analyze, java_metrics, java_plan, java_apply, java_diff)
- Workspace: /home/faguero/accenture/renovatio/mcp-demo
- Goal: Automated code analysis and modernization recommendations
```
