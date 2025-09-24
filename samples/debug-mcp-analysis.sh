#!/bin/bash

echo "=== Diagnóstico MCP Server de Renovatio ==="
echo "Fecha: $(date)"
echo ""

echo "1. Verificando estado del servidor MCP..."
curl -s http://localhost:8080/actuator/health | jq .

echo -e "\n2. Listando herramientas MCP disponibles..."
curl -X POST http://localhost:8080/mcp \
  -H "Content-Type: application/json" \
  -d '{
    "jsonrpc": "2.0",
    "id": "list-tools",
    "method": "tools/list",
    "params": {}
  }' | jq '.result.tools[] | {name: .name, description: .description}'

echo -e "\n3. Verificando contenido del directorio mcp-demo..."
ls -la /home/faguero/accenture/renovatio/mcp-demo/

echo -e "\n4. Probando java_analyze con logging detallado..."
curl -X POST http://localhost:8080/mcp \
  -H "Content-Type: application/json" \
  -d '{
    "jsonrpc": "2.0",
    "id": "debug-java-analyze",
    "method": "tools/call",
    "params": {
      "name": "java_analyze",
      "arguments": {
        "workspacePath": "/home/faguero/accenture/renovatio/mcp-demo",
        "nql": "show all Java classes and methods"
      }
    }
  }' | jq .

echo -e "\n5. Probando java_metrics con configuración básica..."
curl -X POST http://localhost:8080/mcp \
  -H "Content-Type: application/json" \
  -d '{
    "jsonrpc": "2.0",
    "id": "debug-java-metrics",
    "method": "tools/call",
    "params": {
      "name": "java_metrics",
      "arguments": {
        "workspacePath": "/home/faguero/accenture/renovatio/mcp-demo",
        "nql": "calculate basic metrics"
      }
    }
  }' | jq .

echo -e "\n6. Probando nql_compile para verificar compilación NQL..."
curl -X POST http://localhost:8080/mcp \
  -H "Content-Type: application/json" \
  -d '{
    "jsonrpc": "2.0",
    "id": "debug-nql-compile",
    "method": "tools/call",
    "params": {
      "name": "nql_compile",
      "arguments": {
        "question": "find all Java classes",
        "context": "Java analysis"
      }
    }
  }' | jq .

echo -e "\n=== Fin del diagnóstico ==="
