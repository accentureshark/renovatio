#!/bin/bash

# Script para probar la herramienta java_analyze directamente contra el servidor MCP

echo "=== Testing MCP Server Tool Invocation ==="
echo "Testing java_analyze tool..."

# Test 1: List available tools
echo "1. Listing available tools:"
curl -s -X POST http://localhost:8080/mcp \
  -H "Content-Type: application/json" \
  -d '{
    "jsonrpc": "2.0",
    "id": 1,
    "method": "tools/list",
    "params": {}
  }' | jq '.'

echo -e "\n2. Testing java_analyze tool call:"
# Test 2: Call java_analyze with underscore
curl -s -X POST http://localhost:8080/mcp \
  -H "Content-Type: application/json" \
  -d '{
    "jsonrpc": "2.0",
    "id": 2,
    "method": "tools/call",
    "params": {
      "name": "java_analyze",
      "arguments": {
        "workspacePath": "/home/faguero/accenture/renovatio/mcp-demo"
      }
    }
  }' | jq '.'

echo -e "\n3. Testing java.analyze tool call (with dot):"
# Test 3: Call java.analyze with dot
curl -s -X POST http://localhost:8080/mcp \
  -H "Content-Type: application/json" \
  -d '{
    "jsonrpc": "2.0",
    "id": 3,
    "method": "tools/call",
    "params": {
      "name": "java.analyze",
      "arguments": {
        "workspacePath": "/home/faguero/accenture/renovatio/mcp-demo"
      }
    }
  }' | jq '.'

echo -e "\nTest completed."
