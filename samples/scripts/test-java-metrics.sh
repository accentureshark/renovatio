#!/bin/bash

echo "=== Testing java_metrics command specifically ==="

# Test java_metrics with the correct MCP format
echo "Testing java_metrics tool call:"

curl -X POST http://localhost:8080/mcp \
  -H "Content-Type: application/json" \
  -d '{
    "jsonrpc": "2.0",
    "id": 1,
    "method": "tools/call",
    "params": {
      "name": "java_metrics",
      "arguments": {
        "workspacePath": "/home/faguero/accenture/renovatio/mcp-demo"
      }
    }
  }' | jq .

echo ""
echo "=== Test completed ==="
