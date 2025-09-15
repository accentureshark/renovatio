#!/bin/bash

echo "=== DEBUG TEST: Java Metrics with Detailed Logging ==="

cd /home/faguero/accenture/renovatio

# Kill any existing server
pkill -f "renovatio-mcp-server" 2>/dev/null || true
sleep 2

echo "Starting MCP server with detailed logging..."

# Start server in background
cd renovatio-mcp-server
nohup java -jar target/renovatio-mcp-server-0.0.1-SNAPSHOT.jar > debug-detailed.log 2>&1 &
SERVER_PID=$!
echo "MCP Server started with PID: $SERVER_PID"

# Wait for server to initialize
echo "Waiting for server to initialize (15 seconds)..."
sleep 15

echo -e "\n=== Testing java.metrics with detailed logging ==="

# Test with the correct endpoint
RESPONSE=$(curl -s -X POST http://localhost:8080/ \
  -H "Content-Type: application/json" \
  -d '{
    "jsonrpc": "2.0",
    "id": "debug-test",
    "method": "tools/call",
    "params": {
      "name": "java.metrics",
      "arguments": {
        "workspacePath": "/home/faguero/accenture/renovatio/mcp-demo",
        "nql": "calculate comprehensive Java code metrics"
      }
    }
  }')

echo "MCP Response:"
echo "$RESPONSE" | python3 -m json.tool 2>/dev/null || echo "$RESPONSE"

echo -e "\n=== DETAILED SERVER LOGS ==="
echo "Looking for ROUTING TOOL CALL and CONVERT TO MAP DEBUG logs..."
grep -A 20 -B 5 "ROUTING TOOL CALL\|CONVERT TO MAP DEBUG\|Provider.metrics() returned" debug-detailed.log | tail -n 50

echo -e "\n=== Cleanup ==="
kill $SERVER_PID 2>/dev/null || true

echo -e "\n=== ANALYSIS ==="
echo "The detailed logs above should show us:"
echo "1. Whether the JavaLanguageProvider.metrics() method is being called"
echo "2. What MetricsResult is being returned"
echo "3. What happens in the convertToMap method"
echo "4. Whether the conversion is working correctly"
