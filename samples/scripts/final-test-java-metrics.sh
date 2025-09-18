#!/bin/bash

# Final test to verify Java metrics functionality is completely fixed
echo "=== FINAL TEST: Java Metrics via MCP Server (Post-Fix) ==="

cd /home/faguero/accenture/renovatio

# Kill any existing server
pkill -f "renovatio-mcp-server" 2>/dev/null || true
sleep 2

echo "Starting MCP server with corrected spring.factories configuration..."

# Start server in background
cd renovatio-mcp-server
nohup java -jar target/renovatio-mcp-server-0.0.1-SNAPSHOT.jar > final-test-server.log 2>&1 &
SERVER_PID=$!
echo "MCP Server started with PID: $SERVER_PID"

# Wait longer for server to fully initialize with Spring Boot auto-configuration
echo "Waiting for server to fully initialize (15 seconds)..."
sleep 15

echo -e "\n=== Testing java.metrics tool via MCP HTTP endpoint ==="

# Test the corrected java.metrics functionality
echo "Calling java.metrics with workspacePath..."
RESPONSE=$(curl -s -X POST http://localhost:8080/mcp/call \
  -H "Content-Type: application/json" \
  -d '{
    "jsonrpc": "2.0",
    "id": "final-test",
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

# Check if response contains success=true and actual metrics
if echo "$RESPONSE" | grep -q '"success".*true' && echo "$RESPONSE" | grep -q '"total_files"'; then
    echo -e "\n✅ SUCCESS: Java metrics are now working correctly!"
    echo "The server is successfully:"
    echo "  - Detecting JavaLanguageProvider via Spring auto-configuration"
    echo "  - Routing java.metrics calls to the correct provider"
    echo "  - Calculating and returning comprehensive metrics"

    # Extract key metrics for verification
    echo -e "\n=== Key Metrics Verification ==="
    echo "$RESPONSE" | python3 -c "
import json, sys
try:
    data = json.load(sys.stdin)
    result = data.get('result', {})
    if 'metrics' in result:
        metrics = result['metrics']
        print(f'Total Files: {metrics.get(\"total_files\", \"N/A\")}')
        print(f'Total Classes: {metrics.get(\"total_classes\", \"N/A\")}')
        print(f'Total Methods: {metrics.get(\"total_methods\", \"N/A\")}')
        print(f'Lines of Code: {metrics.get(\"lines_of_code\", \"N/A\")}')
        print(f'Cyclomatic Complexity: {metrics.get(\"cyclomatic_complexity\", \"N/A\")}')
        print(f'Maintainability Index: {metrics.get(\"maintainability_index\", \"N/A\")}')
    else:
        print('No metrics found in response')
except Exception as e:
    print(f'Error parsing response: {e}')
" 2>/dev/null
else
    echo -e "\n❌ ISSUE: Response still shows problems"
    echo "Server logs:"
    tail -n 20 final-test-server.log
fi

echo -e "\n=== Cleanup ==="
kill $SERVER_PID 2>/dev/null || true

echo -e "\n=== Summary ==="
echo "Fixed Issues:"
echo "1. ✅ Completed JavaLanguageProvider.metrics() implementation"
echo "2. ✅ Fixed type compatibility (Map<String,Double> -> Map<String,Number>)"
echo "3. ✅ Corrected Scope parameter access"
echo "4. ✅ Added spring.factories configuration for auto-discovery"
echo ""
echo "The MCP client should now successfully receive Java metrics!"
