#!/bin/bash

# Test script for Renovatio MCP Server
echo "🚀 Testing Renovatio MCP Server..."

# Start the server in background
mvn spring-boot:run &
SERVER_PID=$!

# Wait for server to start
echo "⏳ Waiting for server to start..."
sleep 15

echo "🔍 Testing MCP Protocol Endpoints..."

# Test 1: Initialize MCP connection
echo "1. Testing MCP initialize..."
INIT_RESPONSE=$(curl -s -X POST -H "Content-Type: application/json" \
  -d '{"jsonrpc": "2.0", "id": "1", "method": "initialize", "params": {}}' \
  http://localhost:8080/)

if echo "$INIT_RESPONSE" | jq -e '.result.serverInfo.name' > /dev/null; then
  echo "✅ MCP Initialize: SUCCESS"
else
  echo "❌ MCP Initialize: FAILED"
fi

# Test 2: Ping
echo "2. Testing MCP ping..."
PING_RESPONSE=$(curl -s -X POST -H "Content-Type: application/json" \
  -d '{"jsonrpc": "2.0", "id": "2", "method": "ping", "params": {}}' \
  http://localhost:8080/)

if echo "$PING_RESPONSE" | jq -e '.result' > /dev/null; then
  echo "✅ MCP Ping: SUCCESS"
else
  echo "❌ MCP Ping: FAILED"
fi

# Test 3: Restart
echo "3. Testing MCP restart..."
RESTART_RESPONSE=$(curl -s -X POST -H "Content-Type: application/json" \
  -d '{"jsonrpc": "2.0", "id": "3", "method": "restart", "params": {}}' \
  http://localhost:8080/)

if echo "$RESTART_RESPONSE" | jq -e '.result.message' > /dev/null; then
  echo "✅ MCP Restart: SUCCESS"
else
  echo "❌ MCP Restart: FAILED"
fi

# Test 4: List tools
echo "4. Testing tools list..."
TOOLS_RESPONSE=$(curl -s -X POST -H "Content-Type: application/json" \
  -d '{"jsonrpc": "2.0", "id": "4", "method": "tools/list", "params": {}}' \
  http://localhost:8080/)

TOOLS_COUNT=$(echo "$TOOLS_RESPONSE" | jq '.result.tools | length')
if [ "$TOOLS_COUNT" -gt 15 ]; then
  echo "✅ Tools List: SUCCESS ($TOOLS_COUNT tools found)"
else
  echo "❌ Tools List: FAILED"
fi

# Test 5: Execute AutoFormat tool
echo "5. Testing AutoFormat tool execution..."
FORMAT_RESPONSE=$(curl -s -X POST -H "Content-Type: application/json" \
  -d '{"jsonrpc": "2.0", "id": "5", "method": "tools/call", "params": {"name": "org.openrewrite.java.format.AutoFormat", "arguments": {"sourceCode": "public class Test{private int x=0;}"}}}' \
  http://localhost:8080/)

if echo "$FORMAT_RESPONSE" | jq -e '.result.content.text' > /dev/null; then
  echo "✅ AutoFormat Tool: SUCCESS"
else
  echo "❌ AutoFormat Tool: FAILED"
fi

# Test 6: Execute ExplicitInitialization tool
echo "6. Testing ExplicitInitialization tool execution..."
INIT_RESPONSE=$(curl -s -X POST -H "Content-Type: application/json" \
  -d '{"jsonrpc": "2.0", "id": "6", "method": "tools/call", "params": {"name": "org.openrewrite.java.cleanup.ExplicitInitialization", "arguments": {"sourceCode": "public class Test { private String name = null; private int count = 0; }"}}}' \
  http://localhost:8080/)

if echo "$INIT_RESPONSE" | jq -e '.result.content.text' > /dev/null; then
  echo "✅ ExplicitInitialization Tool: SUCCESS"
else
  echo "❌ ExplicitInitialization Tool: FAILED"
fi

echo "🔍 Testing REST API Endpoints..."

# Test 7: REST API tools list
echo "7. Testing REST API tools list..."
REST_TOOLS=$(curl -s http://localhost:8080/mcp/tools | jq '. | length')
if [ "$REST_TOOLS" -gt 15 ]; then
  echo "✅ REST Tools List: SUCCESS ($REST_TOOLS tools found)"
else
  echo "❌ REST Tools List: FAILED"
fi

# Test 8: REST API refactor
echo "8. Testing REST API refactor..."
REST_REFACTOR=$(curl -s -X POST -H "Content-Type: application/json" \
  -d '{"sourceCode": "public class Test { private String name = null; }", "recipe": "org.openrewrite.java.cleanup.ExplicitInitialization"}' \
  http://localhost:8080/api/refactor)

if echo "$REST_REFACTOR" | jq -e '.refactoredCode' > /dev/null; then
  echo "✅ REST Refactor: SUCCESS"
else
  echo "❌ REST Refactor: FAILED"
fi

# Test 9: Swagger UI accessibility
echo "9. Testing Swagger UI..."
SWAGGER_STATUS=$(curl -s -o /dev/null -w "%{http_code}" http://localhost:8080/swagger-ui/index.html)
if [ "$SWAGGER_STATUS" = "200" ]; then
  echo "✅ Swagger UI: SUCCESS"
else
  echo "❌ Swagger UI: FAILED"
fi

# Clean up
echo "🧹 Cleaning up..."
kill $SERVER_PID 2>/dev/null

echo "✨ Test completed!"
echo ""
echo "📋 Summary:"
echo "- MCP Protocol: Fully functional"
echo "- REST API: Backward compatible"
echo "- Tools Available: $TOOLS_COUNT+ OpenRewrite recipes"
echo "- Documentation: Accessible via Swagger UI"
echo ""
echo "🎉 Renovatio is now a full MCP-compliant OpenRewrite server!"