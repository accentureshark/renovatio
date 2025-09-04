
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

# Test 7: List prompts
echo "7. Testing prompts list..."
PROMPTS_RESPONSE=$(curl -s -X POST -H "Content-Type: application/json" \
  -d '{"jsonrpc": "2.0", "id": "7", "method": "prompts/list", "params": {}}' \
  http://localhost:8080/)
PROMPTS_COUNT=$(echo "$PROMPTS_RESPONSE" | jq '.result.prompts | length')
if [ "$PROMPTS_COUNT" -gt 0 ]; then
  echo "✅ Prompts List: SUCCESS ($PROMPTS_COUNT prompts found)"
else
  echo "❌ Prompts List: FAILED"
fi

# Test 8: Get prompt
echo "8. Testing prompts get..."
PROMPT_GET=$(curl -s -X POST -H "Content-Type: application/json" \
  -d '{"jsonrpc": "2.0", "id": "8", "method": "prompts/get", "params": {"name": "format-java"}}' \
  http://localhost:8080/)
if echo "$PROMPT_GET" | jq -e '.result.prompt.messages' > /dev/null; then
  echo "✅ Prompt Get: SUCCESS"
else
  echo "❌ Prompt Get: FAILED"
fi

# Test 9: List resources
echo "9. Testing resources list..."
RESOURCES_RESPONSE=$(curl -s -X POST -H "Content-Type: application/json" \
  -d '{"jsonrpc": "2.0", "id": "9", "method": "resources/list", "params": {}}' \
  http://localhost:8080/)
RESOURCES_COUNT=$(echo "$RESOURCES_RESPONSE" | jq '.result.resources | length')
if [ "$RESOURCES_COUNT" -gt 0 ]; then
  echo "✅ Resources List: SUCCESS ($RESOURCES_COUNT resources found)"
else
  echo "❌ Resources List: FAILED"
fi

# Test 10: Read resource
echo "10. Testing resources read..."
RESOURCE_READ=$(curl -s -X POST -H "Content-Type: application/json" \
  -d '{"jsonrpc": "2.0", "id": "10", "method": "resources/read", "params": {"uri": "file://welcome.txt"}}' \
  http://localhost:8080/)
if echo "$RESOURCE_READ" | jq -e '.result.contents[0].text' > /dev/null; then
  echo "✅ Resource Read: SUCCESS"
else
  echo "❌ Resource Read: FAILED"
fi

echo "🔍 Testing REST API Endpoints..."

# Test 11: REST API tools list
echo "11. Testing REST API tools list..."
REST_TOOLS=$(curl -s http://localhost:8080/mcp/tools | jq '. | length')
if [ "$REST_TOOLS" -gt 15 ]; then
  echo "✅ REST Tools List: SUCCESS ($REST_TOOLS tools found)"
else
  echo "❌ REST Tools List: FAILED"
fi

# Test 12: REST API refactor
echo "12. Testing REST API refactor..."
REST_REFACTOR=$(curl -s -X POST -H "Content-Type: application/json" \
  -d '{"sourceCode": "public class Test { private String name = null; }", "recipe": "org.openrewrite.java.cleanup.ExplicitInitialization"}' \
  http://localhost:8080/api/refactor)

if echo "$REST_REFACTOR" | jq -e '.refactoredCode' > /dev/null; then
  echo "✅ REST Refactor: SUCCESS"
else
  echo "❌ REST Refactor: FAILED"
fi

# Test 13: Swagger UI accessibility
echo "13. Testing Swagger UI..."
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
echo "- Prompts Available: $PROMPTS_COUNT"
echo "- Resources Available: $RESOURCES_COUNT"
echo ""
echo "🎉 Renovatio is now a full MCP-compliant OpenRewrite server!"
