#!/bin/bash

# Renovatio MCP Server Demo
echo "🎯 Renovatio OpenRewrite MCP Server - Demo"
echo "==========================================="
echo ""

# Start server
echo "🚀 Starting Renovatio MCP Server..."
mvn spring-boot:run > /dev/null 2>&1 &
SERVER_PID=$!
sleep 12

echo "✅ Server started on http://localhost:8080"
echo ""

# Demo 1: Show MCP capabilities
echo "📡 MCP Server Capabilities:"
echo "============================"
curl -s -X POST -H "Content-Type: application/json" \
  -d '{"jsonrpc": "2.0", "id": "1", "method": "initialize", "params": {}}' \
  http://localhost:8080/ | jq '.result.serverInfo'
echo ""

# Demo 2: Show available tools (first 5)
echo "🛠️  Available OpenRewrite Tools (sample):"
echo "=========================================="
curl -s -X POST -H "Content-Type: application/json" \
  -d '{"jsonrpc": "2.0", "id": "2", "method": "tools/list", "params": {}}' \
  http://localhost:8080/ | jq '.result.tools[0:5] | .[] | {name: .name, description: .description}'
echo ""

# Demo 3: Code formatting example
echo "✨ Code Formatting Demo:"
echo "========================"
echo "Before:"
echo 'public class Test{private int x=0;public void test(){System.out.println("Hello");}}'
echo ""
echo "After AutoFormat:"
FORMATTED_RESULT=$(curl -s -X POST -H "Content-Type: application/json" \
  -d '{"jsonrpc": "2.0", "id": "3", "method": "tools/call", "params": {"name": "org.openrewrite.java.format.AutoFormat", "arguments": {"sourceCode": "public class Test{private int x=0;public void test(){System.out.println(\"Hello\");}}"}}}' \
  http://localhost:8080/ | jq -r '.result.content.text')
echo "$FORMATTED_RESULT"
echo ""

# Demo 4: Explicit initialization cleanup
echo "🧹 Explicit Initialization Cleanup Demo:"
echo "========================================"
echo "Before:"
echo 'public class Test { private String name = null; private int count = 0; private boolean active = false; }'
echo ""
echo "After cleanup:"
CLEANUP_RESULT=$(curl -s -X POST -H "Content-Type: application/json" \
  -d '{"jsonrpc": "2.0", "id": "4", "method": "tools/call", "params": {"name": "org.openrewrite.java.cleanup.ExplicitInitialization", "arguments": {"sourceCode": "public class Test { private String name = null; private int count = 0; private boolean active = false; }"}}}' \
  http://localhost:8080/ | jq -r '.result.content.text')
echo "$CLEANUP_RESULT"
echo ""

# Show endpoints
echo "📚 Available Endpoints:"
echo "======================"
echo "• MCP Protocol: POST http://localhost:8080/"
echo "• REST API: http://localhost:8080/mcp/tools"
echo "• Swagger UI: http://localhost:8080/swagger-ui/index.html"
echo ""

echo "🎉 Demo completed! Server is still running..."
echo "Press Ctrl+C to stop the server when you're done exploring."
echo ""

# Keep server running and wait for user to stop
wait $SERVER_PID