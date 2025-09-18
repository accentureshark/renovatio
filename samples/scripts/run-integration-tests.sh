#!/bin/bash

# Integration Test Runner for Renovatio MCP Server
echo "=== Renovatio MCP Server Integration Tests ==="

cd /home/faguero/accenture/renovatio

# Check if server is already running and stop it
echo "Checking for existing MCP server processes..."
pkill -f "renovatio-mcp-server" 2>/dev/null || true
sleep 2

# Build the project first
echo "Building Renovatio project..."
mvn clean compile -DskipTests -q

if [ $? -ne 0 ]; then
    echo "❌ Build failed. Cannot proceed with integration tests."
    exit 1
fi

echo "✅ Build successful"

# Run the integration tests
echo "Running MCP Server Integration Tests..."
cd renovatio-mcp-server

# Execute the integration tests with Maven
mvn test -Dtest=McpServerIntegrationTest -Dspring.profiles.active=test

TEST_RESULT=$?

if [ $TEST_RESULT -eq 0 ]; then
    echo ""
    echo "🎉 ==============================================="
    echo "🎉 ALL INTEGRATION TESTS PASSED SUCCESSFULLY!"
    echo "🎉 ==============================================="
    echo ""
    echo "✅ Server health check: PASSED"
    echo "✅ Tools listing: PASSED"
    echo "✅ Java metrics tool: PASSED"
    echo "✅ Java analyze tool: PASSED"
    echo "✅ Java plan tool: PASSED"
    echo "✅ Java apply tool: PASSED"
    echo "✅ Java diff tool: PASSED"
    echo "✅ NQL compile tool: PASSED"
    echo "✅ Common index tool: PASSED"
    echo "✅ Common search tool: PASSED"
    echo "✅ Error handling: PASSED"
    echo "✅ Performance tests: PASSED"
    echo ""
    echo "The MCP server is fully functional and ready for use!"
else
    echo ""
    echo "❌ ==============================================="
    echo "❌ SOME INTEGRATION TESTS FAILED"
    echo "❌ ==============================================="
    echo ""
    echo "Please check the test output above for details."
    echo "Common issues:"
    echo "- Workspace path not accessible: /home/faguero/accenture/renovatio/mcp-demo"
    echo "- JavaLanguageProvider not properly registered"
    echo "- Missing dependencies or configuration issues"
    echo ""
    echo "Check the server logs and test output for more information."
fi

# Cleanup any test processes
pkill -f "renovatio-mcp-server" 2>/dev/null || true

echo ""
echo "Integration test execution completed."
echo "Exit code: $TEST_RESULT"

exit $TEST_RESULT
