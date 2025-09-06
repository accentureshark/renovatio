#!/bin/bash

# Renovatio COBOL Migration Demo Script
# This script demonstrates the COBOL to Java migration capabilities

echo "🚀 Renovatio COBOL Migration Demo"
echo "=================================="

# Server URL (adjust as needed)
SERVER_URL="http://localhost:8181"

echo ""
echo "📊 1. Analyzing COBOL workspace..."
curl -s -X POST -H "Content-Type: application/json" \
  -d '{
    "jsonrpc": "2.0",
    "id": "1",
    "method": "tools/call",
    "params": {
      "name": "cobol.analyze",
      "arguments": {
        "workspacePath": "/path/to/cobol/project",
        "includeMetrics": true
      }
    }
  }' \
  $SERVER_URL | jq '.'

echo ""
echo "🏗️ 2. Generating Java stubs from COBOL..."
curl -s -X POST -H "Content-Type: application/json" \
  -d '{
    "jsonrpc": "2.0",
    "id": "2",
    "method": "tools/call",
    "params": {
      "name": "cobol.generate.stubs",
      "arguments": {
        "workspacePath": "/path/to/cobol/project",
        "targetPackage": "com.example.cobol.migrated",
        "generateTests": true
      }
    }
  }' \
  $SERVER_URL | jq '.'

echo ""
echo "📋 3. Creating migration plan..."
PLAN_RESPONSE=$(curl -s -X POST -H "Content-Type: application/json" \
  -d '{
    "jsonrpc": "2.0",
    "id": "3",
    "method": "tools/call",
    "params": {
      "name": "cobol.migration.plan",
      "arguments": {
        "workspacePath": "/path/to/cobol/project",
        "migrationStrategy": "incremental",
        "targetFramework": "spring-boot"
      }
    }
  }' \
  $SERVER_URL)

echo $PLAN_RESPONSE | jq '.'

# Extract plan ID for next step
PLAN_ID=$(echo $PLAN_RESPONSE | jq -r '.result.planId // "test-plan-id"')

echo ""
echo "⚡ 4. Applying migration plan (dry run)..."
curl -s -X POST -H "Content-Type: application/json" \
  -d "{
    \"jsonrpc\": \"2.0\",
    \"id\": \"4\",
    \"method\": \"tools/call\",
    \"params\": {
      \"name\": \"cobol.migration.apply\",
      \"arguments\": {
        \"planId\": \"$PLAN_ID\",
        \"dryRun\": true,
        \"outputPath\": \"/tmp/migrated-java\"
      }
    }
  }" \
  $SERVER_URL | jq '.'

echo ""
echo "📈 5. Calculating code metrics..."
curl -s -X POST -H "Content-Type: application/json" \
  -d '{
    "jsonrpc": "2.0",
    "id": "5",
    "method": "tools/call",
    "params": {
      "name": "cobol.metrics",
      "arguments": {
        "workspacePath": "/path/to/cobol/project",
        "includeComplexity": true,
        "includeDependencies": true
      }
    }
  }' \
  $SERVER_URL | jq '.'

echo ""
echo "✅ Demo completed!"
echo ""
echo "📚 Available COBOL Migration Tools:"
echo "   • cobol.analyze - Analyze COBOL programs"
echo "   • cobol.generate.stubs - Generate Java stubs"
echo "   • cobol.migration.plan - Create migration plan"
echo "   • cobol.migration.apply - Apply migration plan"
echo "   • cobol.metrics - Calculate code metrics"
echo "   • cobol.diff - Generate migration diffs"
echo ""
echo "🔧 To use with real COBOL projects:"
echo "   1. Update workspacePath to point to your COBOL directory"
echo "   2. Ensure the Renovatio server is running on $SERVER_URL"
echo "   3. Run: chmod +x demo-cobol-migration.sh && ./demo-cobol-migration.sh"
echo ""
echo "📖 For more information, see renovatio-provider-cobol/README.md"