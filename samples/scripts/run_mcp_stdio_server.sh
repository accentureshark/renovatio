#!/bin/bash

# Renovatio MCP Server - Stdio Transport
# This script starts the MCP server with stdio transport for direct MCP client communication

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$SCRIPT_DIR"

echo "[Renovatio MCP] Starting MCP stdio server..." >&2

# Change to project root
cd "$PROJECT_ROOT"

# Build the project if needed
echo "[Renovatio MCP] Building project..." >&2
mvn clean compile -q

# Set up classpath
CLASSPATH="renovatio-mcp-server/target/classes"
CLASSPATH="$CLASSPATH:renovatio-core/target/classes"
CLASSPATH="$CLASSPATH:renovatio-provider-java/target/classes"
CLASSPATH="$CLASSPATH:renovatio-provider-cobol/target/classes"
CLASSPATH="$CLASSPATH:renovatio-shared/target/classes"

# Add Maven dependencies to classpath
for jar in $(find ~/.m2/repository -name "*.jar" 2>/dev/null | head -100); do
    CLASSPATH="$CLASSPATH:$jar"
done

echo "[Renovatio MCP] Starting STDIO server with corrected JavaLanguageProvider..." >&2

# Run the MCP stdio server directly
java -cp "$CLASSPATH" \
     -Dlogging.level.root=DEBUG \
     -Dlogging.level.org.shark.renovatio=DEBUG \
     org.shark.renovatio.mcp.server.McpStdioServerApplication
