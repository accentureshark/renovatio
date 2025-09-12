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
if [ ! -f "renovatio-mcp-server/target/renovatio-mcp-server-0.0.1-SNAPSHOT.jar" ]; then
    echo "[Renovatio MCP] Building project..." >&2
    mvn clean package -DskipTests -q
fi

# Run the MCP stdio server using Spring Boot
java -jar "renovatio-mcp-server/target/renovatio-mcp-server-0.0.1-SNAPSHOT.jar" \
     --spring.main.web-application-type=none \
     --spring.main.banner-mode=off \
     --logging.level.root=ERROR \
     "$@"
