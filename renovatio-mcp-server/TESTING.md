# Renovatio MCP Server Integration Testing

This module provides an end-to-end integration test for the MCP HTTP JSON-RPC API using RestAssured: `McpIntegrationTest`.
The test suite assumes a running external MCP server. It will NOT start Spring; it calls the HTTP endpoint directly.

## What the test covers

- Health check: GET /mcp/health
- MCP core methods via POST /mcp (JSON-RPC 2.0):
  - initialize, ping, capabilities, server/info
  - tools/list, tools/describe, tools/call (generic for all tools returned)
  - cli/manifest
  - content/write, content/read
  - workspace/list, workspace/describe
  - logging/subscribe, logging/unsubscribe
  - prompts/list, prompts/get
  - resources/list, resources/read
  - shutdown (optional)
- Java provider flow: plan -> apply (dry-run) -> diff -> analyze -> metrics -> discover -> recipe_list/describe -> pipeline (dry-run)
- COBOL provider flow: analyze -> metrics

## Environment variables

- MCP_BASE_URL (optional): Base URL for the MCP server.
  - Default: http://127.0.0.1:8081 (auto-fallback to 8080 if 8081 is down)
- MCP_WORKSPACE (optional): Workspace root used for generic operations (content/write, etc.).
  - Default: current project directory
- MCP_WORKSPACE_JAVA (optional): Java workspace for Java tools.
  - Default: /home/faguero/accenture/melian/src/main/java
- MCP_WORKSPACE_COBOL (optional): COBOL workspace for COBOL tools.
  - Default: /home/faguero/accenture/renovatio/samples/cobol/Cobol-Programming-Collection/Cobol Utilities
- MCP_ALLOW_MUTATIONS (optional): If "true", enables potentially destructive tools (e.g., java.format; COBOL stub generation).
  - Default: false
- MCP_TEST_SHUTDOWN (optional): If "true", enables the `shutdown` test at the end (will stop your server!).
  - Default: false

## Run the test

From this module directory (renovatio-mcp-server):

```bash
# Ensure the MCP server is running and reachable at MCP_BASE_URL
export MCP_BASE_URL="http://127.0.0.1:8081"
export MCP_WORKSPACE_JAVA="/home/faguero/accenture/melian/src/main/java"
export MCP_WORKSPACE_COBOL="/home/faguero/accenture/renovatio/samples/cobol/Cobol-Programming-Collection/Cobol Utilities"
# Optional: allow mutations and enable shutdown
# export MCP_ALLOW_MUTATIONS="true"
# export MCP_TEST_SHUTDOWN="true"

mvn -Dtest=org.shark.renovatio.mcp.server.McpIntegrationTest test
```

Notes:
- If the MCP server is not reachable, the entire test class is skipped (class-level `assumeTrue`). Surefire may report `tests=0` in that case.
- The test prefers writing temp files inside `MCP_WORKSPACE` to maximize accessibility by the server sandbox.
- Paths containing spaces (e.g., COBOL path) are passed as-is in JSON and are supported by the test; ensure the server supports them on your OS.

## Troubleshooting

- 404/connection refused: Verify your server exposes `GET /mcp/health` and `POST /mcp` on `MCP_BASE_URL`.
- Tool call errors: The test asserts that either `result` exists or an `error.message` is returned for each tool call. If your tool requires specific parameters, add them as needed.
- Long runs/timeouts: You can run a subset of tests by targeting a single method, e.g.:

```bash
mvn -Dtest=org.shark.renovatio.mcp.server.McpIntegrationTest#java_plan_apply_diff_pipeline_analyze_metrics_discover_recipe_flow test
```

## Updating

If you add new MCP endpoints or tools, extend `McpIntegrationTest` accordingly so that the suite remains comprehensive.
