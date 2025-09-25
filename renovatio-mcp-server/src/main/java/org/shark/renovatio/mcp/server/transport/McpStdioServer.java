package org.shark.renovatio.mcp.server.transport;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.shark.renovatio.mcp.server.model.ToolCallResult;
import org.shark.renovatio.mcp.server.service.McpToolingService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.io.*;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

/**
 * MCP Server that handles stdio transport according to MCP specification.
 * This enables direct communication with MCP clients like VS Code extensions.
 */
@Component
public class McpStdioServer {

    private final ObjectMapper objectMapper = new ObjectMapper();
    private final ExecutorService executor = Executors.newSingleThreadExecutor();

    @Autowired
    private McpToolingService mcpToolingService;

    private volatile boolean running = false;
    private BufferedReader stdin;
    private PrintWriter stdout;

    /**
     * Start the MCP stdio server
     */
    public CompletableFuture<Void> start() {
        return CompletableFuture.runAsync(() -> {
            try {
                stdin = new BufferedReader(new InputStreamReader(System.in));
                stdout = new PrintWriter(System.out, true);
                running = true;

                System.err.println("[MCP Server] Starting stdio transport...");

                String line;
                while (running && (line = stdin.readLine()) != null) {
                    if (line.trim().isEmpty()) {
                        continue;
                    }

                    try {
                        handleRequest(line.trim());
                    } catch (Exception e) {
                        System.err.println("[MCP Server] Error handling request: " + e.getMessage());
                        sendErrorResponse(null, -32603, "Internal error: " + e.getMessage());
                    }
                }
            } catch (IOException e) {
                System.err.println("[MCP Server] IO Error: " + e.getMessage());
            } finally {
                cleanup();
            }
        }, executor);
    }

    /**
     * Stop the MCP stdio server
     */
    public void stop() {
        running = false;
        cleanup();
    }

    private void handleRequest(String jsonRequest) {
        try {
            System.err.println("[MCP Server] Received request: " + jsonRequest);

            JsonNode requestNode = objectMapper.readTree(jsonRequest);

            String jsonrpc = requestNode.has("jsonrpc") ? requestNode.get("jsonrpc").asText() : "2.0";
            Object id = requestNode.has("id") ?
                    (requestNode.get("id").isNull() ? null : requestNode.get("id").asText()) : null;
            String method = requestNode.has("method") ? requestNode.get("method").asText() : null;
            JsonNode params = requestNode.has("params") ? requestNode.get("params") : null;

            if (method == null) {
                sendErrorResponse(id, -32600, "Invalid Request - missing method");
                return;
            }

            System.err.println("[MCP Server] Processing method: " + method);

            // Handle initialize method specifically
            if ("initialize".equals(method)) {
                handleInitialize(id);
                return;
            }

            // Handle tools/list method
            if ("tools/list".equals(method)) {
                handleToolsList(id);
                return;
            }

            // Handle tools/call method
            if ("tools/call".equals(method)) {
                handleToolsCall(id, params);
                return;
            }

            // Handle other methods
            sendErrorResponse(id, -32601, "Method not found: " + method);

        } catch (Exception e) {
            System.err.println("[MCP Server] JSON parsing error: " + e.getMessage());
            e.printStackTrace();
            sendErrorResponse(null, -32700, "Parse error");
        }
    }

    private void handleInitialize(Object id) {
        try {
            Map<String, Object> capabilities = new HashMap<>();
            capabilities.put("tools", Map.of("listChanged", true));
            capabilities.put("resources", Map.of("listChanged", true));
            capabilities.put("prompts", Map.of());
            capabilities.put("logging", Map.of());

            Map<String, Object> serverInfo = Map.of(
                    "name", "Renovatio MCP Server",
                    "version", "1.0.0"
            );

            Map<String, Object> result = new HashMap<>();
            result.put("protocolVersion", "2024-11-05");
            result.put("serverInfo", serverInfo);
            result.put("capabilities", capabilities);

            sendSuccessResponse(id, result);

        } catch (Exception e) {
            System.err.println("[MCP Server] Error in initialize: " + e.getMessage());
            sendErrorResponse(id, -32603, "Internal error during initialization");
        }
    }

    private void handleToolsList(Object id) {
        try {
            // Get tools from the service
            var tools = mcpToolingService.getMcpTools();

            Map<String, Object> result = new HashMap<>();
            result.put("tools", tools);

            sendSuccessResponse(id, result);

        } catch (Exception e) {
            System.err.println("[MCP Server] Error in tools/list: " + e.getMessage());
            e.printStackTrace();
            sendErrorResponse(id, -32603, "Internal error getting tools");
        }
    }

    private void handleToolsCall(Object id, JsonNode params) {
        try {
            if (params == null || !params.has("name")) {
                sendErrorResponse(id, -32602, "Invalid params - missing tool name");
                return;
            }

            String toolName = normalizeToolName(params.get("name").asText());
            JsonNode arguments = params.has("arguments") ? params.get("arguments") : null;

            System.err.println("[MCP Server] Calling tool: " + toolName + " with arguments: " + arguments);

            // Convert JsonNode arguments to Map
            Map<String, Object> argumentsMap = new HashMap<>();
            if (arguments != null && arguments.isObject()) {
                arguments.fields().forEachRemaining(entry -> {
                    JsonNode value = entry.getValue();
                    if (value.isTextual()) {
                        argumentsMap.put(entry.getKey(), value.asText());
                    } else if (value.isNumber()) {
                        argumentsMap.put(entry.getKey(), value.asDouble());
                    } else if (value.isBoolean()) {
                        argumentsMap.put(entry.getKey(), value.asBoolean());
                    } else {
                        argumentsMap.put(entry.getKey(), value.toString());
                    }
                });
            }

            ToolCallResult result = mcpToolingService.executeToolWithEnvelope(toolName, argumentsMap);
            sendSuccessResponse(id, result);

        } catch (Exception e) {
            System.err.println("[MCP Server] Error in tools/call: " + e.getMessage());
            e.printStackTrace();
            sendErrorResponse(id, -32603, "Internal error calling tool: " + e.getMessage());
        }
    }

    private void sendSuccessResponse(Object id, Object result) {
        Map<String, Object> response = new HashMap<>();
        response.put("jsonrpc", "2.0");
        response.put("id", id);
        response.put("result", result);

        try {
            String jsonResponse = objectMapper.writeValueAsString(response);
            stdout.println(jsonResponse);
            stdout.flush();

            System.err.println("[MCP Server] Sent response: " + jsonResponse);
        } catch (Exception e) {
            System.err.println("[MCP Server] Error sending response: " + e.getMessage());
        }
    }

    private void sendErrorResponse(Object id, int code, String message) {
        Map<String, Object> error = new HashMap<>();
        error.put("code", code);
        error.put("message", message);

        Map<String, Object> response = new HashMap<>();
        response.put("jsonrpc", "2.0");
        response.put("id", id);
        response.put("error", error);

        try {
            String jsonResponse = objectMapper.writeValueAsString(response);
            stdout.println(jsonResponse);
            stdout.flush();

            System.err.println("[MCP Server] Sent error response: " + jsonResponse);
        } catch (Exception e) {
            System.err.println("[MCP Server] Error sending error response: " + e.getMessage());
        }
    }

    private void cleanup() {
        try {
            if (stdin != null) stdin.close();
            if (stdout != null) stdout.close();
        } catch (IOException e) {
            System.err.println("[MCP Server] Error during cleanup: " + e.getMessage());
        }
        executor.shutdown();
    }

    private String normalizeToolName(String name) {
        if (name == null) {
            return null;
        }
        if (name.contains("_")) {
            return name;
        }
        int idx = name.indexOf('.');
        if (idx < 0) {
            return name;
        }
        return name.substring(0, idx) + '_' + name.substring(idx + 1);
    }
}
