package org.shark.renovatio.mcp.server.service;

import org.shark.renovatio.mcp.server.model.*;
import org.springframework.stereotype.Service;

import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;

/**
 * MCP Protocol Service - implements the full Model Content Protocol specification.
 * 
 * This service handles all MCP JSON-RPC 2.0 methods according to the specification at:
 * https://modelcontextprotocol.io/docs/develop/build-server
 */
@Service
public class McpProtocolService {
    
    private final McpToolingService mcpToolingService;
    private final Set<String> loggingSubscribers = ConcurrentHashMap.newKeySet();

    public McpProtocolService(McpToolingService mcpToolingService) {
        this.mcpToolingService = mcpToolingService;
    }

    /**
     * Main entry point for handling MCP requests.
     */
    public McpResponse handleMcpRequest(McpRequest request) {
        try {
            switch (request.getMethod()) {
                case "initialize":
                    return handleInitialize(request);
                case "ping":
                    return handlePing(request);
                case "shutdown":
                    return handleShutdown(request);
                case "tools/list":
                    return handleToolsList(request);
                case "tools/call":
                    return handleToolsCall(request);
                case "tools/describe":
                    return handleToolsDescribe(request);
                case "capabilities":
                    return handleCapabilities(request);
                case "server/info":
                    return handleServerInfo(request);
                case "content/read":
                    return handleContentRead(request);
                case "content/write":
                    return handleContentWrite(request);
                case "workspace/list":
                    return handleWorkspaceList(request);
                case "workspace/describe":
                    return handleWorkspaceDescribe(request);
                case "logging/subscribe":
                    return handleLoggingSubscribe(request);
                case "logging/unsubscribe":
                    return handleLoggingUnsubscribe(request);
                case "prompts/list":
                    return handlePromptsList(request);
                case "prompts/get":
                    return handlePromptsGet(request);
                case "resources/list":
                    return handleResourcesList(request);
                case "resources/read":
                    return handleResourcesRead(request);
                case "cli/manifest":
                    return handleCliManifest(request);
                default:
                    return new McpResponse(request.getId(),
                        new McpError(-32601, "Method not found: " + request.getMethod()));
            }
        } catch (Exception e) {
            return new McpResponse(request.getId(),
                new McpError(-32603, "Internal error: " + e.getMessage()));
        }
    }

    /**
     * MCP initialize method - establishes protocol version and capabilities.
     */
    private McpResponse handleInitialize(McpRequest request) {
        Map<String, Object> result = new HashMap<>();
        result.put("protocolVersion", "2025-06-18");
        result.put("capabilities", new McpCapabilities());
        result.put("serverInfo", Map.of(
            "name", "Renovatio MCP Server",
            "version", "1.0.0",
            "description", "Multi-language refactoring and migration platform with full MCP compliance"
        ));
        
        // Include available tools in initialization
        var tools = mcpToolingService.getMcpTools();
        result.put("availableTools", tools);
        
        return new McpResponse(request.getId(), result);
    }

    /**
     * MCP ping method - simple connectivity test.
     */
    private McpResponse handlePing(McpRequest request) {
        Map<String, Object> result = new HashMap<>();
        result.put("status", "pong");
        result.put("timestamp", System.currentTimeMillis());
        return new McpResponse(request.getId(), result);
    }

    /**
     * MCP shutdown method - graceful server shutdown.
     */
    private McpResponse handleShutdown(McpRequest request) {
        Map<String, Object> result = new HashMap<>();
        result.put("message", "Server shutdown requested");
        result.put("gracefulShutdown", true);
        return new McpResponse(request.getId(), result);
    }

    /**
     * MCP tools/list method - returns all available tools.
     */
    private McpResponse handleToolsList(McpRequest request) {
        Map<String, Object> result = new HashMap<>();
        result.put("tools", mcpToolingService.getMcpTools());
        return new McpResponse(request.getId(), result);
    }

    /**
     * MCP tools/call method - executes a specific tool with parameters.
     */
    private McpResponse handleToolsCall(McpRequest request) {
        try {
            @SuppressWarnings("unchecked")
            Map<String, Object> params = (Map<String, Object>) request.getParams();
            if (params == null || !params.containsKey("name")) {
                return new McpResponse(request.getId(),
                    new McpError(-32602, "Invalid params - missing tool name"));
            }

            String toolName = normalizeToolName((String) params.get("name"));
            @SuppressWarnings("unchecked")
            Map<String, Object> arguments = (Map<String, Object>) params.getOrDefault("arguments", new HashMap<>());

            ToolCallResult toolResult = mcpToolingService.executeToolWithEnvelope(toolName, arguments);
            return new McpResponse(request.getId(), toolResult);
        } catch (Exception e) {
            return new McpResponse(request.getId(),
                new McpError(-32603, "Tool execution error: " + e.getMessage()));
        }
    }

    private String normalizeToolName(String name) {
        if (name == null) {
            return null;
        }
        int dotIndex = name.indexOf('.');
        if (dotIndex < 0) {
            return name != null ? name.toLowerCase(Locale.ROOT) : null;
        }

        int underscoreIndex = name.indexOf('_');
        if (underscoreIndex >= 0 && underscoreIndex < dotIndex) {
            return name.toLowerCase(Locale.ROOT);
        }

        String normalized = name.substring(0, dotIndex) + '_' + name.substring(dotIndex + 1);
        return normalized.toLowerCase(Locale.ROOT);
    }

    /**
     * Helper method for stdio server to call tools.
     */
    public Map<String, Object> callToolAction(String toolName, String action, com.fasterxml.jackson.databind.JsonNode params) {
        try {
            Map<String, Object> arguments = new HashMap<>();
            if (params != null && params.isObject()) {
                arguments = new com.fasterxml.jackson.databind.ObjectMapper().convertValue(params, Map.class);
            }

            // For MCP compatibility, combine tool name and action
            String fullToolName = toolName;
            if (action != null && !action.isEmpty()) {
                fullToolName = toolName + "_" + action;
            }

            return mcpToolingService.executeTool(fullToolName, arguments);
        } catch (Exception e) {
            Map<String, Object> errorResult = new HashMap<>();
            errorResult.put("success", false);
            errorResult.put("error", e.getMessage());
            return errorResult;
        }
    }

    /**
     * MCP tools/describe method - provides detailed information about a specific tool.
     */
    private McpResponse handleToolsDescribe(McpRequest request) {
        @SuppressWarnings("unchecked")
        Map<String, Object> params = (Map<String, Object>) request.getParams();
        String toolName = normalizeToolName((String) params.get("name"));
        var tool = mcpToolingService.getTool(toolName);
        if (tool == null) {
            return new McpResponse(request.getId(), new McpError(-32602, "Tool not found: " + toolName));
        }
        Map<String, Object> result = new HashMap<>();
        result.put("tool", tool);
        return new McpResponse(request.getId(), result);
    }

    /**
     * MCP capabilities method - returns server capabilities.
     */
    private McpResponse handleCapabilities(McpRequest request) {
        Map<String, Object> result = new HashMap<>();
        result.put("capabilities", new McpCapabilities());
        return new McpResponse(request.getId(), result);
    }

    /**
     * MCP server/info method - returns server information.
     */
    private McpResponse handleServerInfo(McpRequest request) {
        Map<String, Object> serverInfo = new HashMap<>();
        serverInfo.put("name", "Renovatio MCP Server");
        serverInfo.put("version", "1.0.0");
        serverInfo.put("description", "Multi-language refactoring and migration platform");
        serverInfo.put("protocolVersion", "2025-06-18");
        serverInfo.put("supportedLanguages", mcpToolingService.getSupportedLanguages());

        Map<String, Object> result = new HashMap<>();
        result.put("serverInfo", serverInfo);
        return new McpResponse(request.getId(), result);
    }

    // Content handling methods
    private McpResponse handleContentRead(McpRequest request) {
        Map<String, Object> params = (Map<String, Object>) request.getParams();
        String path = params != null ? (String) params.get("path") : null;
        if (path == null || path.isEmpty()) {
            return new McpResponse(request.getId(), new McpError(-32602, "Missing or empty 'path' parameter"));
        }
        try {
            String content = mcpToolingService.readFileContent(path);
            Map<String, Object> result = new HashMap<>();
            result.put("content", content);
            return new McpResponse(request.getId(), result);
        } catch (Exception e) {
            return new McpResponse(request.getId(), new McpError(-32603, "Error reading file: " + e.getMessage()));
        }
    }

    private McpResponse handleContentWrite(McpRequest request) {
        Map<String, Object> params = (Map<String, Object>) request.getParams();
        String path = params != null ? (String) params.get("path") : null;
        String content = params != null ? (String) params.get("content") : null;
        if (path == null || path.isEmpty() || content == null) {
            return new McpResponse(request.getId(), new McpError(-32602, "Missing 'path' or 'content' parameter"));
        }
        try {
            mcpToolingService.writeFileContent(path, content);
            Map<String, Object> result = new HashMap<>();
            result.put("message", "Content written successfully");
            return new McpResponse(request.getId(), result);
        } catch (Exception e) {
            return new McpResponse(request.getId(), new McpError(-32603, "Error writing file: " + e.getMessage()));
        }
    }

    // Workspace handling methods
    private McpResponse handleWorkspaceList(McpRequest request) {
        try {
            var workspaces = mcpToolingService.listWorkspaces();
            Map<String, Object> result = new HashMap<>();
            result.put("workspaces", workspaces);
            return new McpResponse(request.getId(), result);
        } catch (Exception e) {
            return new McpResponse(request.getId(), new McpError(-32603, "Error listing workspaces: " + e.getMessage()));
        }
    }

    private McpResponse handleWorkspaceDescribe(McpRequest request) {
        Map<String, Object> params = (Map<String, Object>) request.getParams();
        String workspaceId = params != null ? (String) params.get("id") : null;
        if (workspaceId == null || workspaceId.isEmpty()) {
            return new McpResponse(request.getId(), new McpError(-32602, "Missing or empty 'id' parameter"));
        }
        try {
            var workspace = mcpToolingService.describeWorkspace(workspaceId);
            Map<String, Object> result = new HashMap<>();
            result.put("workspace", workspace);
            return new McpResponse(request.getId(), result);
        } catch (Exception e) {
            return new McpResponse(request.getId(), new McpError(-32603, "Error describing workspace: " + e.getMessage()));
        }
    }

    // Logging subscription methods
    private McpResponse handleLoggingSubscribe(McpRequest request) {
        @SuppressWarnings("unchecked")
        Map<String, Object> params = (Map<String, Object>) request.getParams();
        String clientId = params != null ? (String) params.get("id") : null;
        if (clientId == null || clientId.isEmpty()) {
            return new McpResponse(request.getId(), new McpError(-32602, "Missing or empty 'id' parameter"));
        }
        loggingSubscribers.add(clientId);
        Map<String, Object> result = new HashMap<>();
        result.put("subscribed", true);
        return new McpResponse(request.getId(), result);
    }

    private McpResponse handleLoggingUnsubscribe(McpRequest request) {
        @SuppressWarnings("unchecked")
        Map<String, Object> params = (Map<String, Object>) request.getParams();
        String clientId = params != null ? (String) params.get("id") : null;
        if (clientId == null || clientId.isEmpty()) {
            return new McpResponse(request.getId(), new McpError(-32602, "Missing or empty 'id' parameter"));
        }
        boolean removed = loggingSubscribers.remove(clientId);
        Map<String, Object> result = new HashMap<>();
        result.put("unsubscribed", removed);
        return new McpResponse(request.getId(), result);
    }

    // Prompts and resources handling
    private McpResponse handlePromptsList(McpRequest request) {
        Map<String, Object> result = new HashMap<>();
        result.put("prompts", mcpToolingService.getPrompts());
        return new McpResponse(request.getId(), result);
    }

    private McpResponse handlePromptsGet(McpRequest request) {
        @SuppressWarnings("unchecked")
        Map<String, Object> params = (Map<String, Object>) request.getParams();
        String name = (String) params.get("name");
        var prompt = mcpToolingService.getPrompt(name);
        if (prompt == null) {
            return new McpResponse(request.getId(), new McpError(-32602, "Prompt not found: " + name));
        }
        Map<String, Object> result = new HashMap<>();
        result.put("prompt", prompt);
        return new McpResponse(request.getId(), result);
    }

    private McpResponse handleResourcesList(McpRequest request) {
        Map<String, Object> result = new HashMap<>();
        result.put("resources", mcpToolingService.getResources());
        return new McpResponse(request.getId(), result);
    }

    private McpResponse handleResourcesRead(McpRequest request) {
        @SuppressWarnings("unchecked")
        Map<String, Object> params = (Map<String, Object>) request.getParams();
        String uri = (String) params.get("uri");
        var resource = mcpToolingService.getResource(uri);
        if (resource == null) {
            return new McpResponse(request.getId(), new McpError(-32602, "Resource not found: " + uri));
        }
        Map<String, Object> result = new HashMap<>();
        result.put("resource", resource);
        return new McpResponse(request.getId(), result);
    }

    /**
     * CLI manifest method - provides tool information for CLI clients.
     */
    private McpResponse handleCliManifest(McpRequest request) {
        List<McpTool> tools = mcpToolingService.getMcpTools();
        List<Map<String, Object>> commands = tools.stream().map(tool -> {
            Map<String, Object> command = new HashMap<>();
            command.put("name", tool.getName());
            command.put("description", tool.getDescription());
            command.put("inputSchema", tool.getInputSchema());
            return command;
        }).collect(Collectors.toList());

        Map<String, Object> result = new HashMap<>();
        result.put("commands", commands);
        result.put("serverInfo", Map.of(
            "name", "Renovatio MCP Server",
            "version", "1.0.0"
        ));
        return new McpResponse(request.getId(), result);
    }
}
