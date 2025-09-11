package org.shark.renovatio.core.application;

import org.shark.renovatio.core.mcp.*;
import org.springframework.stereotype.Service;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;

@Service
public class McpService {
    private final McpToolingService mcpToolingService;
    private final Set<String> loggingSubscribers = ConcurrentHashMap.newKeySet();

    public McpService(McpToolingService mcpToolingService) {
        this.mcpToolingService = mcpToolingService;
    }

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

    private McpResponse handleInitialize(McpRequest request) {
        Map<String, Object> result = new HashMap<>();
        result.put("protocolVersion", "2025-06-18");
        result.put("capabilities", new McpCapabilities());
        result.put("serverInfo", Map.of(
            "name", "Renovatio OpenRewrite MCP Server",
            "version", "1.0.0"
        ));
        var tools = mcpToolingService.getMcpTools();
        System.out.println("DEBUG tools before put: " + tools);
        result.put("availableTools", tools);
        System.out.println("DEBUG result after put: " + result);
        return new McpResponse(request.getId(), result);
    }

    private McpResponse handlePing(McpRequest request) {
        return new McpResponse(request.getId(), new HashMap<>());
    }

    private McpResponse handleShutdown(McpRequest request) {
        Map<String, Object> result = new HashMap<>();
        result.put("message", "Server shutdown requested");
        return new McpResponse(request.getId(), result);
    }

    private McpResponse handleToolsList(McpRequest request) {
        Map<String, Object> result = new HashMap<>();
        result.put("tools", mcpToolingService.getMcpTools());
        return new McpResponse(request.getId(), result);
    }

    private McpResponse handleToolsCall(McpRequest request) {
        @SuppressWarnings("unchecked")
        Map<String, Object> params = (Map<String, Object>) request.getParams();
        String toolName = (String) params.get("name");
        @SuppressWarnings("unchecked")
        Map<String, Object> arguments = (Map<String, Object>) params.get("arguments");

        Map<String, Object> rawResponse = mcpToolingService.executeTool(toolName, arguments);
        Map<String, Object> response = new HashMap<>();
        Object type = rawResponse.get("type");
        Object text = rawResponse.get("text");
        response.put("type", type != null ? type : "text");
        response.put("text", text != null ? text.toString() : rawResponse.toString());

        Map<String, Object> result = new HashMap<>();
        result.put("content", List.of(response));
        return new McpResponse(request.getId(), result);
    }

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
        return new McpResponse(request.getId(), result);
    }

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

    private McpResponse handleCapabilities(McpRequest request) {
        Map<String, Object> result = new HashMap<>();
        result.put("capabilities", new McpCapabilities());
        return new McpResponse(request.getId(), result);
    }

    private McpResponse handleServerInfo(McpRequest request) {
        Map<String, Object> result = new HashMap<>();
        result.put("serverInfo", Map.of(
            "name", "Renovatio MCP Server",
            "version", "1.0.0"
        ));
        return new McpResponse(request.getId(), result);
    }

    private McpResponse handleContentRead(McpRequest request) {
        // Renovatio: Read file content from workspace
        Map<String, Object> params = (Map<String, Object>) request.getParams();
        String path = params != null ? (String) params.get("path") : null;
        Map<String, Object> result = new HashMap<>();
        if (path == null || path.isEmpty()) {
            return new McpResponse(request.getId(), new McpError(-32602, "Missing or empty 'path' parameter"));
        }
        try {
            String content = mcpToolingService.readFileContent(path);
            result.put("content", content);
            return new McpResponse(request.getId(), result);
        } catch (Exception e) {
            return new McpResponse(request.getId(), new McpError(-32603, "Error reading file: " + e.getMessage()));
        }
    }

    private McpResponse handleContentWrite(McpRequest request) {
        // Renovatio: Write file content to workspace
        Map<String, Object> params = (Map<String, Object>) request.getParams();
        String path = params != null ? (String) params.get("path") : null;
        String content = params != null ? (String) params.get("content") : null;
        Map<String, Object> result = new HashMap<>();
        if (path == null || path.isEmpty() || content == null) {
            return new McpResponse(request.getId(), new McpError(-32602, "Missing 'path' or 'content' parameter"));
        }
        try {
            mcpToolingService.writeFileContent(path, content);
            result.put("message", "Content written successfully");
            return new McpResponse(request.getId(), result);
        } catch (Exception e) {
            return new McpResponse(request.getId(), new McpError(-32603, "Error writing file: " + e.getMessage()));
        }
    }

    private McpResponse handleWorkspaceList(McpRequest request) {
        // Renovatio: List workspace root directories/files
        Map<String, Object> result = new HashMap<>();
        try {
            var workspaces = mcpToolingService.listWorkspaces();
            result.put("workspaces", workspaces);
            return new McpResponse(request.getId(), result);
        } catch (Exception e) {
            return new McpResponse(request.getId(), new McpError(-32603, "Error listing workspaces: " + e.getMessage()));
        }
    }

    private McpResponse handleWorkspaceDescribe(McpRequest request) {
        // Renovatio: Describe workspace (metadata, structure)
        Map<String, Object> params = (Map<String, Object>) request.getParams();
        String workspaceId = params != null ? (String) params.get("id") : null;
        Map<String, Object> result = new HashMap<>();
        if (workspaceId == null || workspaceId.isEmpty()) {
            return new McpResponse(request.getId(), new McpError(-32602, "Missing or empty 'id' parameter"));
        }
        try {
            var workspace = mcpToolingService.describeWorkspace(workspaceId);
            result.put("workspace", workspace);
            return new McpResponse(request.getId(), result);
        } catch (Exception e) {
            return new McpResponse(request.getId(), new McpError(-32603, "Error describing workspace: " + e.getMessage()));
        }
    }

    private McpResponse handleToolsDescribe(McpRequest request) {
        @SuppressWarnings("unchecked")
        Map<String, Object> params = (Map<String, Object>) request.getParams();
        String toolName = (String) params.get("name");
        var tool = mcpToolingService.getTool(toolName);
        if (tool == null) {
            return new McpResponse(request.getId(), new McpError(-32602, "Tool not found: " + toolName));
        }
        Map<String, Object> result = new HashMap<>();
        result.put("tool", tool);
        return new McpResponse(request.getId(), result);
    }
}
