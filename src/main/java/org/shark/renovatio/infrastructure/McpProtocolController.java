package org.shark.renovatio.infrastructure;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.shark.renovatio.application.McpToolingService;
import org.shark.renovatio.domain.mcp.*;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

@RestController
@RequestMapping("/mcp-internal")
@Tag(name = "MCP Protocol")
public class McpProtocolController {

    @Autowired
    private McpToolingService mcpToolingService;

    @PostMapping("/mcp-internal")
    @Operation(summary = "MCP JSON-RPC 2.0 endpoint")
    public McpResponse handleMcpRequest(@RequestBody McpRequest request) {
        try {
            switch (request.getMethod()) {
                case "initialize":
                    return handleInitialize(request);
                case "ping":
                    return handlePing(request);
                case "restart":
                    return handleRestart(request);
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
        return new McpResponse(request.getId(), result);
    }

    private McpResponse handlePing(McpRequest request) {
        return new McpResponse(request.getId(), new HashMap<>());
    }

    private McpResponse handleRestart(McpRequest request) {
        Map<String, Object> result = new HashMap<>();
        result.put("message", "Server restart requested");
        return new McpResponse(request.getId(), result);
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

    private McpResponse handleCliManifest(McpRequest request) {
        Map<String, Object> result = new HashMap<>();
        result.put("commands", mcpToolingService.getMcpTools());
        return new McpResponse(request.getId(), result);
    }

    private McpResponse handleToolsCall(McpRequest request) {
        // Extract parameters from the request
        @SuppressWarnings("unchecked")
        Map<String, Object> params = (Map<String, Object>) request.getParams();
        String toolName = (String) params.get("name");
        @SuppressWarnings("unchecked")
        Map<String, Object> arguments = (Map<String, Object>) params.get("arguments");
        
        // Execute the tool
        var response = mcpToolingService.executeTool(toolName, arguments);
        
        Map<String, Object> result = new HashMap<>();
        result.put("content", response);
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
        Map<String, Object> content = new HashMap<>();
        content.put("uri", resource.getUri());
        content.put("mimeType", resource.getMimeType());
        content.put("text", resource.getText());

        Map<String, Object> result = new HashMap<>();
        result.put("contents", List.of(content));
        return new McpResponse(request.getId(), result);
    }

    private McpResponse handleLoggingSubscribe(McpRequest request) {
        return new McpResponse(request.getId(), new McpError(-32601, "logging/subscribe not implemented"));
    }

    private McpResponse handleLoggingUnsubscribe(McpRequest request) {
        return new McpResponse(request.getId(), new McpError(-32601, "logging/unsubscribe not implemented"));
    }

    private McpResponse handleToolsDescribe(McpRequest request) {
        return new McpResponse(request.getId(), new McpError(-32601, "tools/describe not implemented"));
    }

    private McpResponse handleCapabilities(McpRequest request) {
        Map<String, Object> result = new HashMap<>();
        result.put("capabilities", new McpCapabilities());
        return new McpResponse(request.getId(), result);
    }

    private McpResponse handleServerInfo(McpRequest request) {
        Map<String, Object> result = new HashMap<>();
        result.put("name", "Renovatio OpenRewrite MCP Server");
        result.put("version", "1.0.0");
        result.put("description", "Servidor MCP compatible con OpenRewrite y herramientas de refactorización.");
        return new McpResponse(request.getId(), result);
    }

    private McpResponse handleContentRead(McpRequest request) {
        return new McpResponse(request.getId(), new McpError(-32601, "content/read not implemented"));
    }

    private McpResponse handleContentWrite(McpRequest request) {
        return new McpResponse(request.getId(), new McpError(-32601, "content/write not implemented"));
    }

    private McpResponse handleWorkspaceList(McpRequest request) {
        return new McpResponse(request.getId(), new McpError(-32601, "workspace/list not implemented"));
    }

    private McpResponse handleWorkspaceDescribe(McpRequest request) {
        return new McpResponse(request.getId(), new McpError(-32601, "workspace/describe not implemented"));
    }
}