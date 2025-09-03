package org.shark.renovatio.infrastructure;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.shark.renovatio.application.McpToolingService;
import org.shark.renovatio.domain.mcp.*;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import java.util.HashMap;
import java.util.Map;

@RestController
@RequestMapping("/")
@Tag(name = "MCP Protocol")
public class McpProtocolController {

    @Autowired
    private McpToolingService mcpToolingService;

    @PostMapping("/")
    @Operation(summary = "MCP JSON-RPC 2.0 endpoint")
    public McpResponse handleMcpRequest(@RequestBody McpRequest request) {
        try {
            switch (request.getMethod()) {
                case "initialize":
                    return handleInitialize(request);
                case "tools/list":
                    return handleToolsList(request);
                case "tools/call":
                    return handleToolsCall(request);
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
        result.put("protocolVersion", "2024-11-05");
        result.put("capabilities", new McpCapabilities());
        result.put("serverInfo", Map.of(
            "name", "Renovatio OpenRewrite MCP Server",
            "version", "1.0.0"
        ));
        return new McpResponse(request.getId(), result);
    }

    private McpResponse handleToolsList(McpRequest request) {
        Map<String, Object> result = new HashMap<>();
        result.put("tools", mcpToolingService.getMcpTools());
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
}