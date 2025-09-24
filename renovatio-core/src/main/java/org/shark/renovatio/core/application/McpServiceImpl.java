package org.shark.renovatio.core.application;

import org.shark.renovatio.core.mcp.McpRequest;
import org.shark.renovatio.core.mcp.McpResponse;
import org.springframework.stereotype.Service;

@Service
public class McpServiceImpl implements McpService {
    @Override
    public McpResponse handleMcpRequest(McpRequest request) {
        McpResponse response = new McpResponse();
        response.setJsonrpc("2.0");
        response.setId(request.getId());
        if ("initialize".equalsIgnoreCase(request.getMethod())) {
            // MCP-compliant result
            java.util.Map<String, Object> capabilities = new java.util.HashMap<>();
            capabilities.put("tools", java.util.Map.of("listChanged", true));
            capabilities.put("resources", java.util.Map.of("listChanged", true));
            capabilities.put("prompts", java.util.Map.of());
            capabilities.put("logging", java.util.Map.of());
            capabilities.put("progress", true);

            java.util.Map<String, Object> serverInfo = java.util.Map.of(
                "name", "Renovatio MCP Server",
                "version", "1.0.0"
            );

            java.util.Map<String, Object> result = new java.util.HashMap<>();
            result.put("protocolVersion", "2024-11-05");
            result.put("serverInfo", serverInfo);
            result.put("capabilities", capabilities);
            response.setResult(result);
        } else {
            // Standard MCP error for unknown method
            java.util.Map<String, Object> error = new java.util.HashMap<>();
            error.put("code", -32601);
            error.put("message", "Method not found: " + request.getMethod());
            response.setError(error);
        }
        return response;
    }
}
