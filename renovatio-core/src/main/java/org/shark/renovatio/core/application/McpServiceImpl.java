package org.shark.renovatio.core.application;

import org.shark.renovatio.core.mcp.McpRequest;
import org.shark.renovatio.core.mcp.McpResponse;
import org.springframework.stereotype.Service;

@Service
public class McpServiceImpl implements McpService {
    @Override
    public McpResponse handleMcpRequest(McpRequest request) {
        // Minimal stub implementation
        McpResponse response = new McpResponse();
        response.setResult("MCP stub response");
        return response;
    }
}

