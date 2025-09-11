package org.shark.renovatio.core.application;

import org.shark.renovatio.core.mcp.McpRequest;
import org.shark.renovatio.core.mcp.McpResponse;

public interface McpService {
    McpResponse handleMcpRequest(McpRequest request);
}

