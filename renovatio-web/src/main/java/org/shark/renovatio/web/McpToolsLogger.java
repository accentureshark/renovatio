package org.shark.renovatio.web;

import jakarta.annotation.PostConstruct;
import org.shark.renovatio.core.application.McpService;
import org.shark.renovatio.core.mcp.McpRequest;
import org.shark.renovatio.core.mcp.McpResponse;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.Map;

@Component
public class McpToolsLogger {
    private static final Logger logger = LoggerFactory.getLogger(McpToolsLogger.class);
    private final McpService mcpService;

    public McpToolsLogger(McpService mcpService) {
        this.mcpService = mcpService;
    }

    @PostConstruct
    public void logAvailableMcpTools() {
        McpRequest request = new McpRequest("startup", "tools/list", null);
        McpResponse response = mcpService.handleMcpRequest(request);
        Object toolsObj = null;
        if (response.getResult() instanceof Map) {
            toolsObj = ((Map<?, ?>) response.getResult()).get("tools");
        }
        if (toolsObj instanceof List) {
            List<?> tools = (List<?>) toolsObj;
            logger.info("MCP Tools disponibles al iniciar el servidor:");
            for (Object tool : tools) {
                if (tool instanceof Map) {
                    String name = String.valueOf(((Map<?, ?>) tool).get("name"));
                    String desc = String.valueOf(((Map<?, ?>) tool).get("description"));
                    logger.info("- {}: {}", name, desc);
                }
            }
        } else {
            logger.warn("No se pudo obtener la lista de MCP Tools disponibles.");
        }
    }
}
