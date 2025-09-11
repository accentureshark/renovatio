package org.shark.renovatio.web;

import org.shark.renovatio.core.application.McpToolingService;
import org.springframework.stereotype.Component;
import org.springframework.context.event.EventListener;
import org.springframework.boot.context.event.ApplicationReadyEvent;

/**
 * Logs all available MCP tools and providers when the Renovatio Web server starts.
 */
@Component
public class McpToolsStartupLogger {
    private final McpToolingService mcpToolingService;

    public McpToolsStartupLogger(McpToolingService mcpToolingService) {
        this.mcpToolingService = mcpToolingService;
    }

    @EventListener(ApplicationReadyEvent.class)
    public void logProvidersAndToolsOnStartup() {
        mcpToolingService.logProvidersAndTools();
    }
}
