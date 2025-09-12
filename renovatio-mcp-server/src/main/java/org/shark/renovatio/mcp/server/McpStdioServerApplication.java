package org.shark.renovatio.mcp.server;

import org.shark.renovatio.mcp.server.transport.McpStdioServer;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.ConfigurableApplicationContext;

/**
 * MCP Server application with stdio transport for direct MCP client communication.
 * This is the main entry point for MCP clients like VS Code extensions.
 */
@SpringBootApplication(scanBasePackages = {
    "org.shark.renovatio.mcp.server",
    "org.shark.renovatio.core"
})
public class McpStdioServerApplication {

    public static void main(String[] args) {
        // Disable web server for stdio mode
        System.setProperty("spring.main.web-application-type", "none");
        
        System.err.println("[Renovatio MCP] Starting MCP stdio server...");
        
        ConfigurableApplicationContext context = SpringApplication.run(McpStdioServerApplication.class, args);
        
        try {
            McpStdioServer mcpServer = context.getBean(McpStdioServer.class);
            
            // Start the stdio server and wait
            mcpServer.start().get();
            
        } catch (Exception e) {
            System.err.println("[Renovatio MCP] Error starting MCP server: " + e.getMessage());
            e.printStackTrace();
        } finally {
            context.close();
        }
    }
}
