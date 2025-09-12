package org.shark.renovatio.mcp.server;

import org.shark.renovatio.mcp.server.transport.McpStdioServer;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.context.annotation.ComponentScan;

/**
 * Main Spring Boot application for the Renovatio MCP Server.
 * 
 * This server implements the Model Content Protocol (MCP) specification
 * and exposes Renovatio's refactoring and migration capabilities as MCP tools.
 */
@SpringBootApplication
@ComponentScan(basePackages = {
    "org.shark.renovatio.mcp.server",
    "org.shark.renovatio.core.service",
    "org.shark.renovatio.shared"
})
public class McpServerApplication {

    public static void main(String[] args) {
        // Check if running in stdio mode
        boolean stdioMode = isStdioMode(args);

        if (stdioMode) {
            // Disable web server for stdio mode
            System.setProperty("spring.main.web-application-type", "none");
            System.setProperty("spring.main.banner-mode", "off");
            System.setProperty("logging.level.root", "ERROR");

            System.err.println("[Renovatio MCP] Starting MCP stdio server...");

            ConfigurableApplicationContext context = SpringApplication.run(McpServerApplication.class, args);

            try {
                McpStdioServer mcpServer = context.getBean(McpStdioServer.class);
                // Start the stdio server and wait
                mcpServer.start().get();
            } catch (Exception e) {
                System.err.println("[Renovatio MCP] Error starting MCP stdio server: " + e.getMessage());
                e.printStackTrace(System.err);
            } finally {
                context.close();
            }
        } else {
            // Normal HTTP/REST mode
            SpringApplication.run(McpServerApplication.class, args);
        }
    }

    private static boolean isStdioMode(String[] args) {
        // Check if stdio mode is requested via system property or argument
        if ("none".equals(System.getProperty("spring.main.web-application-type"))) {
            return true;
        }

        for (String arg : args) {
            if (arg.contains("web-application-type=none") || arg.equals("--stdio")) {
                return true;
            }
        }

        return false;
    }
}