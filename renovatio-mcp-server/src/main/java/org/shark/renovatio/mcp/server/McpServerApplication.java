package org.shark.renovatio.mcp.server;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
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
        SpringApplication.run(McpServerApplication.class, args);
    }
}