package org.shark.renovatio.mcp.server.model;

import java.util.Map;

/**
 * MCP Tool model representing a tool available through the MCP protocol
 */
public class McpTool {
    private String name;
    private String description;
    private Map<String, Object> inputSchema;

    public McpTool() {}

    public McpTool(String name, String description, Map<String, Object> inputSchema) {
        this.name = name;
        this.description = description;
        this.inputSchema = inputSchema;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public Map<String, Object> getInputSchema() {
        return inputSchema;
    }

    public void setInputSchema(Map<String, Object> inputSchema) {
        this.inputSchema = inputSchema;
    }

    @Override
    public String toString() {
        return "McpTool{" +
                "name='" + name + '\'' +
                ", description='" + description + '\'' +
                '}';
    }
}
