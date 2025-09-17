package org.shark.renovatio.mcp.server.model;

import java.util.List;
import java.util.Map;

/**
 * MCP Tool model representing a tool available through the MCP protocol
 */
public class McpTool {
    private String name;
    private String description;
    private Map<String, Object> inputSchema;
    private List<Map<String, Object>> parameters;
    private Map<String, Object> example;

    public McpTool() {}

    public McpTool(String name, String description, Map<String, Object> inputSchema, List<Map<String, Object>> parameters, Map<String, Object> example) {
        this.name = name;
        this.description = description;
        this.inputSchema = inputSchema;
        this.parameters = parameters;
        this.example = example;
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

    public List<Map<String, Object>> getParameters() {
        return parameters;
    }

    public void setParameters(List<Map<String, Object>> parameters) {
        this.parameters = parameters;
    }

    public Map<String, Object> getExample() {
        return example;
    }

    public void setExample(Map<String, Object> example) {
        this.example = example;
    }

    @Override
    public String toString() {
        return "McpTool{" +
                "name='" + name + '\'' +
                ", description='" + description + '\'' +
                ", inputSchema=" + inputSchema +
                ", parameters=" + parameters +
                ", example=" + example +
                '}';
    }
}
