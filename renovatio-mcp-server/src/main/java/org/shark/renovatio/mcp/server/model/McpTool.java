package org.shark.renovatio.mcp.server.model;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * MCP Tool model representing a tool available through the MCP protocol
 */
public class McpTool {
    private String name;
    private String description;
    private Map<String, Object> inputSchema;
    private Map<String, Object> outputSchema;
    private List<Map<String, Object>> parameters;
    private Map<String, Object> example;
    private Map<String, Object> metadata;

    public McpTool() {
        this.metadata = new LinkedHashMap<>();
    }

    public McpTool(String name, String description, Map<String, Object> inputSchema, Map<String, Object> outputSchema,
                   List<Map<String, Object>> parameters, Map<String, Object> example) {
        this(name, description, inputSchema, outputSchema, parameters, example, null);
    }

    public McpTool(String name, String description, Map<String, Object> inputSchema, Map<String, Object> outputSchema,
                   List<Map<String, Object>> parameters, Map<String, Object> example, Map<String, Object> metadata) {
        this.name = name;
        this.description = description;
        this.inputSchema = inputSchema;
        this.outputSchema = outputSchema;
        this.parameters = parameters;
        this.example = example;
        this.metadata = metadata != null ? new LinkedHashMap<>(metadata) : new LinkedHashMap<>();
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

    public Map<String, Object> getOutputSchema() {
        return outputSchema;
    }

    public void setOutputSchema(Map<String, Object> outputSchema) {
        this.outputSchema = outputSchema;
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

    public Map<String, Object> getMetadata() {
        return metadata;
    }

    public void setMetadata(Map<String, Object> metadata) {
        this.metadata = metadata != null ? new LinkedHashMap<>(metadata) : new LinkedHashMap<>();
    }

    @Override
    public String toString() {
        return "McpTool{" +
                "name='" + name + '\'' +
                ", description='" + description + '\'' +
                ", inputSchema=" + inputSchema +
                ", outputSchema=" + outputSchema +
                ", parameters=" + parameters +
                ", example=" + example +
                ", metadata=" + metadata +
                '}';
    }
}
