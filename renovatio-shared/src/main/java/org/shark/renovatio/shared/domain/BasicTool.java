package org.shark.renovatio.shared.domain;

import java.util.HashMap;
import java.util.Map;

/**
 * Basic implementation of the Tool interface for internal use.
 */
public class BasicTool implements Tool {

    private String name;
    private String description;
    private Map<String, Object> inputSchema;
    private Map<String, Object> metadata;

    public BasicTool() {
        this.inputSchema = new HashMap<>();
        this.metadata = new HashMap<>();
    }

    public BasicTool(String name, String description) {
        this();
        this.name = name;
        this.description = description;
    }

    public BasicTool(String name, String description, Map<String, Object> inputSchema) {
        this();
        this.name = name;
        this.description = description;
        this.inputSchema = inputSchema != null ? inputSchema : new HashMap<>();
    }

    @Override
    public String getName() {
        return name;
    }

    // Setters for internal use
    public void setName(String name) {
        this.name = name;
    }

    @Override
    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    @Override
    public Map<String, Object> getInputSchema() {
        return inputSchema;
    }

    public void setInputSchema(Map<String, Object> inputSchema) {
        this.inputSchema = inputSchema != null ? inputSchema : new HashMap<>();
    }

    @Override
    public Map<String, Object> getMetadata() {
        return metadata;
    }

    public void setMetadata(Map<String, Object> metadata) {
        this.metadata = metadata != null ? metadata : new HashMap<>();
    }
}
