package org.shark.renovatio.shared.domain;

import java.util.HashMap;
import java.util.Map;

/**
 * Basic implementation of the Tool interface for use in the core engine.
 * This provides a protocol-agnostic tool representation.
 */
public class BasicTool implements Tool {
    
    private final String name;
    private final String description;
    private final Map<String, Object> inputSchema;
    private final Map<String, Object> metadata;
    
    public BasicTool(String name, String description, Map<String, Object> inputSchema) {
        this.name = name;
        this.description = description;
        this.inputSchema = inputSchema != null ? inputSchema : new HashMap<>();
        this.metadata = new HashMap<>();
    }
    
    public BasicTool(String name, String description, Map<String, Object> inputSchema, Map<String, Object> metadata) {
        this.name = name;
        this.description = description;
        this.inputSchema = inputSchema != null ? inputSchema : new HashMap<>();
        this.metadata = metadata != null ? metadata : new HashMap<>();
    }
    
    @Override
    public String getName() {
        return name;
    }
    
    @Override
    public String getDescription() {
        return description;
    }
    
    @Override
    public Map<String, Object> getInputSchema() {
        return inputSchema;
    }
    
    @Override
    public Map<String, Object> getMetadata() {
        return metadata;
    }
    
    @Override
    public String toString() {
        return String.format("BasicTool{name='%s', description='%s'}", name, description);
    }
}