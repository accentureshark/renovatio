package org.shark.renovatio.provider.jcl.domain;

import java.util.Map;

/**
 * Simple MCP Tool representation for the JCL provider.
 */
public class JclMcpTool {
    private String name;
    private String description;
    private Map<String, Object> inputSchema;

    public JclMcpTool() {
    }

    public JclMcpTool(String name, String description) {
        this.name = name;
        this.description = description;
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
}
