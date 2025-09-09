package org.shark.renovatio.core.mcp;

import io.swagger.v3.oas.annotations.media.Schema;
import java.util.Map;

@Schema(description = "MCP Tool definition")
public class McpTool {
    @Schema(description = "Tool name")
    private String name;
    
    @Schema(description = "Tool description")
    private String description;
    
    @Schema(description = "Input schema definition")
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
}