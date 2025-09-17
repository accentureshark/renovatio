package org.shark.renovatio.mcp.server.service;

import org.shark.renovatio.mcp.server.model.McpTool;
import org.shark.renovatio.shared.domain.Tool;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * Adapter that converts between protocol-agnostic Tool objects and MCP-specific McpTool objects.
 * This allows the core engine to remain protocol-agnostic while the MCP server can work with
 * MCP-specific objects.
 */
@Component
public class McpToolAdapter {
    
    /**
     * Convert a generic Tool to an MCP-specific McpTool
     */
    public McpTool toMcpTool(Tool tool) {
        McpTool mcpTool = new McpTool();

        // Convert tool names to MCP-compliant format (dots to underscores)
        String mcpToolName = tool.getName();
        if (mcpToolName.contains(".")) {
            mcpToolName = mcpToolName.replace(".", "_");
        }

        mcpTool.setName(mcpToolName);
        mcpTool.setDescription(tool.getDescription());

        Map<String, Object> schema = tool.getInputSchema();
        Map<String, Object> mcpSchema = null;
        if (isProviderTool(mcpToolName) && schema != null) {
            // Make a copy of the schema to avoid modifying the original
            mcpSchema = new HashMap<>(schema);
            Map<String, Object> properties = (Map<String, Object>) mcpSchema.get("properties");

            if (properties != null && !properties.containsKey("workspacePath")) {
                // Add workspacePath as required parameter
                Map<String, Object> workspacePathProperty = new HashMap<>();
                workspacePathProperty.put("type", "string");
                workspacePathProperty.put("description", "Path to the workspace directory to analyze");
                properties.put("workspacePath", workspacePathProperty);

                // Update required fields
                List<String> required = (List<String>) mcpSchema.get("required");
                if (required == null) {
                    required = new ArrayList<>();
                    mcpSchema.put("required", required);
                }
                if (!required.contains("workspacePath")) {
                    required.add("workspacePath");
                }
            }
            mcpTool.setInputSchema(mcpSchema);
        } else {
            mcpTool.setInputSchema(schema);
            mcpSchema = schema;
        }

        // MCP-compliant: extract parameters from inputSchema if present
        List<Map<String, Object>> parameters = new ArrayList<>();
        if (mcpSchema != null && mcpSchema.containsKey("properties")) {
            Map<String, Object> properties = (Map<String, Object>) mcpSchema.get("properties");
            List<String> required = (List<String>) mcpSchema.getOrDefault("required", new ArrayList<>());
            for (Map.Entry<String, Object> entry : properties.entrySet()) {
                String paramName = entry.getKey();
                Map<String, Object> prop = (Map<String, Object>) entry.getValue();
                Map<String, Object> param = new HashMap<>();
                param.put("name", paramName);
                param.put("type", prop.getOrDefault("type", "string"));
                param.put("description", prop.getOrDefault("description", ""));
                param.put("required", required.contains(paramName));
                parameters.add(param);
            }
        }
        mcpTool.setParameters(parameters);

        // MCP-compliant: set example if present in schema
        Map<String, Object> example = null;
        if (mcpSchema != null && mcpSchema.containsKey("example")) {
            Object ex = mcpSchema.get("example");
            if (ex instanceof Map) {
                example = (Map<String, Object>) ex;
            }
        }
        mcpTool.setExample(example);

        return mcpTool;
    }
    
    /**
     * Check if a tool is a provider-specific tool (java, cobol, etc.)
     */
    private boolean isProviderTool(String toolName) {
        return toolName.contains("_") &&
               !toolName.startsWith("nql_") &&
               !toolName.startsWith("common_");
    }

    /**
     * Convert a list of generic Tools to MCP-specific McpTools
     */
    public List<McpTool> toMcpTools(List<Tool> tools) {
        return tools.stream()
                .map(this::toMcpTool)
                .collect(Collectors.toList());
    }
    
    /**
     * Convert an MCP-specific McpTool to a generic Tool
     */
    public Tool fromMcpTool(McpTool mcpTool) {
        // Keep original tool name format with underscores
        String internalToolName = mcpTool.getName();
        // Removed automatic underscore to dot conversion

        return new org.shark.renovatio.shared.domain.BasicTool(
            internalToolName,
            mcpTool.getDescription(),
            mcpTool.getInputSchema()
        );
    }
    
    /**
     * Convert a list of MCP-specific McpTools to generic Tools
     */
    public List<Tool> fromMcpTools(List<McpTool> mcpTools) {
        return mcpTools.stream()
                .map(this::fromMcpTool)
                .collect(Collectors.toList());
    }
}