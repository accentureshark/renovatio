package org.shark.renovatio.mcp.server.service;

import org.shark.renovatio.mcp.server.model.McpTool;
import org.shark.renovatio.shared.domain.Tool;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
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

        String originalToolName = tool.getName();
        String mcpToolName = originalToolName;
        if (mcpToolName != null && !mcpToolName.contains("_") && mcpToolName.contains(".")) {
            mcpToolName = mcpToolName.replace('.', '_');
        }

        mcpTool.setName(mcpToolName);
        mcpTool.setDescription(tool.getDescription());

        Map<String, Object> schema = tool.getInputSchema();
        Map<String, Object> mcpSchema;
        if (isProviderTool(mcpToolName) && schema != null) {
            mcpSchema = new HashMap<>(schema);

            if (schema.get("properties") instanceof Map<?, ?> props) {
                Map<String, Object> properties = new LinkedHashMap<>();
                props.forEach((key, value) -> properties.put(String.valueOf(key), value));
                if (!properties.containsKey("workspacePath")) {
                    Map<String, Object> workspacePathProperty = new HashMap<>();
                    workspacePathProperty.put("type", "string");
                    workspacePathProperty.put("description", "Path to the workspace directory to analyze");
                    properties.put("workspacePath", workspacePathProperty);

                    List<String> requiredFromSchema = new ArrayList<>();
                    if (schema.get("required") instanceof List<?> list) {
                        for (Object entry : list) {
                            if (entry != null) {
                                requiredFromSchema.add(String.valueOf(entry));
                            }
                        }
                    }
                    if (!requiredFromSchema.contains("workspacePath")) {
                        requiredFromSchema.add("workspacePath");
                    }
                    mcpSchema.put("required", requiredFromSchema);
                }
                mcpSchema.put("properties", properties);
            }
            mcpTool.setInputSchema(mcpSchema);
        } else {
            mcpTool.setInputSchema(schema);
            mcpSchema = schema;
        }

        // MCP-compliant: extract parameters from inputSchema if present
        List<Map<String, Object>> parameters = new ArrayList<>();
        if (mcpSchema != null && mcpSchema.containsKey("properties")) {
            Object propsObj = mcpSchema.get("properties");
            Object requiredObj = mcpSchema.getOrDefault("required", new ArrayList<>());
            if (propsObj instanceof Map<?, ?> && requiredObj instanceof List<?>) {
                @SuppressWarnings("unchecked")
                Map<String, Object> properties = (Map<String, Object>) propsObj;
                @SuppressWarnings("unchecked")
                List<String> required = (List<String>) requiredObj;
                for (Map.Entry<String, Object> entry : properties.entrySet()) {
                    String paramName = entry.getKey();
                    Object propObj = entry.getValue();
                    if (propObj instanceof Map<?, ?>) {
                        @SuppressWarnings("unchecked")
                        Map<String, Object> prop = (Map<String, Object>) propObj;
                        Map<String, Object> param = new HashMap<>();
                        param.put("name", paramName);
                        param.put("type", prop.getOrDefault("type", "string"));
                        param.put("description", prop.getOrDefault("description", ""));
                        param.put("required", required.contains(paramName));
                        parameters.add(param);
                    }
                }
            }
        }
        mcpTool.setParameters(parameters);

        // MCP-compliant: set example if present in schema
        Map<String, Object> example = null;
        if (mcpSchema != null && mcpSchema.containsKey("example")) {
            Object ex = mcpSchema.get("example");
            if (ex instanceof Map<?, ?>) {
                @SuppressWarnings("unchecked")
                Map<String, Object> exMap = (Map<String, Object>) ex;
                example = exMap;
            }
        }
        mcpTool.setExample(example);

        Map<String, Object> outputSchema = buildOutputSchema(tool);
        mcpTool.setOutputSchema(outputSchema);

        return mcpTool;
    }

    private Map<String, Object> buildOutputSchema(Tool tool) {
        Map<String, Object> metadata = tool.getMetadata();
        if (metadata != null) {
            Object schema = metadata.get("outputSchema");
            if (schema instanceof Map<?, ?> map) {
                @SuppressWarnings("unchecked")
                Map<String, Object> copy = new LinkedHashMap<>((Map<String, Object>) map);
                return copy;
            }
        }

        String canonicalName = toCanonicalName(tool.getName());
        if (!"java.analyze".equals(canonicalName)) {
            return null;
        }

        Map<String, Object> issueProperties = new HashMap<>();
        issueProperties.put("file", Map.of("type", "string"));
        issueProperties.put("line", Map.of("type", "integer"));
        issueProperties.put("severity", Map.of("type", "string"));
        issueProperties.put("type", Map.of("type", "string"));
        issueProperties.put("message", Map.of("type", "string"));

        Map<String, Object> issueSchema = new HashMap<>();
        issueSchema.put("type", "object");
        issueSchema.put("properties", issueProperties);
        issueSchema.put("required", List.of("file", "message"));

        Map<String, Object> issuesSchema = new HashMap<>();
        issuesSchema.put("type", "array");
        issuesSchema.put("items", issueSchema);

        Map<String, Object> metricsProperties = new HashMap<>();
        metricsProperties.put("totalFiles", Map.of("type", "integer"));
        metricsProperties.put("issuesFound", Map.of("type", "integer"));
        metricsProperties.put("durationMs", Map.of("type", "integer"));

        Map<String, Object> metricsSchema = new HashMap<>();
        metricsSchema.put("type", "object");
        metricsSchema.put("properties", metricsProperties);

        Map<String, Object> properties = new HashMap<>();
        properties.put("issues", issuesSchema);
        properties.put("metrics", metricsSchema);

        Map<String, Object> outputSchema = new HashMap<>();
        outputSchema.put("type", "object");
        outputSchema.put("properties", properties);
        return outputSchema;
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

    private String toCanonicalName(String toolName) {
        if (toolName == null) {
            return null;
        }
        int idx = toolName.indexOf('_');
        if (idx < 0) {
            return toolName;
        }
        return toolName.substring(0, idx) + '.' + toolName.substring(idx + 1);
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
