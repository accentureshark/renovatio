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
        if (mcpToolName != null) {
            int dotIndex = mcpToolName.indexOf('.');
            if (dotIndex > 0) {
                int underscoreIndex = mcpToolName.indexOf('_');
                if (underscoreIndex < 0 || dotIndex < underscoreIndex) {
                    mcpToolName = mcpToolName.substring(0, dotIndex) + '_' + mcpToolName.substring(dotIndex + 1);
                }
            }
        }

        mcpTool.setName(mcpToolName);
        mcpTool.setDescription(tool.getDescription());

        Map<String, Object> toolMetadata = tool.getMetadata();
        Map<String, Object> metadataCopy = toolMetadata != null
                ? new LinkedHashMap<>(toolMetadata)
                : new LinkedHashMap<>();
        mcpTool.setMetadata(metadataCopy);

        Map<String, Object> schema = tool.getInputSchema();
        Map<String, Object> mcpSchema = normalizeSchema(schema);

        if (isProviderTool(mcpToolName)) {
            ensureWorkspacePath(mcpSchema);
        }

        if (mcpSchema != null && mcpSchema.get("required") instanceof List<?> requiredList
                && ((List<?>) mcpSchema.get("required")).isEmpty()) {
            mcpSchema.remove("required");
        }

        mcpTool.setInputSchema(mcpSchema);

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
        if (metadataCopy.containsKey("parameters")) {
            List<Map<String, Object>> metadataParameters = convertToParameterList(metadataCopy.get("parameters"));
            if (metadataParameters != null) {
                parameters = metadataParameters;
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
        if (metadataCopy.containsKey("example")) {
            Map<String, Object> metadataExample = convertToMap(metadataCopy.get("example"));
            if (metadataExample != null) {
                example = metadataExample;
            }
        }
        mcpTool.setExample(example);

        Map<String, Object> outputSchema = buildOutputSchema(tool);
        mcpTool.setOutputSchema(outputSchema);

        return mcpTool;
    }

    private List<Map<String, Object>> convertToParameterList(Object parametersObj) {
        if (!(parametersObj instanceof List<?> list)) {
            return null;
        }
        List<Map<String, Object>> parameters = new ArrayList<>();
        for (Object item : list) {
            if (item instanceof Map<?, ?> mapItem) {
                Map<String, Object> parameter = new LinkedHashMap<>();
                mapItem.forEach((key, value) -> parameter.put(String.valueOf(key), value));
                parameters.add(parameter);
            }
        }
        return parameters;
    }

    private Map<String, Object> convertToMap(Object value) {
        if (value instanceof Map<?, ?> map) {
            Map<String, Object> copy = new LinkedHashMap<>();
            map.forEach((key, mapValue) -> copy.put(String.valueOf(key), mapValue));
            return copy;
        }
        return null;
    }

    private Map<String, Object> normalizeSchema(Map<String, Object> schema) {
        Map<String, Object> normalized = new LinkedHashMap<>();
        if (schema != null) {
            schema.forEach((key, value) -> normalized.put(String.valueOf(key), value));
        }

        Object propertiesObj = normalized.get("properties");
        Map<String, Object> properties = new LinkedHashMap<>();
        if (propertiesObj instanceof Map<?, ?> map) {
            map.forEach((key, value) -> properties.put(String.valueOf(key), value));
        }
        normalized.put("properties", properties);

        Object requiredObj = normalized.get("required");
        List<String> required = new ArrayList<>();
        if (requiredObj instanceof List<?> list) {
            for (Object entry : list) {
                if (entry != null) {
                    required.add(String.valueOf(entry));
                }
            }
        }
        if (!required.isEmpty()) {
            normalized.put("required", required);
        }

        normalized.putIfAbsent("type", "object");
        normalized.putIfAbsent("additionalProperties", Boolean.FALSE);
        return normalized;
    }

    private void ensureWorkspacePath(Map<String, Object> schema) {
        if (schema == null) {
            return;
        }
        Object propsObj = schema.get("properties");
        @SuppressWarnings("unchecked")
        Map<String, Object> props = propsObj instanceof Map<?, ?>
                ? new LinkedHashMap<>((Map<String, Object>) propsObj)
                : new LinkedHashMap<>();
        schema.put("properties", props);


        if (!props.containsKey("workspacePath")) {
            Map<String, Object> workspacePathProperty = new LinkedHashMap<>();
            workspacePathProperty.put("type", "string");
            workspacePathProperty.put("description", "Path to the workspace directory to analyze");
            props.put("workspacePath", workspacePathProperty);

            List<String> required = new ArrayList<>();
            Object requiredObj = schema.get("required");
            if (requiredObj instanceof List<?> list) {
                for (Object entry : list) {
                    if (entry != null) {
                        required.add(String.valueOf(entry));
                    }
                }
            }
            if (!required.contains("workspacePath")) {
                required.add("workspacePath");
            }
            if (!required.isEmpty()) {
                schema.put("required", required);
            }
        }
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

        org.shark.renovatio.shared.domain.BasicTool basicTool = new org.shark.renovatio.shared.domain.BasicTool(
            internalToolName,
            mcpTool.getDescription(),
            mcpTool.getInputSchema()
        );

        Map<String, Object> metadata = mcpTool.getMetadata();
        if (metadata != null) {
            basicTool.setMetadata(new LinkedHashMap<>(metadata));
        }

        Map<String, Object> example = mcpTool.getExample();
        if (example != null && (metadata == null || !metadata.containsKey("example"))) {
            // Preserve schema-derived example symmetry by keeping it inside metadata when absent
            Map<String, Object> metadataCopy = new LinkedHashMap<>(basicTool.getMetadata());
            Map<String, Object> exampleCopy = convertToMap(example);
            if (exampleCopy != null) {
                metadataCopy.put("example", exampleCopy);
                basicTool.setMetadata(metadataCopy);
            }
        }

        List<Map<String, Object>> parameters = mcpTool.getParameters();
        if (parameters != null && !parameters.isEmpty() && (metadata == null || !metadata.containsKey("parameters"))) {
            List<Map<String, Object>> parameterCopies = parameters.stream()
                    .map(param -> {
                        Map<String, Object> copy = new LinkedHashMap<>();
                        param.forEach((key, value) -> copy.put(String.valueOf(key), value));
                        return copy;
                    })
                    .collect(Collectors.toList());

            Map<String, Object> metadataCopy = new LinkedHashMap<>(basicTool.getMetadata());
            metadataCopy.put("parameters", parameterCopies);
            basicTool.setMetadata(metadataCopy);
        }

        return basicTool;
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
