package org.shark.renovatio.mcp.server.service;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.shark.renovatio.mcp.server.model.McpPrompt;
import org.shark.renovatio.mcp.server.model.McpResource;
import org.shark.renovatio.mcp.server.model.McpTool;
import org.shark.renovatio.mcp.server.model.ToolCallResult;
import org.shark.renovatio.core.service.LanguageProviderRegistry;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.io.ClassPathResource;
import org.springframework.stereotype.Service;
import org.springframework.context.event.EventListener;
import org.springframework.boot.context.event.ApplicationReadyEvent;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

@Service
public class McpToolingService {
    private static final Logger logger = LoggerFactory.getLogger(McpToolingService.class);
    private static final ObjectMapper JSON_MAPPER = new ObjectMapper();
    private final String spec;
    private final LanguageProviderRegistry providerRegistry;
    private final McpToolAdapter toolAdapter;
    private final List<McpPrompt> prompts;
    private final List<McpResource> resources;

    @Autowired
    public McpToolingService(LanguageProviderRegistry providerRegistry, McpToolAdapter toolAdapter) {
        this.providerRegistry = providerRegistry;
        this.toolAdapter = toolAdapter;
        try {
            var resource = new ClassPathResource("mcp-tooling.json", McpToolingService.class.getClassLoader());
            JsonNode root = JSON_MAPPER.readTree(resource.getInputStream());
            this.spec = root.path("spec").asText();
            
            this.prompts = createPrompts();
            this.resources = createResources();
        } catch (IOException e) {
            throw new RuntimeException("No se pudo leer mcp-tooling.json", e);
        }
    }

    /**
     * Get available MCP tools based on registered providers
     */
    public List<McpTool> getMcpTools() {
        // Use the new protocol-agnostic API and convert to MCP tools
        var tools = providerRegistry.generateTools();
        var mcpTools = toolAdapter.toMcpTools(tools);
        logger.debug("Resolved {} MCP tool(s)", mcpTools.size());
        return mcpTools;
    }

    @EventListener(ApplicationReadyEvent.class)
    public void logRegisteredTools() {
        try {
            var tools = getMcpTools();
            if (tools.isEmpty()) {
                logger.warn("No MCP tools registered.");
                return;
            }

            logger.info(formatToolCatalog(tools));
        } catch (Exception exception) {
            logger.warn("Could not list MCP tools at startup: {}", exception.getMessage(), exception);
        }
    }

    /**
     * Execute a tool using the provider registry
     */
    public Map<String, Object> executeTool(String toolName, Map<String, Object> arguments) {
        logger.debug("Executing MCP tool: '{}' with arguments: {}", toolName, arguments);

        try {
            String internalToolName = toInternalToolName(toolName);

            Map<String, Object> normalizedArguments = new HashMap<>(arguments);

            logger.debug("Routing tool call to provider: {} with args: {}", internalToolName, normalizedArguments);
            var result = providerRegistry.routeToolCall(internalToolName, normalizedArguments);

            // Ensure result has proper structure
            if (result == null) {
                logger.warn("Provider returned null result for tool: {}", internalToolName);
                Map<String, Object> errorResult = new HashMap<>();
                errorResult.put("type", "text");
                errorResult.put("text", "Tool execution returned no results");
                errorResult.put("success", false);
                return errorResult;
            }

            // Check if result indicates success
            Object successObj = result.get("success");
            if (successObj != null && Boolean.FALSE.equals(successObj)) {
                logger.warn("Tool execution failed for: {} - Result: {}", internalToolName, result);
            } else {
                logger.debug("Tool execution successful for: {}", internalToolName);
            }

            return result;

        } catch (Exception e) {
            logger.error("Error executing tool '{}': {}", toolName, e.getMessage(), e);
            Map<String, Object> errorResult = new HashMap<>();
            errorResult.put("type", "text");
            errorResult.put("text", "Error executing tool '" + toolName + "': " + e.getMessage());
            errorResult.put("success", false);
            errorResult.put("error", e.getClass().getSimpleName());
            return errorResult;
        }
    }

    /**
     * Execute a tool and produce an MCP-compliant result envelope.
     */
    public ToolCallResult executeToolWithEnvelope(String toolName, Map<String, Object> arguments) {
        Map<String, Object> rawResult = executeTool(toolName, arguments);
        return buildToolCallResult(toolName, rawResult);
    }

    /**
     * Get server specification
     */
    public String getSpec() {
        return spec;
    }

    /**
     * Get prompts
     */
    public List<McpPrompt> getPrompts() {
        return prompts;
    }

    /**
     * Get specific prompt by name
     */
    public McpPrompt getPrompt(String name) {
        return prompts.stream()
                .filter(p -> p.getName().equals(name))
                .findFirst()
                .orElse(null);
    }

    /**
     * Get resources
     */
    public List<McpResource> getResources() {
        return resources;
    }

    /**
     * Get specific resource by URI
     */
    public McpResource getResource(String uri) {
        return resources.stream()
                .filter(r -> r.getUri().equals(uri))
                .findFirst()
                .orElse(null);
    }

    /**
     * Read file content from workspace
     */
    public String readFileContent(String path) throws IOException {
        java.nio.file.Path filePath = java.nio.file.Paths.get(path);
        return java.nio.file.Files.readString(filePath);
    }

    /**
     * Write file content to workspace
     */
    public void writeFileContent(String path, String content) throws IOException {
        java.nio.file.Path filePath = java.nio.file.Paths.get(path);
        java.nio.file.Files.writeString(filePath, content);
    }

    /**
     * List workspace root directories/files
     */
    public List<String> listWorkspaces() {
        // Example: list directories in current working directory
        List<String> workspaces = new ArrayList<>();
        java.nio.file.Path root = java.nio.file.Paths.get("");
        try (var stream = java.nio.file.Files.list(root)) {
            stream.filter(java.nio.file.Files::isDirectory)
                  .forEach(dir -> workspaces.add(dir.toString()));
        } catch (IOException e) {
            // Log and return empty list
        }
        return workspaces;
    }

    /**
     * Describe workspace by id (directory name)
     */
    public Map<String, Object> describeWorkspace(String workspaceId) {
        Map<String, Object> info = new HashMap<>();
        java.nio.file.Path dir = java.nio.file.Paths.get(workspaceId);
        info.put("id", workspaceId);
        info.put("exists", java.nio.file.Files.exists(dir));
        info.put("isDirectory", java.nio.file.Files.isDirectory(dir));
        try {
            info.put("files", java.nio.file.Files.list(dir)
                .map(java.nio.file.Path::toString)
                .toArray(String[]::new));
        } catch (IOException e) {
            info.put("files", new String[0]);
        }
        return info;
    }

    /**
     * Get tool by name
     */
    public McpTool getTool(String toolName) {
        return getMcpTools().stream()
                .filter(tool -> tool.getName().equals(toolName))
                .findFirst()
                .orElse(null);
    }

    /**
     * Get supported languages from registered providers
     */
    public java.util.Set<String> getSupportedLanguages() {
        return providerRegistry.getSupportedLanguages();
    }

    private ToolCallResult buildToolCallResult(String toolName, Map<String, Object> rawResult) {
        String canonicalName = toCanonicalName(toolName);
        if (rawResult == null || rawResult.isEmpty()) {
            return ToolCallResult.error("Tool '" + canonicalName + "' returned no results");
        }

        Map<String, Object> structured = sanitizeMap(rawResult);
        boolean success = parseSuccess(structured);
        String type = stringValue(structured.get("type"), "").toLowerCase(Locale.ROOT);

        if (!success) {
            String message = stringValue(structured.get("message"), "Tool '" + canonicalName + "' execution failed");
            return ToolCallResult.error(message, structured);
        }

        String summary = buildSummary(canonicalName, structured);
        Object payload = adaptStructuredForClient(type, structured);
        return ToolCallResult.ok(summary, payload);
    }

    private Map<String, Object> sanitizeMap(Map<?, ?> raw) {
        Map<String, Object> sanitized = new LinkedHashMap<>();
        raw.forEach((key, value) -> sanitized.put(String.valueOf(key), sanitizeValue(value)));
        return sanitized;
    }

    private Object sanitizeValue(Object value) {
        if (value == null) {
            return null;
        }
        if (value instanceof Map<?, ?> map) {
            return sanitizeMap(map);
        }
        if (value instanceof List<?> list) {
            List<Object> sanitizedList = new ArrayList<>(list.size());
            for (Object item : list) {
                sanitizedList.add(sanitizeValue(item));
            }
            return sanitizedList;
        }
        if (value instanceof Number || value instanceof Boolean || value instanceof String) {
            return value;
        }
        if (value instanceof Enum<?> enumValue) {
            return enumValue.name();
        }
        try {
            return JSON_MAPPER.convertValue(value, Map.class);
        } catch (IllegalArgumentException ignored) {
            return String.valueOf(value);
        }
    }

    private String formatToolCatalog(List<McpTool> tools) {
        int nameColumnWidth = Math.min(
                tools.stream()
                        .map(McpTool::getName)
                        .filter(name -> name != null && !name.isBlank())
                        .mapToInt(String::length)
                        .max()
                        .orElse(10),
                60);

        StringBuilder builder = new StringBuilder();
        builder.append(System.lineSeparator())
                .append("================ MCP Tools ================")
                .append(System.lineSeparator())
                .append(String.format(Locale.ROOT, "Total: %d tool(s)%n", tools.size()));

        for (McpTool tool : tools) {
            String name = tool.getName() == null || tool.getName().isBlank()
                    ? "<unnamed>"
                    : tool.getName();
            String description = tool.getDescription() == null || tool.getDescription().isBlank()
                    ? "(sin descripción)"
                    : tool.getDescription();

            if (name.length() > nameColumnWidth) {
                name = name.substring(0, nameColumnWidth - 1) + "…";
            }

            builder.append(String.format(Locale.ROOT, "  %1$-" + nameColumnWidth + "s : %2$s%n", name, description));
        }

        builder.append("===========================================");
        return builder.toString();
    }

    private boolean parseSuccess(Map<String, Object> structured) {
        Object successValue = structured.get("success");
        if (successValue == null) {
            return !structured.containsKey("error");
        }
        return parseBoolean(successValue);
    }

    private boolean parseBoolean(Object value) {
        if (value instanceof Boolean bool) {
            return bool;
        }
        if (value instanceof Number number) {
            return number.intValue() != 0;
        }
        if (value instanceof String str) {
            return Boolean.parseBoolean(str);
        }
        return true;
    }

    private String buildSummary(String toolName, Map<String, Object> structured) {
        String type = stringValue(structured.get("type"), "").toLowerCase();
        return switch (type) {
            case "analyze" -> buildAnalyzeSummary(toolName, structured);
            case "metrics" -> buildMetricsSummary(toolName, structured);
            case "plan" -> buildPlanSummary(toolName, structured);
            case "apply" -> buildApplySummary(toolName, structured);
            case "diff" -> buildDiffSummary(toolName, structured);
            default -> buildGenericSummary(toolName, structured);
        };
    }

    private String buildAnalyzeSummary(String toolName, Map<String, Object> structured) {
        Map<String, Object> data = asMap(structured.get("data"));
        Map<String, Object> ast = asMap(structured.get("ast"));
        Map<String, Object> dependencies = asMap(structured.get("dependencies"));
        Map<String, Object> performance = asMap(structured.get("performance"));
        Map<String, Object> metricsData = asMap(data.get("metrics"));

        long files = getLong(metricsData, "totalFiles");
        if (files == 0) {
            List<?> analyzedFiles = asList(data.get("analyzedFiles"));
            if (!analyzedFiles.isEmpty()) {
                files = analyzedFiles.size();
            }
        }
        if (files == 0 && data.containsKey("files")) {
            files = asList(data.get("files")).size();
        }
        if (files == 0) {
            files = data.size();
        }

        long classes = getLong(ast, "totalClasses");
        long methods = getLong(ast, "totalMethods");
        long uniqueImports = getLong(dependencies, "uniqueImports");
        long totalImports = getLong(dependencies, "totalImports");

        Long durationMs = getOptionalLong(metricsData, "durationMs");
        if (durationMs == null) {
            durationMs = getOptionalLong(performance, "executionTimeMs");
        }
        if (durationMs == null) {
            durationMs = getOptionalLong(ast, "durationMs");
        }

        List<String> metrics = new ArrayList<>();
        if (classes > 0) {
            metrics.add(classes + " classes");
        }
        if (methods > 0) {
            metrics.add(methods + " methods");
        }
        if (uniqueImports > 0) {
            String importSummary = uniqueImports + " unique imports";
            if (totalImports > 0 && totalImports != uniqueImports) {
                importSummary += " (" + totalImports + " total)";
            }
            metrics.add(importSummary);
        }

        StringBuilder summary = new StringBuilder();
        summary.append(toolName)
            .append(": analyzed ")
            .append(files)
            .append(files == 1 ? " file" : " files");
        if (!metrics.isEmpty()) {
            summary.append(" (").append(String.join(", ", metrics)).append(")");
        }

        List<?> issues = asList(data.containsKey("issues") ? data.get("issues") : structured.get("issues"));
        if (!issues.isEmpty()) {
            summary.append(", found ").append(issues.size()).append(" issues");
        } else {
            long issuesFound = getLong(metricsData, "issuesFound");
            if (issuesFound > 0) {
                summary.append(", found ").append(issuesFound).append(" issues");
            }
        }

        if (durationMs != null && durationMs > 0) {
            summary.append(" in ").append(durationMs).append(" ms");
        }

        String message = stringValue(structured.get("message"), "");
        if (message.isEmpty()) {
            message = stringValue(data.get("summary"), "");
        }
        if (!message.isEmpty()) {
            summary.append(". ").append(message);
        } else {
            summary.append('.');
        }

        return summary.toString();
    }

    private String buildMetricsSummary(String toolName, Map<String, Object> structured) {
        Map<String, Object> metrics = asMap(structured.get("metrics"));
        List<String> parts = new ArrayList<>();
        if (metrics.containsKey("totalFiles")) {
            parts.add(metrics.get("totalFiles") + " files");
        }
        if (metrics.containsKey("issuesFound")) {
            parts.add(metrics.get("issuesFound") + " issues");
        }
        if (metrics.containsKey("durationMs")) {
            parts.add(metrics.get("durationMs") + " ms");
        }

        StringBuilder summary = new StringBuilder(toolName).append(": metrics collected");
        if (!parts.isEmpty()) {
            summary.append(" - ").append(String.join(", ", parts));
        }

        String message = stringValue(structured.get("message"), "");
        if (!message.isEmpty()) {
            summary.append(". ").append(message);
        } else {
            summary.append('.');
        }

        return summary.toString();
    }

    private String buildPlanSummary(String toolName, Map<String, Object> structured) {
        String planId = stringValue(structured.get("planId"), "plan");
        List<?> steps = asList(structured.get("steps"));
        StringBuilder summary = new StringBuilder(toolName)
            .append(": generated plan ")
            .append(planId)
            .append(" with ")
            .append(steps.size())
            .append(" steps");

        String message = stringValue(structured.get("message"), "");
        if (!message.isEmpty()) {
            summary.append(". ").append(message);
        } else {
            summary.append('.');
        }

        return summary.toString();
    }

    private String buildApplySummary(String toolName, Map<String, Object> structured) {
        boolean dryRun = parseBoolean(structured.get("dryRun"));
        List<?> changes = asList(structured.get("changes"));
        String action = dryRun ? "previewed" : "applied";

        StringBuilder summary = new StringBuilder(toolName)
            .append(": ")
            .append(action)
            .append(' ')
            .append(changes.size())
            .append(" changes");

        String message = stringValue(structured.get("message"), "");
        if (!message.isEmpty()) {
            summary.append(". ").append(message);
        } else {
            summary.append('.');
        }

        return summary.toString();
    }

    private String buildDiffSummary(String toolName, Map<String, Object> structured) {
        List<?> hunks = asList(structured.get("hunks"));
        StringBuilder summary = new StringBuilder(toolName)
            .append(": produced diff with ")
            .append(hunks.size())
            .append(hunks.size() == 1 ? " hunk" : " hunks");

        String message = stringValue(structured.get("message"), "");
        if (!message.isEmpty()) {
            summary.append(". ").append(message);
        } else {
            summary.append('.');
        }

        return summary.toString();
    }

    private String buildGenericSummary(String toolName, Map<String, Object> structured) {
        String message = stringValue(structured.get("message"), "");
        if (!message.isEmpty()) {
            return toolName + ": " + message;
        }
        return toolName + " executed successfully.";
    }

    private Object adaptStructuredForClient(String type, Map<String, Object> structured) {
        if ("analyze".equals(type)) {
            return simplifyAnalyzeStructured(structured);
        }
        return structured;
    }

    private Map<String, Object> simplifyAnalyzeStructured(Map<String, Object> structured) {
        Map<String, Object> simplified = new LinkedHashMap<>();
        Map<String, Object> data = asMap(structured.get("data"));

        simplified.put("type", "analyze");
        simplified.put("success", parseSuccess(structured));

        String summary = firstNonEmptyString(
            structured.get("summary"),
            data.get("summary"),
            structured.get("message")
        );
        if (!summary.isEmpty()) {
            simplified.put("summary", summary);
        }

        List<?> issues = firstNonEmptyList(structured.get("issues"), data.get("issues"));
        simplified.put("issues", new ArrayList<>(issues));

        Map<String, Object> metrics = mergeMaps(structured.get("metrics"), data.get("metrics"));
        if (!metrics.isEmpty()) {
            simplified.put("metrics", metrics);
        } else {
            simplified.put("metrics", Collections.emptyMap());
        }

        List<?> files = firstNonEmptyList(
            structured.get("analyzedFiles"),
            structured.get("files"),
            data.get("analyzedFiles")
        );
        simplified.put("analyzedFiles", new ArrayList<>(files));

        List<?> diffs = firstNonEmptyList(structured.get("diffs"), data.get("diffs"));
        if (!diffs.isEmpty()) {
            simplified.put("diffs", new ArrayList<>(diffs));
        }

        Boolean applied = firstBoolean(structured.get("applied"), data.get("applied"));
        if (applied != null) {
            simplified.put("applied", applied);
        }

        Long duration = getOptionalLong(metrics, "durationMs");
        if (duration == null) {
            duration = getOptionalLong(asMap(structured.get("performance")), "executionTimeMs");
        }
        if (duration == null) {
            duration = getOptionalLong(asMap(data.get("performance")), "executionTimeMs");
        }
        if (duration != null) {
            simplified.put("durationMs", duration);
        }

        return simplified;
    }

    private Map<String, Object> mergeMaps(Object... candidates) {
        Map<String, Object> merged = new LinkedHashMap<>();
        for (Object candidate : candidates) {
            Map<String, Object> map = asMap(candidate);
            for (Map.Entry<String, Object> entry : map.entrySet()) {
                merged.putIfAbsent(entry.getKey(), entry.getValue());
            }
        }
        return merged;
    }

    private List<?> firstNonEmptyList(Object... candidates) {
        for (Object candidate : candidates) {
            List<?> list = asList(candidate);
            if (!list.isEmpty()) {
                return list;
            }
        }
        return Collections.emptyList();
    }

    private String firstNonEmptyString(Object... candidates) {
        for (Object candidate : candidates) {
            String text = stringValue(candidate, "");
            if (!text.isEmpty()) {
                return text;
            }
        }
        return "";
    }

    private Boolean firstBoolean(Object... candidates) {
        for (Object candidate : candidates) {
            if (candidate == null) {
                continue;
            }
            return parseBoolean(candidate);
        }
        return null;
    }

    private Map<String, Object> asMap(Object value) {
        if (value instanceof Map<?, ?> map) {
            Map<String, Object> copy = new LinkedHashMap<>();
            map.forEach((k, v) -> copy.put(String.valueOf(k), v));
            return copy;
        }
        return Collections.emptyMap();
    }

    private List<?> asList(Object value) {
        if (value instanceof List<?> list) {
            return list;
        }
        return Collections.emptyList();
    }

    private long getLong(Map<String, Object> map, String key) {
        Object value = map.get(key);
        if (value instanceof Number number) {
            return number.longValue();
        }
        if (value instanceof String str && !str.isBlank()) {
            try {
                return Long.parseLong(str);
            } catch (NumberFormatException ignored) {
                return 0L;
            }
        }
        return 0L;
    }

    private Long getOptionalLong(Map<String, Object> map, String key) {
        Object value = map.get(key);
        if (value instanceof Number number) {
            return number.longValue();
        }
        if (value instanceof String str && !str.isBlank()) {
            try {
                return Long.parseLong(str);
            } catch (NumberFormatException ignored) {
                return null;
            }
        }
        return null;
    }

    private String stringValue(Object value, String defaultValue) {
        if (value == null) {
            return defaultValue;
        }
        String text = value.toString().trim();
        return text.isEmpty() ? defaultValue : text;
    }

    private List<McpPrompt> createPrompts() {
        List<McpPrompt> prompts = new ArrayList<>();

        List<McpPrompt.Message> formatMessages = new ArrayList<>();
        formatMessages.add(new McpPrompt.Message("user", "Provide code to format"));
        formatMessages.add(new McpPrompt.Message("assistant", "Here is the formatted code"));
        prompts.add(new McpPrompt("format-code", "Format source code", formatMessages));

        List<McpPrompt.Message> analyzeMessages = new ArrayList<>();
        analyzeMessages.add(new McpPrompt.Message("user", "Analyze this code structure"));
        analyzeMessages.add(new McpPrompt.Message("assistant", "Code analysis completed"));
        prompts.add(new McpPrompt("analyze-code", "Analyze code structure", analyzeMessages));

        List<McpPrompt.Message> generateStubMessages = new ArrayList<>();
        generateStubMessages.add(new McpPrompt.Message("user", "Generate Java stubs for this COBOL interface"));
        generateStubMessages.add(new McpPrompt.Message("assistant", "Java stubs generated"));
        prompts.add(new McpPrompt("generate-stubs", "Generate interface stubs", generateStubMessages));

        return prompts;
    }

    private List<McpResource> createResources() {
        List<McpResource> resources = new ArrayList<>();
        resources.add(new McpResource("file://welcome.txt", "Welcome", "text/plain",
                "Welcome to the Renovatio Multi-Language MCP server"));
        resources.add(new McpResource("file://architecture.txt", "Architecture", "text/plain",
                "Multi-language architecture with Provider SPI pattern"));
        resources.add(new McpResource("file://nql-syntax.txt", "NQL Syntax", "text/plain",
                "Natural Query Language syntax reference"));
        return resources;
    }

    /**
     * Log all registered language providers and generated MCP tools in a unified, descriptive format.
     */
    public void logProvidersAndTools() {
        logger.info("================ Renovatio MCP Server Started ================");
        logger.info("Registered Language Providers:");
        var providers = providerRegistry.getAllProviders();
        if (providers.isEmpty()) {
            logger.warn("No language providers registered.");
        } else {
            for (var provider : providers) {
                logger.info("- {}: {}", provider.language(), provider.capabilities());
            }
        }
        logger.info("");
        logger.info("Generated MCP Tools:");
        var tools = getMcpTools();
        if (tools.isEmpty()) {
            logger.warn("No MCP tools generated.");
        } else {
            for (var tool : tools) {
                logger.info("- {}: {}", tool.getName(), tool.getDescription());
            }
        }
        logger.info("===============================================================");
    }

    @EventListener(ApplicationReadyEvent.class)
    public void logAvailableToolsOnApplicationReady() {
        List<McpTool> tools = getMcpTools();
        if (tools == null || tools.isEmpty()) {
            logger.warn("No MCP Tools available on server startup. Check provider registration.");
        } else {
            logger.info("MCP Tools available on server startup:");
            for (McpTool tool : tools) {
                logger.info("- {}: {}", tool.getName(), tool.getDescription());
            }
        }
    }

    private String toInternalToolName(String toolName) {
        if (toolName == null) {
            return null;
        }
        int idx = toolName.indexOf('_');
        if (idx < 0) {
            return toolName;
        }
        String language = toolName.substring(0, idx);
        String remainder = toolName.substring(idx + 1);
        return language + '.' + remainder;
    }

    private String toCanonicalName(String toolName) {
        if (toolName == null) {
            return null;
        }
        int idx = toolName.indexOf('_');
        if (idx < 0) {
            return toolName;
        }
        String language = toolName.substring(0, idx);
        String remainder = toolName.substring(idx + 1);
        return language + '.' + remainder;
    }
}
