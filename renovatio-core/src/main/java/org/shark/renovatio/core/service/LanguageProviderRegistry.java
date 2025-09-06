package org.shark.renovatio.core.service;

import org.shark.renovatio.core.mcp.McpTool;
import org.shark.renovatio.shared.spi.LanguageProvider;
import org.shark.renovatio.shared.nql.NqlQuery;
import org.shark.renovatio.shared.nql.NqlCompileResult;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.concurrent.ConcurrentHashMap;
import java.util.ArrayList;
import java.util.HashMap;

/**
 * Core service that manages language providers and tool routing
 */
@Service
public class LanguageProviderRegistry {
    
    private final Map<String, LanguageProvider> providers = new ConcurrentHashMap<>();
    
    /**
     * Register a language provider
     */
    public void registerProvider(LanguageProvider provider) {
        providers.put(provider.language(), provider);
    }
    
    /**
     * Get provider for a specific language
     */
    public LanguageProvider getProvider(String language) {
        return providers.get(language);
    }
    
    /**
     * Get all registered providers
     */
    public List<LanguageProvider> getAllProviders() {
        return new ArrayList<>(providers.values());
    }
    
    /**
     * Get all supported languages
     */
    public List<String> getSupportedLanguages() {
        return new ArrayList<>(providers.keySet());
    }
    
    /**
     * Generate MCP tools dynamically based on registered providers
     */
    public List<McpTool> generateMcpTools() {
        List<McpTool> tools = new ArrayList<>();
        
        // Add common tools
        tools.add(createNqlCompileTool());
        tools.add(createCommonIndexTool());
        tools.add(createCommonSearchTool());
        
        // Add provider-specific tools
        for (LanguageProvider provider : providers.values()) {
            String lang = provider.language();
            var capabilities = provider.capabilities();
            
            if (capabilities.contains(LanguageProvider.Capabilities.ANALYZE)) {
                tools.add(createProviderTool(lang, "analyze", "Analyze code structure and extract information"));
            }
            if (capabilities.contains(LanguageProvider.Capabilities.PLAN)) {
                tools.add(createProviderTool(lang, "plan", "Create execution plan for transformations"));
            }
            if (capabilities.contains(LanguageProvider.Capabilities.APPLY)) {
                tools.add(createProviderTool(lang, "apply", "Apply transformation plan"));
            }
            if (capabilities.contains(LanguageProvider.Capabilities.DIFF)) {
                tools.add(createProviderTool(lang, "diff", "Generate semantic diff"));
            }
            if (capabilities.contains(LanguageProvider.Capabilities.STUBS)) {
                tools.add(createProviderTool(lang, "generate_stubs", "Generate interface stubs"));
            }
            if (capabilities.contains(LanguageProvider.Capabilities.METRICS)) {
                tools.add(createProviderTool(lang, "metrics", "Calculate code metrics"));
            }
        }
        
        return tools;
    }
    
    /**
     * Route tool call to appropriate provider
     */
    public Map<String, Object> routeToolCall(String toolName, Map<String, Object> arguments) {
        String[] parts = toolName.split("\\.", 2);
        
        if (parts.length < 2) {
            return createErrorResult("Invalid tool name format. Expected: language.operation");
        }
        
        String namespace = parts[0];
        String operation = parts[1];
        
        // Handle common tools
        if ("common".equals(namespace)) {
            return handleCommonTool(operation, arguments);
        }
        
        // Handle NQL tools
        if ("nql".equals(namespace)) {
            return handleNqlTool(operation, arguments);
        }
        
        // Handle provider-specific tools
        LanguageProvider provider = providers.get(namespace);
        if (provider == null) {
            return createErrorResult("No provider found for language: " + namespace);
        }
        
        return handleProviderTool(provider, operation, arguments);
    }
    
    private McpTool createNqlCompileTool() {
        McpTool tool = new McpTool();
        tool.setName("nql.compile");
        tool.setDescription("Compile natural language to NQL");
        
        Map<String, Object> schema = new HashMap<>();
        schema.put("type", "object");
        Map<String, Object> properties = new HashMap<>();
        properties.put("question", Map.of("type", "string", "description", "Natural language question"));
        properties.put("context", Map.of("type", "string", "description", "Additional context"));
        schema.put("properties", properties);
        schema.put("required", List.of("question"));
        
        tool.setInputSchema(schema);
        return tool;
    }
    
    private McpTool createCommonIndexTool() {
        McpTool tool = new McpTool();
        tool.setName("common.index");
        tool.setDescription("Index repository for search");
        
        Map<String, Object> schema = new HashMap<>();
        schema.put("type", "object");
        Map<String, Object> properties = new HashMap<>();
        properties.put("repoId", Map.of("type", "string", "description", "Repository ID"));
        schema.put("properties", properties);
        schema.put("required", List.of("repoId"));
        
        tool.setInputSchema(schema);
        return tool;
    }
    
    private McpTool createCommonSearchTool() {
        McpTool tool = new McpTool();
        tool.setName("common.search");
        tool.setDescription("Search indexed repository");
        
        Map<String, Object> schema = new HashMap<>();
        schema.put("type", "object");
        Map<String, Object> properties = new HashMap<>();
        properties.put("repoId", Map.of("type", "string", "description", "Repository ID"));
        properties.put("query", Map.of("type", "string", "description", "Search query"));
        properties.put("path", Map.of("type", "string", "description", "Path filter"));
        schema.put("properties", properties);
        schema.put("required", List.of("repoId", "query"));
        
        tool.setInputSchema(schema);
        return tool;
    }
    
    private McpTool createProviderTool(String language, String operation, String description) {
        McpTool tool = new McpTool();
        tool.setName(language + "." + operation);
        tool.setDescription(description + " for " + language);
        
        Map<String, Object> schema = new HashMap<>();
        schema.put("type", "object");
        Map<String, Object> properties = new HashMap<>();
        properties.put("nql", Map.of("type", "string", "description", "NQL query"));
        properties.put("scope", Map.of("type", "string", "description", "Operation scope"));
        schema.put("properties", properties);
        
        tool.setInputSchema(schema);
        return tool;
    }
    
    private Map<String, Object> handleCommonTool(String operation, Map<String, Object> arguments) {
        // Placeholder for common operations like indexing and search
        Map<String, Object> result = new HashMap<>();
        result.put("type", "text");
        result.put("text", "Common operation '" + operation + "' not yet implemented");
        return result;
    }
    
    private Map<String, Object> handleNqlTool(String operation, Map<String, Object> arguments) {
        // Placeholder for NQL compilation
        Map<String, Object> result = new HashMap<>();
        result.put("type", "text");
        result.put("text", "NQL operation '" + operation + "' not yet implemented");
        return result;
    }
    
    private Map<String, Object> handleProviderTool(LanguageProvider provider, String operation, Map<String, Object> arguments) {
        // Placeholder for provider operations - to be implemented by specific providers
        Map<String, Object> result = new HashMap<>();
        result.put("type", "text");
        result.put("text", "Provider operation '" + operation + "' for " + provider.language() + " not yet implemented");
        return result;
    }
    
    private Map<String, Object> createErrorResult(String message) {
        Map<String, Object> result = new HashMap<>();
        result.put("type", "text");
        result.put("text", "Error: " + message);
        return result;
    }
}