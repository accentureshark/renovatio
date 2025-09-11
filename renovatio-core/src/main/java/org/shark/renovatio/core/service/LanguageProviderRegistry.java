package org.shark.renovatio.core.service;

import org.shark.renovatio.core.mcp.McpTool;
import org.shark.renovatio.shared.spi.LanguageProvider;
import org.shark.renovatio.shared.nql.NqlQuery;
import org.shark.renovatio.shared.nql.NqlCompileResult;
import org.shark.renovatio.shared.domain.*;
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
        logger.info("generateMcpTools called. Providers: {}", getSupportedLanguages());
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
        
        logger.info("Generated MCP tools: {}", tools);
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
        try {
            // Extract common parameters
            String nqlString = (String) arguments.get("nql");
            String scopeString = (String) arguments.get("scope");
            String workspacePath = (String) arguments.get("workspacePath");

            // Create basic NQL query object
            NqlQuery query = new NqlQuery();
            query.setOriginalQuery(nqlString);
            query.setLanguage(provider.language());
            
            // Create basic scope
            Scope scope = new Scope();
            if (scopeString != null) {
                scope.setPaths(List.of(scopeString.split(",")));
            }
            
            // Create workspace with the correct path from arguments
            Workspace workspace = new Workspace();
            workspace.setId("mcp-" + System.currentTimeMillis());
            workspace.setPath(workspacePath != null ? workspacePath : ".");
            workspace.setBranch("main");
            
            Map<String, Object> result = new HashMap<>();
            
            switch (operation) {
                case "analyze":
                    var analyzeResult = provider.analyze(query, workspace);
                    result.put("type", "object");
                    result.put("success", analyzeResult.isSuccess());
                    result.put("message", analyzeResult.getMessage());
                    result.put("runId", analyzeResult.getRunId());
                    result.put("ast", analyzeResult.getAst());
                    result.put("dependencies", analyzeResult.getDependencies());
                    result.put("symbols", analyzeResult.getSymbols());
                    // Include data if available
                    if (analyzeResult.getData() != null) {
                        result.put("data", analyzeResult.getData());
                    }
                    break;
                    
                case "plan":
                    var planResult = provider.plan(query, scope, workspace);
                    result.put("type", "object");
                    result.put("success", planResult.isSuccess());
                    result.put("message", planResult.getMessage());
                    result.put("planId", planResult.getPlanId());
                    result.put("planContent", planResult.getPlanContent());
                    result.put("steps", planResult.getSteps());
                    break;
                    
                case "apply":
                    String planId = (String) arguments.getOrDefault("planId", "default-plan");
                    boolean dryRun = Boolean.parseBoolean((String) arguments.getOrDefault("dryRun", "true"));
                    var applyResult = provider.apply(planId, dryRun, workspace);
                    result.put("type", "object");
                    result.put("success", applyResult.isSuccess());
                    result.put("message", applyResult.getMessage());
                    result.put("diff", applyResult.getDiff());
                    result.put("changes", applyResult.getChanges());
                    result.put("dryRun", applyResult.isDryRun());
                    break;
                    
                case "diff":
                    String runId = (String) arguments.getOrDefault("runId", "default-run");
                    var diffResult = provider.diff(runId, workspace);
                    result.put("type", "object");
                    result.put("success", diffResult.isSuccess());
                    result.put("message", diffResult.getMessage());
                    result.put("unifiedDiff", diffResult.getUnifiedDiff());
                    result.put("semanticDiff", diffResult.getSemanticDiff());
                    result.put("hunks", diffResult.getHunks());
                    break;
                    
                case "generate_stubs":
                    var stubResultOpt = provider.generateStubs(query, workspace);
                    if (stubResultOpt.isPresent()) {
                        var stubResult = stubResultOpt.get();
                        result.put("type", "object");
                        result.put("success", stubResult.isSuccess());
                        result.put("message", stubResult.getMessage());
                        result.put("targetLanguage", stubResult.getTargetLanguage());
                        result.put("generatedFiles", stubResult.getGeneratedFiles());
                        result.put("stubTemplate", stubResult.getStubTemplate());
                    } else {
                        result.put("type", "text");
                        result.put("text", "Stub generation not supported for " + provider.language());
                    }
                    break;
                    
                case "metrics":
                    var metricsResult = provider.metrics(scope, workspace);
                    result.put("type", "object");
                    result.put("success", metricsResult.isSuccess());
                    result.put("message", metricsResult.getMessage());
                    result.put("metrics", metricsResult.getMetrics());
                    result.put("details", metricsResult.getDetails());
                    break;
                    
                default:
                    result.put("type", "text");
                    result.put("text", "Unknown operation: " + operation);
            }
            
            return result;
            
        } catch (Exception e) {
            Map<String, Object> result = new HashMap<>();
            result.put("type", "text");
            result.put("text", "Error executing " + operation + " for " + provider.language() + ": " + e.getMessage());
            return result;
        }
    }
    
    private Map<String, Object> createErrorResult(String message) {
        Map<String, Object> result = new HashMap<>();
        result.put("type", "text");
        result.put("text", "Error: " + message);
        return result;
    }

    private static final org.slf4j.Logger logger = org.slf4j.LoggerFactory.getLogger(LanguageProviderRegistry.class);

    public LanguageProviderRegistry() {
        logger.info("LanguageProviderRegistry constructor called");
    }

    @jakarta.annotation.PostConstruct
    public void registerDefaultProviders() {
        logger.info("registerDefaultProviders called");
        // Register Java provider
        try {
            Class<?> javaProviderClass = Class.forName("org.shark.renovatio.provider.java.JavaLanguageProvider");
            LanguageProvider javaProvider = (LanguageProvider) javaProviderClass.getDeclaredConstructor().newInstance();
            registerProvider(javaProvider);
            logger.info("JavaLanguageProvider registered successfully.");
        } catch (Exception e) {
            logger.error("Could not register JavaLanguageProvider: {}", e.getMessage());
        }
        // Register COBOL provider
        try {
            Class<?> cobolProviderClass = Class.forName("org.shark.renovatio.provider.cobol.CobolLanguageProvider");
            LanguageProvider cobolProvider = (LanguageProvider) cobolProviderClass.getDeclaredConstructor().newInstance();
            registerProvider(cobolProvider);
            logger.info("CobolLanguageProvider registered successfully.");
        } catch (Exception e) {
            logger.error("Could not register CobolLanguageProvider: {}", e.getMessage());
        }
        logger.info("Providers after registration: {}", getSupportedLanguages());
    }
}
