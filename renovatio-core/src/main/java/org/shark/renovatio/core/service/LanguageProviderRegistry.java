package org.shark.renovatio.core.service;

import org.shark.renovatio.shared.spi.LanguageProvider;
import org.shark.renovatio.shared.domain.*;
import org.shark.renovatio.shared.nql.NqlQuery;
import org.springframework.stereotype.Service;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import jakarta.annotation.PostConstruct;
import java.util.*;

/**
 * Registry for language providers that handles routing tool calls to appropriate providers.
 * This component connects MCP tools to language providers using Spring's auto-configuration.
 */
@Service
public class LanguageProviderRegistry {

    private static final Logger logger = LoggerFactory.getLogger(LanguageProviderRegistry.class);

    @Autowired
    private ApplicationContext applicationContext;

    private final Map<String, LanguageProvider> providers = new HashMap<>();

    /**
     * Auto-register all LanguageProvider beans after Spring context initialization
     */
    @PostConstruct
    public void registerDefaultProviders() {
        logger.info("LanguageProviderRegistry initializing...");

        try {
            // Find all LanguageProvider beans in the Spring context
            Map<String, LanguageProvider> providerBeans = applicationContext.getBeansOfType(LanguageProvider.class);

            logger.info("Found {} LanguageProvider beans in Spring context", providerBeans.size());

            for (Map.Entry<String, LanguageProvider> entry : providerBeans.entrySet()) {
                String beanName = entry.getKey();
                LanguageProvider provider = entry.getValue();

                providers.put(provider.language(), provider);
                logger.info("Auto-registered LanguageProvider: {} (bean: {}) with capabilities: {}",
                        provider.language(), beanName, provider.capabilities());
            }

            logger.info("LanguageProviderRegistry initialized with {} providers: {}",
                    providers.size(), providers.keySet());

        } catch (Exception e) {
            logger.error("Error during LanguageProvider auto-registration: {}", e.getMessage(), e);
        }
    }

    /**
     * Manual registration method (kept for compatibility)
     */
    public void registerProvider(LanguageProvider provider) {
        providers.put(provider.language(), provider);
        logger.info("Manually registered language provider: {} with capabilities: {}",
                provider.language(), provider.capabilities());
    }

    /**
     * Get all registered providers
     */
    public Collection<LanguageProvider> getAllProviders() {
        return providers.values();
    }

    /**
     * Get supported languages
     */
    public Set<String> getSupportedLanguages() {
        return providers.keySet();
    }

    /**
     * Get a specific provider by language
     */
    public LanguageProvider getProvider(String language) {
        return providers.get(language);
    }

    /**
     * Generate a list of all MCP-compliant tools from all registered language providers.
     */
    public List<Tool> generateTools() {
        List<Tool> allTools = new ArrayList<>();
        for (LanguageProvider provider : providers.values()) {
            List<Tool> tools = provider.getTools();
            if (tools != null) {
                allTools.addAll(tools);
            }
        }
        return allTools;
    }

    /**
     * Route tool call to appropriate language provider
     */
    public Map<String, Object> routeToolCall(String toolName, Map<String, Object> arguments) {
        logger.info("=== ROUTING TOOL CALL ===");
        logger.info("Tool name: {}", toolName);
        logger.info("Arguments: {}", arguments);

        try {
            // Parse tool name to extract language and capability
            String[] parts = toolName.split("\\.");
            if (parts.length != 2) {
                return createErrorResult("Invalid tool name format: " + toolName);
            }

            String language = parts[0];
            String capability = parts[1].toLowerCase();

            LanguageProvider provider = providers.get(language);
            if (provider == null) {
                return createErrorResult("No provider found for language: " + language);
            }

            logger.info("Found provider for language: {}", language);
            logger.info("Provider class: {}", provider.getClass().getName());
            logger.info("Provider capabilities: {}", provider.capabilities());

            // Create workspace and query objects
            Workspace workspace = createWorkspace(arguments);
            NqlQuery query = createNqlQuery(arguments);
            Scope scope = createScope(arguments);

            logger.info("Created workspace: path={}, id={}", workspace.getPath(), workspace.getId());
            logger.info("Created query: query={}, language={}", query.getOriginalQuery(), query.getLanguage());
            logger.info("Created scope: includePatterns={}", scope.getIncludePatterns());

            // Route to appropriate method based on capability
            logger.info("Routing to capability: {}", capability);
            switch (capability) {
                case "analyze":
                    logger.info("Calling provider.analyze()...");
                    AnalyzeResult analyzeResult = provider.analyze(query, workspace);
                    logger.info("Provider.analyze() returned: success={}, message={}, runId={}",
                            analyzeResult.isSuccess(), analyzeResult.getMessage(), analyzeResult.getRunId());
                    Map<String, Object> analyzeMap = convertToMap(analyzeResult);
                    logger.info("Converted result: {}", analyzeMap);
                    return analyzeMap;

                case "metrics":
                    logger.info("Calling provider.metrics()...");
                    MetricsResult metricsResult = provider.metrics(scope, workspace);
                    logger.info("Provider.metrics() returned: success={}, message={}",
                            metricsResult.isSuccess(), metricsResult.getMessage());
                    Map<String, Object> metricsMap = convertToMap(metricsResult);
                    logger.info("Converted result: {}", metricsMap);
                    return metricsMap;

                case "plan":
                    logger.info("Calling provider.plan()...");
                    PlanResult planResult = provider.plan(query, scope, workspace);
                    logger.info("Provider.plan() returned: success={}, message={}",
                            planResult.isSuccess(), planResult.getMessage());
                    return convertToMap(planResult);

                case "apply":
                    String planId = (String) arguments.get("planId");
                    boolean dryRun = Boolean.parseBoolean(arguments.getOrDefault("dryRun", "true").toString());
                    logger.info("Calling provider.apply() with planId={}, dryRun={}", planId, dryRun);
                    ApplyResult applyResult = provider.apply(planId, dryRun, workspace);
                    logger.info("Provider.apply() returned: success={}, message={}",
                            applyResult.isSuccess(), applyResult.getMessage());
                    return convertToMap(applyResult);

                case "diff":
                    String runId = (String) arguments.get("runId");
                    logger.info("Calling provider.diff() with runId={}", runId);
                    DiffResult diffResult = provider.diff(runId, workspace);
                    logger.info("Provider.diff() returned: success={}, message={}",
                            diffResult.isSuccess(), diffResult.getMessage());
                    return convertToMap(diffResult);

                default:
                    return createErrorResult("Unsupported capability: " + capability);
            }

        } catch (Exception e) {
            logger.error("Error routing tool call: {}", e.getMessage(), e);
            logger.error("Stack trace:", e);
            return createErrorResult("Error executing tool: " + e.getMessage());
        }
    }

    private Workspace createWorkspace(Map<String, Object> arguments) {
        Workspace workspace = new Workspace();
        workspace.setId("default");
        workspace.setPath((String) arguments.get("workspacePath"));
        workspace.setBranch("main");
        return workspace;
    }

    private NqlQuery createNqlQuery(Map<String, Object> arguments) {
        NqlQuery query = new NqlQuery();
        query.setOriginalQuery((String) arguments.getOrDefault("nql", "default query"));
        query.setLanguage((String) arguments.getOrDefault("language", "java"));
        return query;
    }

    private Scope createScope(Map<String, Object> arguments) {
        Scope scope = new Scope();
        String scopePattern = (String) arguments.getOrDefault("scope", "**/*");
        scope.setIncludePatterns(java.util.Arrays.asList(scopePattern));
        return scope;
    }

    private Map<String, Object> convertToMap(Object result) {
        Map<String, Object> map = new HashMap<>();

        logger.info("=== CONVERT TO MAP DEBUG ===");
        logger.info("Input result type: {}", result != null ? result.getClass().getName() : "null");
        logger.info("Input result: {}", result);

        if (result instanceof AnalyzeResult) {
            AnalyzeResult ar = (AnalyzeResult) result;
            logger.info("Converting AnalyzeResult: success={}, message={}", ar.isSuccess(), ar.getMessage());
            map.put("success", ar.isSuccess());
            map.put("message", ar.getMessage());
            map.put("runId", ar.getRunId());
            map.put("data", ar.getData());
            map.put("ast", ar.getAst());
            map.put("symbols", ar.getSymbols());
            map.put("dependencies", ar.getDependencies());
            map.put("type", "analyze");

        } else if (result instanceof MetricsResult) {
            MetricsResult mr = (MetricsResult) result;
            logger.info("Converting MetricsResult: success={}, message={}", mr.isSuccess(), mr.getMessage());
            logger.info("MetricsResult metrics: {}", mr.getMetrics());
            logger.info("MetricsResult details: {}", mr.getDetails());

            map.put("success", mr.isSuccess());
            map.put("message", mr.getMessage());
            map.put("metrics", mr.getMetrics());
            map.put("details", mr.getDetails());
            map.put("type", "metrics");

            logger.info("Final converted map: {}", map);

        } else if (result instanceof PlanResult) {
            PlanResult pr = (PlanResult) result;
            logger.info("Converting PlanResult: success={}, message={}", pr.isSuccess(), pr.getMessage());
            map.put("success", pr.isSuccess());
            map.put("message", pr.getMessage());
            map.put("planId", pr.getPlanId());
            map.put("planContent", pr.getPlanContent());
            map.put("steps", pr.getSteps());
            map.put("type", "plan");

        } else if (result instanceof ApplyResult) {
            ApplyResult ar = (ApplyResult) result;
            logger.info("Converting ApplyResult: success={}, message={}", ar.isSuccess(), ar.getMessage());
            map.put("success", ar.isSuccess());
            map.put("message", ar.getMessage());
            map.put("dryRun", ar.isDryRun());
            map.put("diff", ar.getDiff());
            map.put("changes", ar.getChanges());
            map.put("type", "apply");

        } else if (result instanceof DiffResult) {
            DiffResult dr = (DiffResult) result;
            logger.info("Converting DiffResult: success={}, message={}", dr.isSuccess(), dr.getMessage());
            map.put("success", dr.isSuccess());
            map.put("message", dr.getMessage());
            map.put("unifiedDiff", dr.getUnifiedDiff());
            map.put("semanticDiff", dr.getSemanticDiff());
            map.put("hunks", dr.getHunks());
            map.put("type", "diff");
        } else {
            logger.warn("Unknown result type: {}", result != null ? result.getClass().getName() : "null");
            map.put("success", false);
            map.put("message", "Unknown result type");
            map.put("type", "error");
        }

        logger.info("convertToMap returning: {}", map);
        return map;
    }

    private Map<String, Object> createErrorResult(String errorMessage) {
        Map<String, Object> result = new HashMap<>();
        result.put("success", false);
        result.put("message", errorMessage);
        result.put("type", "error");
        return result;
    }

    private String generateDescription(String language, String capability) {
        return String.format("%s for %s",
                capability.substring(0, 1).toUpperCase() +
                        capability.substring(1).toLowerCase(),
                language);
    }
}
