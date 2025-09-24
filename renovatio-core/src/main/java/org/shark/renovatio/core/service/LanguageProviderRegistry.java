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
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;

/**
 * Registry for language providers that handles routing tool calls to appropriate providers.
 * This component connects MCP tools to language providers using Spring's auto-configuration.
 */
@Service
public class LanguageProviderRegistry {

    private static final Logger logger = LoggerFactory.getLogger(LanguageProviderRegistry.class);
    private static final Set<String> RESERVED_ARGUMENT_KEYS = Set.of(
            "workspacePath",
            "scope",
            "planId",
            "runId",
            "dryRun",
            "language",
            "nql"
    );

    @Autowired
    private ApplicationContext applicationContext;

    private final Map<String, List<LanguageProvider>> providersByLanguage = new LinkedHashMap<>();
    private final Map<String, LanguageProvider> toolProviders = new ConcurrentHashMap<>();

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
                registerProviderInternal(entry.getValue(), entry.getKey());
            }

            logger.info("LanguageProviderRegistry initialized with {} providers: {}",
                    providersByLanguage.values().stream().mapToInt(List::size).sum(), providersByLanguage.keySet());

        } catch (Exception e) {
            logger.error("Error during LanguageProvider auto-registration: {}", e.getMessage(), e);
        }
    }

    private void registerProviderInternal(LanguageProvider provider, String source) {
        if (provider == null) {
            return;
        }

        String language = Optional.ofNullable(provider.language()).orElse("unknown");
        List<LanguageProvider> languageProviders = providersByLanguage.computeIfAbsent(language, key -> new ArrayList<>());
        languageProviders.add(provider);

        if (languageProviders.size() == 1) {
            logger.info("Registered LanguageProvider [{}] for language '{}' via {} with capabilities: {}",
                    provider.getClass().getSimpleName(), language, source, provider.capabilities());
        } else {
            logger.info("Registered additional LanguageProvider [{}] for language '{}' via {} ({} providers total)",
                    provider.getClass().getSimpleName(), language, source, languageProviders.size());
        }
    }

    /**
     * Manual registration method (kept for compatibility)
     */
    public void registerProvider(LanguageProvider provider) {
        registerProviderInternal(provider, "manual-registration");
    }

    /**
     * Get all registered providers
     */
    public List<LanguageProvider> getAllProviders() {
        return providersByLanguage.values().stream()
                .flatMap(Collection::stream)
                .collect(Collectors.toCollection(ArrayList::new));
    }

    /**
     * Get supported languages
     */
    public Set<String> getSupportedLanguages() {
        return providersByLanguage.keySet();
    }

    /**
     * Get a specific provider by language
     */
    public LanguageProvider getProvider(String language) {
        List<LanguageProvider> languageProviders = providersByLanguage.get(language);
        if (languageProviders == null || languageProviders.isEmpty()) {
            return null;
        }
        return languageProviders.get(0);
    }

    /**
     * Generate a list of all MCP-compliant tools from all registered language providers.
     */
    public List<Tool> generateTools() {
        List<Tool> allTools = new ArrayList<>();
        toolProviders.clear();
        for (Map.Entry<String, List<LanguageProvider>> entry : providersByLanguage.entrySet()) {
            for (LanguageProvider provider : entry.getValue()) {
                List<Tool> tools = provider.getTools();
                if (tools != null) {
                    for (Tool tool : tools) {
                        if (tool == null) {
                            continue;
                        }
                        allTools.add(tool);
                        LanguageProvider previous = toolProviders.put(tool.getName(), provider);
                        if (previous != null && previous != provider) {
                            logger.debug("Tool '{}' was previously provided by {} and is now mapped to {}", tool.getName(),
                                    previous.getClass().getSimpleName(), provider.getClass().getSimpleName());
                        }
                    }
                }
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
            Map<String, Object> safeArguments = arguments != null
                    ? new LinkedHashMap<>(arguments)
                    : new LinkedHashMap<>();

            // Parse tool name to extract language and capability
            int separator = toolName.indexOf('.');
            if (separator < 0 || separator == toolName.length() - 1) {
                return createErrorResult("Invalid tool name format: " + toolName);
            }

            String language = toolName.substring(0, separator);
            String capabilitySection = toolName.substring(separator + 1);

            String normalizedCapability = capabilitySection.replace('.', '_').toLowerCase(Locale.ROOT);

            List<LanguageProvider> languageProviders = providersByLanguage.get(language);
            if (languageProviders == null || languageProviders.isEmpty()) {
                return createErrorResult("No provider found for language: " + language);
            }

            LanguageProvider provider = resolveProvider(languageProviders, toolName, normalizedCapability);
            if (provider == null) {
                return createErrorResult("Unsupported capability '" + capabilitySection + "' for language: " + language);
            }

            logger.info("Found provider for language: {}", language);
            logger.info("Provider class: {}", provider.getClass().getName());
            logger.info("Provider capabilities: {}", provider.capabilities());

            if (provider instanceof org.shark.renovatio.shared.spi.ExtendedLanguageProvider extendedProvider) {
                Map<String, Object> extendedResult = extendedProvider.executeExtendedTool(normalizedCapability, safeArguments);
                if (extendedResult != null) {
                    return extendedResult;
                }
            }

            String capability = capabilitySection;
            String recipeId = null;

            int dotInCapability = capability.indexOf('.');
            if (dotInCapability > 0) {
                recipeId = capability.substring(dotInCapability + 1);
                capability = capability.substring(0, dotInCapability);
            } else {
                int underscoreInCapability = capability.indexOf('_');
                if (underscoreInCapability > 0
                    && shouldSplitRecipeIdentifier(capability.substring(0, underscoreInCapability))) {
                    recipeId = capability.substring(underscoreInCapability + 1);
                    capability = capability.substring(0, underscoreInCapability);
                }
            }

            if (recipeId != null && !recipeId.isEmpty()) {
                safeArguments.putIfAbsent("recipeId", recipeId);
                if (arguments != null) {
                    arguments.putIfAbsent("recipeId", recipeId);
                }
            }

            String capabilityKey = capability.toLowerCase(Locale.ROOT);

            // Create workspace and query objects
            Workspace workspace = createWorkspace(safeArguments);
            NqlQuery query = createNqlQuery(safeArguments, language);
            Scope scope = createScope(safeArguments);

            logger.info("Created workspace: path={}, id={}", workspace.getPath(), workspace.getId());
            logger.info("Created query: query={}, language={}", query.getOriginalQuery(), query.getLanguage());
            logger.info("Created scope: includePatterns={}", scope.getIncludePatterns());

            // Route to appropriate method based on capability
            logger.info("Routing to capability: {}", capability);
            switch (capabilityKey) {
                case "analyze":
                    query.setType(NqlQuery.QueryType.FIND);
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
                    query.setType(NqlQuery.QueryType.PLAN);
                    logger.info("Calling provider.plan()...");
                    PlanResult planResult = provider.plan(query, scope, workspace);
                    logger.info("Provider.plan() returned: success={}, message={}",
                            planResult.isSuccess(), planResult.getMessage());
                    return convertToMap(planResult);

                case "apply":
                    query.setType(NqlQuery.QueryType.APPLY);
                    String planId = (String) safeArguments.get("planId");
                    boolean dryRun = Boolean.parseBoolean(safeArguments.getOrDefault("dryRun", "true").toString());
                    logger.info("Calling provider.apply() with planId={}, dryRun={}", planId, dryRun);
                    ApplyResult applyResult = provider.apply(planId, dryRun, workspace);
                    logger.info("Provider.apply() returned: success={}, message={}",
                            applyResult.isSuccess(), applyResult.getMessage());
                    return convertToMap(applyResult);

                case "diff":
                    String runId = (String) safeArguments.get("runId");
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

    private LanguageProvider resolveProvider(List<LanguageProvider> candidates, String toolName, String capabilityKey) {
        if (candidates == null || candidates.isEmpty()) {
            return null;
        }

        LanguageProvider direct = toolProviders.get(toolName);
        if (direct != null && candidates.contains(direct)) {
            return direct;
        }

        LanguageProvider.Capabilities capability = toCapability(capabilityKey);
        if (capability == null) {
            for (LanguageProvider provider : candidates) {
                if (provider instanceof org.shark.renovatio.shared.spi.ExtendedLanguageProvider) {
                    return provider;
                }
            }
            return candidates.get(0);
        }

        for (LanguageProvider provider : candidates) {
            Set<LanguageProvider.Capabilities> providerCapabilities = provider.capabilities();
            if (providerCapabilities == null) {
                continue;
            }
            for (LanguageProvider.Capabilities candidateCapability : providerCapabilities) {
                if (candidateCapability == capability || candidateCapability.name().equalsIgnoreCase(capabilityKey)) {
                    return provider;
                }
            }
        }

        return null;
    }

    private boolean shouldSplitRecipeIdentifier(String capabilityPrefix) {
        if (capabilityPrefix == null || capabilityPrefix.isBlank()) {
            return false;
        }
        String normalized = capabilityPrefix.toLowerCase(Locale.ROOT);
        return normalized.equals("apply") || normalized.equals("plan") || normalized.equals("analyze");
    }

    private LanguageProvider.Capabilities toCapability(String capabilityKey) {
        if (capabilityKey == null || capabilityKey.isBlank()) {
            return null;
        }
        try {
            return LanguageProvider.Capabilities.valueOf(capabilityKey.toUpperCase(Locale.ROOT));
        } catch (IllegalArgumentException ex) {
            logger.warn("Unknown capability key '{}': {}", capabilityKey, ex.getMessage());
            return null;
        }
    }

    private Workspace createWorkspace(Map<String, Object> arguments) {
        Workspace workspace = new Workspace();
        workspace.setId("default");
        workspace.setPath((String) arguments.get("workspacePath"));
        workspace.setBranch("main");
        return workspace;
    }

    private NqlQuery createNqlQuery(Map<String, Object> arguments, String language) {
        NqlQuery query = new NqlQuery();
        query.setOriginalQuery((String) arguments.getOrDefault("nql", "default query"));
        if (language != null && !language.isBlank()) {
            query.setLanguage(language);
        } else {
            query.setLanguage((String) arguments.getOrDefault("language", "java"));
        }

        Map<String, Object> parameters = new LinkedHashMap<>();
        for (Map.Entry<String, Object> entry : arguments.entrySet()) {
            if (!RESERVED_ARGUMENT_KEYS.contains(entry.getKey())) {
                parameters.put(entry.getKey(), entry.getValue());
            }
        }
        if (!parameters.isEmpty()) {
            query.setParameters(parameters);
        }
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

        if (result instanceof ProviderResult providerResult) {
            map.put("success", providerResult.isSuccess());
            map.put("message", providerResult.getMessage());
            map.put("runId", providerResult.getRunId());
            map.put("timestamp", providerResult.getTimestamp());
            if (providerResult.getMetadata() != null && !providerResult.getMetadata().isEmpty()) {
                map.put("metadata", providerResult.getMetadata());
            }
        }

        if (result instanceof AnalyzeResult) {
            AnalyzeResult ar = (AnalyzeResult) result;
            logger.info("Converting AnalyzeResult: success={}, message={}", ar.isSuccess(), ar.getMessage());
            map.put("success", ar.isSuccess());
            map.put("message", ar.getMessage());
            map.put("runId", ar.getRunId());

            Map<String, Object> data = ar.getData() != null
                ? new LinkedHashMap<>(ar.getData())
                : new LinkedHashMap<>();
            map.put("data", data);

            map.put("ast", ar.getAst());
            map.put("symbols", ar.getSymbols());
            map.put("dependencies", ar.getDependencies());
            map.put("type", "analyze");

            map.put("summary", data.getOrDefault("summary", ar.getMessage()));
            map.put("issues", data.getOrDefault("issues", Collections.emptyList()));
            map.put("metrics", data.getOrDefault("metrics", Collections.emptyMap()));
            map.put("diffs", data.getOrDefault("diffs", Collections.emptyList()));
            map.put("analyzedFiles", data.getOrDefault("analyzedFiles", Collections.emptyList()));
            if (data.containsKey("applied")) {
                map.put("applied", data.get("applied"));
            }

            if (ar.getPerformance() != null) {
                Map<String, Object> performance = new HashMap<>();
                performance.put("executionTimeMs", ar.getPerformance().getExecutionTimeMs());
                map.put("performance", performance);
            }

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
