package org.shark.renovatio.core.service;

import jakarta.annotation.PostConstruct;
import org.shark.renovatio.shared.domain.*;
import org.shark.renovatio.shared.nql.NqlQuery;
import org.shark.renovatio.shared.spi.LanguageProvider;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.stereotype.Service;

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
    // Keys whose values should not be logged verbatim
    private static final Set<String> SENSITIVE_KEYS = Set.of(
            "content", "source", "code", "diff", "unifiedDiff", "semanticDiff", "hunks",
            "patch", "body", "text", "data", "fileContent", "planContent", "steps", "changes", "issues"
    );
    private static final int MAX_STRING_LOG = 120;
    private final Map<String, List<LanguageProvider>> providersByLanguage = new LinkedHashMap<>();
    private final Map<String, LanguageProvider> toolProviders = new ConcurrentHashMap<>();
    @Autowired
    private ApplicationContext applicationContext;

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
        logger.debug("=== ROUTING TOOL CALL ===");
        logger.debug("Tool name: {}", toolName);
        logger.debug("Arguments: {}", redactForLog(arguments, 0));

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

            logger.debug("Found provider for language: {}", language);
            logger.debug("Provider class: {}", provider.getClass().getName());
            logger.debug("Provider capabilities: {}", provider.capabilities());

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

            logger.debug("Created workspace: path={}, id={}", workspace.getPath(), workspace.getId());
            logger.debug("Created query: query={}, language={}", query.getOriginalQuery(), query.getLanguage());
            logger.debug("Created scope: includePatterns={}", scope.getIncludePatterns());

            // Route to appropriate method based on capability
            logger.debug("Routing to capability: {}", capability);
            switch (capabilityKey) {
                case "analyze":
                    query.setType(NqlQuery.QueryType.FIND);
                    logger.debug("Calling provider.analyze()...");
                    AnalyzeResult analyzeResult = provider.analyze(query, workspace);
                    logger.debug("Provider.analyze() returned: success={}, message={}, runId={}",
                            analyzeResult.isSuccess(), analyzeResult.getMessage(), analyzeResult.getRunId());
                    Map<String, Object> analyzeMap = convertToMap(analyzeResult);
                    logger.debug("Converted result: {}", redactForLog(analyzeMap, 0));
                    return analyzeMap;

                case "metrics":
                    logger.debug("Calling provider.metrics()...");
                    MetricsResult metricsResult = provider.metrics(scope, workspace);
                    logger.debug("Provider.metrics() returned: success={}, message={}",
                            metricsResult.isSuccess(), metricsResult.getMessage());
                    Map<String, Object> metricsMap = convertToMap(metricsResult);
                    logger.debug("Converted result: {}", redactForLog(metricsMap, 0));
                    return metricsMap;

                case "plan":
                    query.setType(NqlQuery.QueryType.PLAN);
                    logger.debug("Calling provider.plan()...");
                    PlanResult planResult = provider.plan(query, scope, workspace);
                    logger.debug("Provider.plan() returned: success={}, message={}",
                            planResult.isSuccess(), planResult.getMessage());
                    return convertToMap(planResult);

                case "apply":
                    query.setType(NqlQuery.QueryType.APPLY);
                    String planId = (String) safeArguments.get("planId");
                    boolean dryRun = Boolean.parseBoolean(safeArguments.getOrDefault("dryRun", "true").toString());
                    logger.debug("Calling provider.apply() with planId={}, dryRun={}", planId, dryRun);
                    ApplyResult applyResult = provider.apply(planId, dryRun, workspace);
                    logger.debug("Provider.apply() returned: success={}, message={}",
                            applyResult.isSuccess(), applyResult.getMessage());
                    return convertToMap(applyResult);

                case "diff":
                    String runId = (String) safeArguments.get("runId");
                    logger.debug("Calling provider.diff() with runId={}", runId);
                    DiffResult diffResult = provider.diff(runId, workspace);
                    logger.debug("Provider.diff() returned: success={}, message={}",
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
        Object scopeArg = arguments.get("scope");
        List<String> patterns = new ArrayList<>();
        if (scopeArg instanceof String s) {
            if (s != null && !s.isBlank()) {
                patterns.add(s);
            }
        } else if (scopeArg instanceof List<?> list) {
            for (Object item : list) {
                if (item != null) {
                    String v = item.toString().trim();
                    if (!v.isBlank()) {
                        patterns.add(v);
                    }
                }
            }
        }
        if (patterns.isEmpty()) {
            patterns = java.util.Arrays.asList("**/*");
        }
        scope.setIncludePatterns(patterns);
        return scope;
    }

    private Map<String, Object> convertToMap(Object result) {
        Map<String, Object> map = new HashMap<>();

        logger.debug("=== CONVERT TO MAP DEBUG ===");
        logger.debug("Input result type: {}", result != null ? result.getClass().getName() : "null");
        logger.debug("Input result: {}", result);

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
            logger.debug("Converting AnalyzeResult: success={}, message={}", ar.isSuccess(), ar.getMessage());
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
            logger.debug("Converting MetricsResult: success={}, message={}", mr.isSuccess(), mr.getMessage());
            logger.debug("MetricsResult metrics: {}", redactForLog(mr.getMetrics(), 0));
            logger.debug("MetricsResult details: {}", redactForLog(mr.getDetails(), 0));

            map.put("success", mr.isSuccess());
            map.put("message", mr.getMessage());
            map.put("metrics", mr.getMetrics());
            map.put("details", mr.getDetails());
            map.put("type", "metrics");

            logger.debug("Final converted map: {}", redactForLog(map, 0));

        } else if (result instanceof PlanResult) {
            PlanResult pr = (PlanResult) result;
            logger.debug("Converting PlanResult: success={}, message={}", pr.isSuccess(), pr.getMessage());
            map.put("success", pr.isSuccess());
            map.put("message", pr.getMessage());
            map.put("planId", pr.getPlanId());
            map.put("planContent", pr.getPlanContent());
            map.put("steps", pr.getSteps());
            map.put("type", "plan");

        } else if (result instanceof ApplyResult) {
            ApplyResult ar = (ApplyResult) result;
            logger.debug("Converting ApplyResult: success={}, message={}", ar.isSuccess(), ar.getMessage());
            map.put("success", ar.isSuccess());
            map.put("message", ar.getMessage());
            map.put("dryRun", ar.isDryRun());
            map.put("diff", ar.getDiff());
            map.put("changes", ar.getChanges());
            map.put("type", "apply");

        } else if (result instanceof DiffResult) {
            DiffResult dr = (DiffResult) result;
            logger.debug("Converting DiffResult: success={}, message={}", dr.isSuccess(), dr.getMessage());
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

        logger.debug("convertToMap returning: {}", redactForLog(map, 0));
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

    // Redaction helpers for logging
    @SuppressWarnings("unchecked")
    private Object redactForLog(Object value, int depth) {
        if (value == null) return null;
        if (depth > 3) return "<redacted>";
        if (value instanceof Map<?, ?> m) {
            Map<String, Object> out = new LinkedHashMap<>();
            for (Map.Entry<?, ?> e : m.entrySet()) {
                String k = String.valueOf(e.getKey());
                Object v = e.getValue();
                if (k != null && isSensitiveKey(k)) {
                    out.put(k, summarize(v));
                } else {
                    out.put(k, redactForLog(v, depth + 1));
                }
            }
            return out;
        }
        if (value instanceof List<?> list) {
            List<Object> out = new ArrayList<>(list.size());
            for (Object item : list) {
                out.add(redactForLog(item, depth + 1));
            }
            return out;
        }
        if (value instanceof CharSequence cs) {
            String s = cs.toString();
            return s.length() > MAX_STRING_LOG ? (s.substring(0, MAX_STRING_LOG) + "…") : s;
        }
        return value;
    }

    private boolean isSensitiveKey(String key) {
        String k = key.toLowerCase(Locale.ROOT);
        if (SENSITIVE_KEYS.contains(k)) return true;
        return k.contains("content") || k.contains("code") || k.contains("source") || k.contains("diff") || k.contains("patch") || k.contains("body") || k.contains("text") || k.contains("data");
    }

    private Object summarize(Object v) {
        if (v == null) return "<redacted>";
        if (v instanceof CharSequence cs) {
            return "<redacted:" + cs.length() + " chars>";
        }
        if (v instanceof List<?> list) {
            return "<redacted:list size=" + list.size() + ">";
        }
        if (v instanceof Map<?, ?> map) {
            return "<redacted:map size=" + map.size() + ">";
        }
        return "<redacted>";
    }
}
