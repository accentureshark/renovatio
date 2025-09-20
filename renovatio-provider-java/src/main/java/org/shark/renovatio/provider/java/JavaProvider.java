package org.shark.renovatio.provider.java;

import org.shark.renovatio.shared.spi.BaseLanguageProvider;
import org.shark.renovatio.shared.domain.*;
import org.shark.renovatio.shared.nql.NqlQuery;
import org.openrewrite.config.Environment;
import org.openrewrite.Recipe;
import org.openrewrite.config.OptionDescriptor;
import org.openrewrite.yaml.YamlResourceLoader;

import java.io.File;
import java.util.*;

/**
 * Java language provider implementation using OpenRewrite
 */
public class JavaProvider extends BaseLanguageProvider {
    
    @Override
    public String language() {
        return "java";
    }
    
    @Override
    public Set<Capabilities> capabilities() {
        return Set.of(
            Capabilities.ANALYZE,
            Capabilities.PLAN,
            Capabilities.APPLY,
            Capabilities.DIFF,
            Capabilities.METRICS
        );
    }
    
    @Override
    public AnalyzeResult analyze(NqlQuery query, Workspace workspace) {
        AnalyzeResult result = new AnalyzeResult(true, "Java analysis completed");
        result.setRunId(generateRunId());
        
        // Placeholder implementation - in real implementation would use OpenRewrite
        Map<String, Object> ast = new HashMap<>();
        ast.put("language", "java");
        ast.put("classes", Arrays.asList("ExampleClass", "AnotherClass"));
        result.setAst(ast);
        
        Map<String, Object> dependencies = new HashMap<>();
        dependencies.put("imports", Arrays.asList("java.util.List", "org.springframework.stereotype.Service"));
        result.setDependencies(dependencies);
        
        return result;
    }
    
    @Override
    public PlanResult plan(NqlQuery query, Scope scope, Workspace workspace) {
        PlanResult result = new PlanResult(true, "Java execution plan created");
        String planId = generatePlanId();
        result.setPlanId(planId);
        result.setRunId(generateRunId());
        
        // Placeholder implementation - would use OpenRewrite to create recipe plans
        String planContent = String.format(
            "Java Plan for query: %s\n" +
            "Target: %s\n" +
            "Predicate: %s\n" +
            "Scope: %s\n",
            query.getOriginalQuery(),
            query.getTarget(),
            query.getPredicate(),
            query.getScope()
        );
        result.setPlanContent(planContent);
        
        Map<String, Object> steps = new HashMap<>();
        steps.put("step1", "Parse Java sources");
        steps.put("step2", "Apply OpenRewrite recipes");
        steps.put("step3", "Generate diffs");
        result.setSteps(steps);
        
        return result;
    }
    
    @Override
    public ApplyResult apply(String planId, boolean dryRun, Workspace workspace) {
        ApplyResult result = new ApplyResult(true, "Java transformations applied");
        result.setRunId(generateRunId());
        result.setDryRun(dryRun);
        
        // Placeholder implementation - would execute OpenRewrite recipes
        String diff = createSampleDiff();
        result.setDiff(diff);
        
        Map<String, Object> changes = new HashMap<>();
        changes.put("filesModified", 3);
        changes.put("linesAdded", 15);
        changes.put("linesRemoved", 8);
        result.setChanges(changes);
        
        return result;
    }
    
    @Override
    public DiffResult diff(String runId, Workspace workspace) {
        DiffResult result = new DiffResult(true, "Java diff generated");
        result.setRunId(runId);
        
        String unifiedDiff = createSampleDiff();
        result.setUnifiedDiff(unifiedDiff);
        
        Map<String, Object> semanticDiff = new HashMap<>();
        semanticDiff.put("methodsAdded", 2);
        semanticDiff.put("methodsRemoved", 1);
        semanticDiff.put("importsChanged", 3);
        result.setSemanticDiff(semanticDiff);
        
        return result;
    }
    
    @Override
    public Optional<StubResult> generateStubs(NqlQuery query, Workspace workspace) {
        // Java provider doesn't typically generate stubs for other languages
        return Optional.empty();
    }
    
    @Override
    public MetricsResult metrics(Scope scope, Workspace workspace) {
        MetricsResult result = new MetricsResult(true, "Java metrics calculated");
        result.setRunId(generateRunId());
        
        Map<String, Number> metrics = new HashMap<>();
        metrics.put("linesOfCode", 1250);
        metrics.put("cyclomaticComplexity", 8.5);
        metrics.put("numberOfClasses", 15);
        metrics.put("numberOfMethods", 87);
        metrics.put("testCoverage", 0.78);
        result.setMetrics(metrics);
        
        Map<String, Object> details = new HashMap<>();
        details.put("topComplexMethods", Arrays.asList("processData", "validateInput", "transformResult"));
        details.put("duplicatedBlocks", 3);
        result.setDetails(details);
        
        return result;
    }
    
    @Override
    public java.util.List<Tool> getTools() {
        List<Tool> tools = new ArrayList<>();
        try {
            // Use the current thread's classloader for deep recipe discovery
            ClassLoader cl = Thread.currentThread().getContextClassLoader();
            Environment.Builder builder = Environment.builder(cl)
                .scanClasspath("org.openrewrite")
                .scanClasspath("org.openrewrite.recipe");
            File rewriteYml = new File("rewrite.yml");
            if (rewriteYml.exists()) {
                try (java.io.InputStream is = java.nio.file.Files.newInputStream(rewriteYml.toPath())) {
                    builder.load(new YamlResourceLoader(is, rewriteYml.toURI(), new Properties()));
                }
            }
            Environment env = builder.build();
            Collection<Recipe> recipes = env.listRecipes();
            System.out.println("[DEBUG] OpenRewrite discovered " + recipes.size() + " recipes:");
            for (Recipe recipe : recipes) {
                System.out.println("[DEBUG] - " + recipe.getName() + ": " + recipe.getDisplayName());
            }
            for (Recipe recipe : recipes) {
                String name = recipe.getName();
                String displayName = recipe.getDisplayName() != null ? recipe.getDisplayName() : name;
                String description = recipe.getDescription() != null ? recipe.getDescription() : displayName;
                // Try to get options/parameters if available
                List<Map<String, Object>> parameters = new ArrayList<>();
                Map<String, Object> properties = new LinkedHashMap<>();
                List<String> required = new ArrayList<>();
                Map<String, Object> example = new LinkedHashMap<>();
                try {
                    var optionDescriptorsMethod = recipe.getClass().getMethod("getOptionDescriptors");
                    @SuppressWarnings("unchecked")
                    List<Object> optionDescriptors = (List<Object>) optionDescriptorsMethod.invoke(recipe);
                    for (Object opt : optionDescriptors) {
                        var getName = opt.getClass().getMethod("getName");
                        var getDescription = opt.getClass().getMethod("getDescription");
                        var getType = opt.getClass().getMethod("getType");
                        var isRequired = opt.getClass().getMethod("isRequired");
                        var getExample = opt.getClass().getMethod("getExample");
                        String optName = (String) getName.invoke(opt);
                        String optDesc = (String) getDescription.invoke(opt);
                        String optType = (String) getType.invoke(opt);
                        boolean optReq = (boolean) isRequired.invoke(opt);
                        Object optExample = getExample.invoke(opt);
                        Map<String, Object> param = new LinkedHashMap<>();
                        param.put("name", optName);
                        param.put("description", optDesc != null ? optDesc : "");
                        param.put("type", optType);
                        param.put("required", optReq);
                        if (optExample != null) {
                            param.put("example", optExample);
                            example.put(optName, optExample);
                        }
                        parameters.add(param);
                        Map<String, Object> prop = new LinkedHashMap<>();
                        prop.put("description", optDesc != null ? optDesc : "");
                        prop.put("type", optType);
                        if (optExample != null) {
                            prop.put("example", optExample);
                        }
                        properties.put(optName, prop);
                        if (optReq) {
                            required.add(optName);
                        }
                    }
                } catch (Exception ignore) {
                    // No options available for this recipe
                }
                Map<String, Object> inputSchema = Map.of(
                    "type", "object",
                    "properties", properties,
                    "required", required,
                    "example", example
                );
                BasicTool tool = new BasicTool(
                    "java_" + name,
                    description,
                    inputSchema
                );
                tool.getMetadata().put("parameters", parameters);
                tool.getMetadata().put("example", example);
                tools.add(tool);
            }
        } catch (Exception e) {
            // Fallback: expose only a default tool if OpenRewrite is not available
            BasicTool fallback = new BasicTool(
                "java_analyze",
                "Analyze for java",
                Map.of(
                    "type", "object",
                    "properties", Map.of(
                        "workspacePath", Map.of(
                            "description", "Path to the workspace directory to analyze",
                            "type", "string"
                        )
                    ),
                    "required", List.of("workspacePath"),
                    "example", Map.of("workspacePath", "/path/to/workspace")
                )
            );
            fallback.getMetadata().put("parameters", List.of(
                Map.of(
                    "name", "workspacePath",
                    "description", "Path to the workspace directory to analyze",
                    "type", "string",
                    "required", true
                )
            ));
            fallback.getMetadata().put("example", Map.of("workspacePath", "/path/to/workspace"));
            tools.add(fallback);
        }
        return tools;
    }

}