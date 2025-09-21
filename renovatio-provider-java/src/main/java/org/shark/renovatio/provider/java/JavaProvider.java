package org.shark.renovatio.provider.java;

import org.shark.renovatio.shared.spi.BaseLanguageProvider;
import org.shark.renovatio.shared.domain.*;
import org.shark.renovatio.shared.nql.NqlQuery;
import org.openrewrite.config.Environment;
import org.openrewrite.config.OptionDescriptor;
import org.openrewrite.config.RecipeDescriptor;


import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Optional;
import java.util.Properties;
import java.util.Set;

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
        List<Tool> recipeTools = new ArrayList<>();

        try {
            ClassLoader cl = Thread.currentThread().getContextClassLoader();
            if (cl == null) {
                cl = JavaProvider.class.getClassLoader();
            }

            // OpenRewrite 8.x+: solo build() para cargar recetas del classpath y rewrite.yml
            Environment env = Environment.builder().build();
            Collection<RecipeDescriptor> descriptors = env.listRecipeDescriptors();
            Set<String> seen = new LinkedHashSet<>();
            for (RecipeDescriptor descriptor : descriptors) {
                collectRecipeTools(descriptor, recipeTools, seen);
            }
        } catch (Exception e) {
            System.err.println("[WARN] Unable to discover OpenRewrite recipes: " + e.getMessage());
            e.printStackTrace(System.err);
        }

        if (recipeTools.isEmpty()) {
            return List.of(createFallbackAnalyzeTool());
        }

        List<Tool> tools = new ArrayList<>();
        tools.add(createAnalyzeTool());
        tools.addAll(recipeTools);
        return tools;
    }

    private void collectRecipeTools(RecipeDescriptor descriptor, List<Tool> tools, Set<String> seen) {
        if (descriptor == null) {
            return;
        }

        String recipeName = descriptor.getName();
        if (recipeName == null || recipeName.isBlank() || !seen.add(recipeName)) {
            // Skip empty names or already processed recipes
            return;
        }

        BasicTool tool = createRecipeTool(descriptor);
        if (tool != null) {
            tools.add(tool);
        }

        Collection<RecipeDescriptor> nested = descriptor.getRecipeList();
        if (nested != null) {
            for (RecipeDescriptor child : nested) {
                collectRecipeTools(child, tools, seen);
            }
        }
    }

    private BasicTool createRecipeTool(RecipeDescriptor descriptor) {
        String recipeName = descriptor.getName();
        if (recipeName == null || recipeName.isBlank()) {
            return null;
        }

        String slug = toRecipeSlug(recipeName);
        String toolName = "java.apply_" + slug;

        String description = descriptor.getDescription();
        if (description == null || description.isBlank()) {
            description = descriptor.getDisplayName();
        }
        if (description == null || description.isBlank()) {
            description = recipeName;
        }

        Map<String, Object> properties = new LinkedHashMap<>();
        Map<String, Object> workspaceProperty = new LinkedHashMap<>();
        workspaceProperty.put("description", "Path to the workspace directory where the recipe will be executed");
        workspaceProperty.put("type", "string");
        properties.put("workspacePath", workspaceProperty);

        List<String> required = new ArrayList<>();
        required.add("workspacePath");

        Map<String, Object> example = new LinkedHashMap<>();
        example.put("workspacePath", "/path/to/workspace");

        List<Map<String, Object>> parameters = new ArrayList<>();
        parameters.add(createParameter(
            "workspacePath",
            "Path to the workspace directory where the recipe will be executed",
            "string",
            true,
            null
        ));

        List<OptionDescriptor> options = descriptor.getOptions();
        if (options != null) {
            for (OptionDescriptor option : options) {
                String optionType = mapOptionType(option.getType());
                Map<String, Object> property = new LinkedHashMap<>();
                String optionDescription = option.getDescription();
                if (optionDescription != null && !optionDescription.isBlank()) {
                    property.put("description", optionDescription);
                }
                property.put("type", optionType);
                Object optionExample = option.getExample();
                if (optionExample != null) {
                    property.put("example", optionExample);
                    example.put(option.getName(), optionExample);
                }
                properties.put(option.getName(), property);
                if (option.isRequired()) {
                    required.add(option.getName());
                }
                parameters.add(createParameter(option.getName(), optionDescription, optionType, option.isRequired(), optionExample));
            }
        }

        Map<String, Object> schema = new LinkedHashMap<>();
        schema.put("type", "object");
        schema.put("properties", properties);
        schema.put("required", required);
        schema.put("example", example);

        BasicTool tool = new BasicTool(toolName, description, schema);
        tool.getMetadata().put("recipeName", recipeName);
        String displayName = descriptor.getDisplayName();
        if (displayName == null || displayName.isBlank()) {
            displayName = description;
        }
        tool.getMetadata().put("displayName", displayName);
        tool.getMetadata().put("parameters", parameters);
        tool.getMetadata().put("example", example);
        tool.getMetadata().put("capability", "apply");
        tool.getMetadata().put("workflowPhase", "refactor");
        tool.getMetadata().put("language", language());
        if (descriptor.getTags() != null && !descriptor.getTags().isEmpty()) {
            tool.getMetadata().put("tags", new ArrayList<>(descriptor.getTags()));
        }
        return tool;
    }

    private BasicTool createAnalyzeTool() {
        return createWorkspaceTool(
            "java.analyze",
            "Analyze Java code structure and dependencies using Renovatio's Java provider"
        );
    }

    private BasicTool createFallbackAnalyzeTool() {
        return createWorkspaceTool(
            "java.analyze",
            "Analyze Java code structure when OpenRewrite recipes are not available"
        );
    }

    private BasicTool createWorkspaceTool(String name, String description) {
        Map<String, Object> workspaceProperty = new LinkedHashMap<>();
        workspaceProperty.put("description", "Path to the workspace directory to analyze");
        workspaceProperty.put("type", "string");

        Map<String, Object> properties = new LinkedHashMap<>();
        properties.put("workspacePath", workspaceProperty);

        List<String> required = List.of("workspacePath");
        Map<String, Object> example = Map.of("workspacePath", "/path/to/workspace");

        Map<String, Object> schema = new LinkedHashMap<>();
        schema.put("type", "object");
        schema.put("properties", properties);
        schema.put("required", required);
        schema.put("example", example);

        BasicTool tool = new BasicTool(name, description, schema);
        List<Map<String, Object>> parameters = new ArrayList<>();
        parameters.add(createParameter(
            "workspacePath",
            "Path to the workspace directory to analyze",
            "string",
            true,
            null
        ));
        tool.getMetadata().put("parameters", parameters);
        tool.getMetadata().put("example", example);
        tool.getMetadata().put("capability", name.substring(name.indexOf('.') + 1));
        tool.getMetadata().put("workflowPhase", name.contains("metrics") ? "baseline" : "analysis");
        tool.getMetadata().put("language", language());
        tool.getMetadata().put("displayName", description);
        return tool;
    }

    private Map<String, Object> createParameter(String name, String description, String type, boolean required, Object example) {
        Map<String, Object> parameter = new LinkedHashMap<>();
        parameter.put("name", name);
        parameter.put("description", description != null ? description : "");
        parameter.put("type", type != null ? type : "string");
        parameter.put("required", required);
        if (example != null) {
            parameter.put("example", example);
        }
        return parameter;
    }

    private String mapOptionType(String optionType) {
        if (optionType == null || optionType.isBlank()) {
            return "string";
        }

        String normalized = optionType.toLowerCase(Locale.ROOT);
        if (normalized.contains("boolean")) {
            return "boolean";
        }
        if (normalized.contains("int") || normalized.contains("long") || normalized.contains("short")
            || normalized.contains("byte")) {
            return "integer";
        }
        if (normalized.contains("double") || normalized.contains("float") || normalized.contains("bigdecimal")
            || normalized.contains("number")) {
            return "number";
        }
        if (normalized.contains("list") || normalized.contains("set") || normalized.contains("collection")
            || normalized.contains("array")) {
            return "array";
        }
        return "string";
    }

    private String toRecipeSlug(String recipeName) {
        if (recipeName == null || recipeName.isBlank()) {
            return "recipe";
        }

        StringBuilder slug = new StringBuilder();
        char previous = '\0';
        for (int index = 0; index < recipeName.length(); index++) {
            char current = recipeName.charAt(index);

            if (Character.isLetterOrDigit(current)) {
                if (Character.isUpperCase(current)) {
                    boolean needsSeparator = slug.length() > 0;
                    if (needsSeparator) {
                        char lastAppended = slug.charAt(slug.length() - 1);
                        if (lastAppended != '_') {
                            if (Character.isLowerCase(previous) || Character.isDigit(previous)) {
                                slug.append('_');
                            } else if (Character.isUpperCase(previous)) {
                                if (index + 1 < recipeName.length()) {
                                    char next = recipeName.charAt(index + 1);
                                    if (Character.isLowerCase(next)) {
                                        slug.append('_');
                                    }
                                }
                            }
                        }
                    }
                    slug.append(Character.toLowerCase(current));
                } else {
                    slug.append(current);
                }
                previous = current;
            } else {
                if (slug.length() > 0 && slug.charAt(slug.length() - 1) != '_') {
                    slug.append('_');
                }
                previous = current;
            }
        }

        while (slug.length() > 0 && slug.charAt(slug.length() - 1) == '_') {
            slug.deleteCharAt(slug.length() - 1);
        }

        if (slug.length() == 0) {
            return "recipe";
        }

        return slug.toString().toLowerCase(Locale.ROOT);
    }

}
