package org.shark.renovatio.provider.java.planner;

import org.shark.renovatio.provider.java.discovery.OpenRewriteRecipeDiscoveryService;
import org.shark.renovatio.provider.java.discovery.OpenRewriteRecipeDiscoveryService.RecipeInfo;
import org.shark.renovatio.provider.java.discovery.OpenRewriteRecipeDiscoveryService.RecipeOption;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;

import java.time.Instant;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Planner that converts high-level refactoring goals into a concrete list of recipes
 * and ordered steps.
 */
@Service
public class JavaRefactorPlanner {

    private static final Logger LOGGER = LoggerFactory.getLogger(JavaRefactorPlanner.class);
    private final OpenRewriteRecipeDiscoveryService discoveryService;
    private final Map<String, JavaPlan> plans = new ConcurrentHashMap<>();

    public JavaRefactorPlanner(OpenRewriteRecipeDiscoveryService discoveryService) {
        this.discoveryService = discoveryService;
    }

    public JavaPlan createPlan(String workspacePath,
                               List<String> goals,
                               List<String> includeRecipes,
                               List<String> excludeRecipes,
                               List<String> scope) {
        LinkedHashSet<String> selected = new LinkedHashSet<>();
        Map<String, List<String>> profiles = discoveryService.profilesToRecipes();

        if (goals != null) {
            for (String goal : goals) {
                if (goal == null || goal.isBlank()) {
                    continue;
                }
                String normalized = goal.toLowerCase(Locale.ROOT);
                selected.addAll(profiles.getOrDefault(normalized, profiles.getOrDefault(goal, List.of())));
            }
        }

        if (includeRecipes != null) {
            for (String recipe : includeRecipes) {
                if (recipe != null && !recipe.isBlank()) {
                    // Apply safety filter to ensure we don't execute recipes that require specific configuration
                    if (isRecipeSafeToExecute(recipe)) {
                        selected.add(recipe);
                    } else {
                        LOGGER.warn("Skipping recipe '{}' - requires specific configuration parameters that are not provided", recipe);
                    }
                }
            }
        }

        if (selected.isEmpty()) {
            selected.addAll(profiles.getOrDefault("quality", List.of()));
        }

        if (excludeRecipes != null) {
            for (String recipe : excludeRecipes) {
                if (recipe != null) {
                    selected.remove(recipe);
                }
            }
        }

        List<JavaPlanStep> steps = new ArrayList<>();
        int index = 1;
        for (String recipe : selected) {
            steps.add(new JavaPlanStep(
                "step-" + index++,
                recipe,
                "Apply OpenRewrite recipe " + recipe
            ));
        }

        JavaPlan plan = new JavaPlan(
            generatePlanId(),
            workspacePath,
            goals != null ? List.copyOf(goals) : List.of(),
            List.copyOf(selected),
            scope != null ? List.copyOf(scope) : List.of(),
            List.copyOf(steps),
            Instant.now()
        );
        plans.put(plan.id(), plan);
        return plan;
    }

    public Optional<JavaPlan> findPlan(String planId) {
        return Optional.ofNullable(plans.get(planId));
    }

    public Map<String, List<String>> profileCatalog() {
        return new LinkedHashMap<>(discoveryService.profilesToRecipes());
    }

    public List<String> resolveRecipes(List<String> goals,
                                       List<String> includeRecipes,
                                       List<String> excludeRecipes) {
        return createPlan(null, goals, includeRecipes, excludeRecipes, List.of()).recipes();
    }

    public List<Map<String, Object>> describePlanSteps(JavaPlan plan) {
        List<Map<String, Object>> result = new ArrayList<>();
        for (JavaPlanStep step : plan.steps()) {
            result.add(step.toMap());
        }
        return result;
    }

    public Map<String, Object> describeRecipes(JavaPlan plan) {
        Map<String, Object> description = new LinkedHashMap<>();
        for (String recipe : plan.recipes()) {
            Optional<RecipeInfo> info = discoveryService.describeRecipe(recipe);
            info.ifPresent(recipeInfo -> description.put(recipe, toDescription(recipeInfo)));
        }
        return description;
    }

    private Map<String, Object> toDescription(RecipeInfo info) {
        Map<String, Object> map = new LinkedHashMap<>();
        map.put("displayName", info.displayName());
        map.put("description", info.description());
        map.put("tags", info.tags());
        map.put("options", info.options());
        return map;
    }

    private String generatePlanId() {
        return "java-plan-" + UUID.randomUUID();
    }

    /**
     * Check if a recipe is safe to execute without additional configuration.
     * Some recipes require specific parameters that may not be set and can cause NPE.
     * This method duplicates the logic from OpenRewriteRecipeDiscoveryService to ensure
     * consistent filtering.
     */
    private boolean isRecipeSafeToExecute(String recipeName) {
        if (recipeName == null || recipeName.isBlank()) {
            return false;
        }
        
        // Filter out recipes that typically require specific configuration
        // These recipes often fail with NPE when required parameters are not set
        Set<String> problematicRecipes = Set.of(
            "org.openrewrite.java.CreateEmptyJavaClass",
            "org.openrewrite.yaml.CreateYamlFile", 
            "org.openrewrite.text.CreateTextFile",
            "org.openrewrite.xml.CreateXmlFile",
            "org.openrewrite.RenameFile",
            "org.openrewrite.java.ChangePackage",
            "org.openrewrite.java.ChangeType",
            "org.openrewrite.java.ChangeFieldType",
            "org.openrewrite.java.ChangeFieldName",
            "org.openrewrite.java.ChangeMethodName",
            "org.openrewrite.java.ChangeStaticFieldToMethod",
            "org.openrewrite.java.AddImport",
            "org.openrewrite.java.RemoveImport",
            "org.openrewrite.java.ReplaceStringLiteral",
            "org.openrewrite.java.search.FindMethods",
            "org.openrewrite.java.search.FindFields",
            "org.openrewrite.java.search.FindTypes",
            "org.openrewrite.java.dependencies.AddDependency",
            "org.openrewrite.java.dependencies.RemoveDependency",
            "org.openrewrite.java.dependencies.ChangeDependency"
        );
        
        if (problematicRecipes.contains(recipeName)) {
            return false;
        }
        
        // Check for recipe patterns that typically require configuration
        if (recipeName.toLowerCase(Locale.ROOT).contains("create") && 
            (recipeName.contains("Class") || recipeName.contains("File"))) {
            return false;
        }
        
        if (recipeName.toLowerCase(Locale.ROOT).contains("change") &&
            (recipeName.contains("Type") || recipeName.contains("Method") || recipeName.contains("Field"))) {
            return false;
        }
        
        // Also filter recipes that are known to require parameters
        Optional<RecipeInfo> recipeInfo = discoveryService.describeRecipe(recipeName);
        if (recipeInfo.isPresent() && hasRequiredOptions(recipeInfo.get())) {
            return false;
        }
        
        return true;
    }
    
    /**
     * Check if a recipe has options that are typically required for execution.
     */
    private boolean hasRequiredOptions(RecipeInfo info) {
        if (info.options() == null || info.options().isEmpty()) {
            return false;
        }
        
        // Check for common parameter patterns that usually indicate required configuration
        for (RecipeOption option : info.options()) {
            if (option.name() == null) {
                continue;
            }
            
            String optionName = option.name().toLowerCase(Locale.ROOT);
            // These are typically required parameters that cause NPE if null
            if (optionName.contains("packagename") || 
                optionName.contains("classname") ||
                optionName.contains("filepath") ||
                optionName.contains("filename") ||
                optionName.contains("path") ||
                optionName.contains("methodname") ||
                optionName.contains("fieldname") ||
                optionName.contains("typename") ||
                optionName.contains("oldname") ||
                optionName.contains("newname") ||
                optionName.contains("oldtype") ||
                optionName.contains("newtype") ||
                optionName.contains("groupid") ||
                optionName.contains("artifactid") ||
                optionName.contains("version") ||
                // Common OpenRewrite option patterns
                optionName.equals("type") ||
                optionName.equals("name") ||
                optionName.equals("target") ||
                optionName.equals("source")) {
                
                // Check if the option has a default value - if not, it's likely required
                if (option.defaultValue() == null || 
                    (option.defaultValue() instanceof String && ((String) option.defaultValue()).isEmpty())) {
                    return true;
                }
            }
        }
        
        return false;
    }
}
