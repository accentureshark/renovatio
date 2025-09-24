package org.shark.renovatio.provider.java.planner;

import org.shark.renovatio.provider.java.discovery.OpenRewriteRecipeDiscoveryService;
import org.shark.renovatio.provider.java.discovery.OpenRewriteRecipeDiscoveryService.RecipeInfo;
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
                    String trimmed = recipe.trim();
                    if (trimmed.isEmpty()) {
                        continue;
                    }
                    // Apply safety filter to ensure we don't execute recipes that require specific configuration
                    if (discoveryService.isRecipeSafe(trimmed)) {
                        selected.add(trimmed);
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
}
