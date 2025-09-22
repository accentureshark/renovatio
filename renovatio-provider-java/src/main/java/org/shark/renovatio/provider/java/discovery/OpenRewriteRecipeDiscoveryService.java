package org.shark.renovatio.provider.java.discovery;

import org.openrewrite.Recipe;
import org.openrewrite.config.Environment;
import org.openrewrite.config.OptionDescriptor;
import org.openrewrite.config.RecipeDescriptor;
import org.openrewrite.config.YamlResourceLoader;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;

import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Properties;
import java.util.Queue;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;

/**
 * Service responsible for discovering OpenRewrite recipes available on the
 * classpath and exposing metadata used by MCP tools.
 */
@Service
public class OpenRewriteRecipeDiscoveryService {

    private static final Logger LOGGER = LoggerFactory.getLogger(OpenRewriteRecipeDiscoveryService.class);

    private final Environment environment;
    private final Map<String, RecipeInfo> recipes;
    private final Map<String, List<String>> profiles;

    public OpenRewriteRecipeDiscoveryService() {
        this.environment = loadEnvironment();
        this.recipes = discoverRecipes(environment);
        this.profiles = buildProfiles(this.recipes);
        LOGGER.info("Discovered {} OpenRewrite recipes", recipes.size());
    }

    public Environment getEnvironment() {
        return environment;
    }

    public List<RecipeInfo> listAllRecipes() {
        return new ArrayList<>(recipes.values());
    }

    public Optional<RecipeInfo> describeRecipe(String name) {
        if (name == null || name.isBlank()) {
            return Optional.empty();
        }
        return Optional.ofNullable(recipes.get(name));
    }

    public Map<String, List<String>> profilesToRecipes() {
        return profiles;
    }

    public Recipe buildCompositeRecipe(Collection<String> recipeNames) {
        if (recipeNames == null || recipeNames.isEmpty()) {
            return Recipe.noop();
        }
        List<String> filtered = recipeNames.stream()
            .filter(Objects::nonNull)
            .filter(name -> !name.isBlank())
            .distinct()
            .collect(Collectors.toList());
        if (filtered.isEmpty()) {
            return Recipe.noop();
        }
        return environment.activateRecipes(filtered.toArray(String[]::new));
    }

    private Environment loadEnvironment() {
        try {
            Properties properties = new Properties();
            Environment.Builder builder = Environment.builder(properties)
                .scanRuntimeClasspath();

            Path rewriteConfig = Path.of("rewrite.yml");
            if (Files.exists(rewriteConfig)) {
                try (InputStream inputStream = Files.newInputStream(rewriteConfig)) {
                    builder.load(new YamlResourceLoader(inputStream, rewriteConfig.toUri(), properties,
                        Thread.currentThread().getContextClassLoader()));
                }
            }
            return builder.build();
        } catch (IOException ex) {
            throw new IllegalStateException("Unable to initialise OpenRewrite environment", ex);
        }
    }

    private Map<String, RecipeInfo> discoverRecipes(Environment environment) {
        Map<String, RecipeInfo> discovered = new LinkedHashMap<>();
        Set<String> visited = ConcurrentHashMap.newKeySet();
        Collection<RecipeDescriptor> descriptors = environment.listRecipeDescriptors();
        Queue<RecipeDescriptor> queue = new ArrayDeque<>(descriptors);
        while (!queue.isEmpty()) {
            RecipeDescriptor descriptor = queue.poll();
            if (descriptor == null) {
                continue;
            }
            if (!visited.add(descriptor.getName())) {
                continue;
            }

            RecipeInfo info = toRecipeInfo(descriptor);
            discovered.put(info.name(), info);

            Collection<RecipeDescriptor> nested = descriptor.getRecipeList();
            if (nested != null) {
                queue.addAll(nested);
            }
        }
        return discovered;
    }

    private RecipeInfo toRecipeInfo(RecipeDescriptor descriptor) {
        String name = descriptor.getName();
        String displayName = descriptor.getDisplayName() != null && !descriptor.getDisplayName().isBlank()
            ? descriptor.getDisplayName()
            : name;
        String description = descriptor.getDescription() != null ? descriptor.getDescription() : "";

        Set<String> tags = new LinkedHashSet<>();
        if (descriptor.getTags() != null) {
            descriptor.getTags().forEach(tag -> tags.add(tag.toLowerCase(Locale.ROOT)));
        }
        tags.addAll(inferTags(descriptor));

        List<RecipeOption> options = new ArrayList<>();
        List<OptionDescriptor> optionDescriptors = descriptor.getOptions();
        if (optionDescriptors != null) {
            for (OptionDescriptor option : optionDescriptors) {
                options.add(new RecipeOption(
                    option.getName(),
                    normalizeType(option.getType()),
                    option.getDescription(),
                    option.getDefaultValue()
                ));
            }
        }
        return new RecipeInfo(name, displayName, description, List.copyOf(tags), List.copyOf(options));
    }

    private Set<String> inferTags(RecipeDescriptor descriptor) {
        Set<String> inferred = new LinkedHashSet<>();
        String name = descriptor.getName() != null ? descriptor.getName() : "";
        String display = descriptor.getDisplayName() != null ? descriptor.getDisplayName() : "";
        String description = descriptor.getDescription() != null ? descriptor.getDescription() : "";
        String haystack = (name + " " + display + " " + description).toLowerCase(Locale.ROOT);

        inferred.add("quality");
        if (haystack.contains("java 17") || haystack.contains("java17") || haystack.contains("upgradejava17")) {
            inferred.add("java17");
            inferred.add("modernize");
        }
        if (haystack.contains("upgrade") || haystack.contains("migrate")) {
            inferred.add("modernize");
        }
        if (haystack.contains("staticanalysis") || haystack.contains("cleanup") || haystack.contains("style")
            || haystack.contains("format") || haystack.contains("normalize")) {
            inferred.add("style");
        }
        if (haystack.contains("deprecat")) {
            inferred.add("deprecations");
        }
        if (haystack.contains("security") || haystack.contains("owasp")) {
            inferred.add("security");
        }
        if (haystack.contains("junit") || haystack.contains("assertj") || haystack.contains("mockito")) {
            inferred.add("testing");
        }
        if (haystack.contains("logging")) {
            inferred.add("observability");
        }
        return inferred;
    }

    private Map<String, List<String>> buildProfiles(Map<String, RecipeInfo> recipes) {
        Map<String, List<String>> mapping = new LinkedHashMap<>();
        mapping.put("modernize_java17", filterByTags(recipes, List.of("modernize", "java17")));
        mapping.put("cleanup_style", filterByTags(recipes, List.of("style")));
        mapping.put("remove_deprecations", filterByTags(recipes, List.of("deprecations")));
        mapping.put("security", filterByTags(recipes, List.of("security")));
        mapping.put("testing_support", filterByTags(recipes, List.of("testing")));
        mapping.put("quality", filterByTags(recipes, List.of("quality")));
        mapping.put("all", new ArrayList<>(recipes.keySet()));
        return mapping;
    }

    private List<String> filterByTags(Map<String, RecipeInfo> recipes, List<String> requiredTags) {
        if (requiredTags == null || requiredTags.isEmpty()) {
            return new ArrayList<>(recipes.keySet());
        }
        List<String> matches = new ArrayList<>();
        for (RecipeInfo info : recipes.values()) {
            if (info.tags().containsAll(requiredTags)) {
                matches.add(info.name());
            }
        }
        if (matches.isEmpty()) {
            // fall back to partial matches
            for (RecipeInfo info : recipes.values()) {
                for (String tag : requiredTags) {
                    if (info.tags().contains(tag)) {
                        matches.add(info.name());
                        break;
                    }
                }
            }
        }
        return matches;
    }

    private String normalizeType(String rawType) {
        if (rawType == null || rawType.isBlank()) {
            return "string";
        }
        String normalized = rawType.toLowerCase(Locale.ROOT);
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

    /**
     * Immutable descriptor for an OpenRewrite recipe.
     */
    public record RecipeInfo(String name,
                             String displayName,
                             String description,
                             List<String> tags,
                             List<RecipeOption> options) {
    }

    /**
     * Immutable descriptor for a recipe option.
     */
    public record RecipeOption(String name,
                               String type,
                               String description,
                               Object defaultValue) {
    }
}
