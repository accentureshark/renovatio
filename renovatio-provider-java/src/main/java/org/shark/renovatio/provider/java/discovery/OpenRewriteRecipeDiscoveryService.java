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
import java.lang.reflect.Method;
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
            .filter(this::isRecipeSafeToExecute)
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
                    extractDefaultValue(option)
                ));
            }
        }
        return new RecipeInfo(name, displayName, description, List.copyOf(tags), List.copyOf(options));
    }

    private Object extractDefaultValue(OptionDescriptor option) {
        if (option == null) {
            return null;
        }
        for (String methodName : List.of("getDefaultValue", "getExample", "getDefault")) {
            try {
                Method method = option.getClass().getMethod(methodName);
                return method.invoke(option);
            } catch (ReflectiveOperationException | SecurityException ignored) {
                // Fall through to try the next accessor name
            }
        }
        return null;
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
        mapping.put("modernize_java17", filterByTagsAndSafety(recipes, List.of("modernize", "java17")));
        mapping.put("cleanup_style", filterByTagsAndSafety(recipes, List.of("style")));
        mapping.put("remove_deprecations", filterByTagsAndSafety(recipes, List.of("deprecations")));
        mapping.put("security", filterByTagsAndSafety(recipes, List.of("security")));
        mapping.put("testing_support", filterByTagsAndSafety(recipes, List.of("testing")));
        mapping.put("quality", filterByTagsAndSafety(recipes, List.of("quality")));
        // For "all" profile, only include safe recipes
        List<String> allSafeRecipes = new ArrayList<>();
        for (String recipeName : recipes.keySet()) {
            if (isRecipeSafeToExecute(recipeName)) {
                allSafeRecipes.add(recipeName);
            }
        }
        mapping.put("all", allSafeRecipes);
        return mapping;
    }

    private List<String> filterByTags(Map<String, RecipeInfo> recipes, List<String> requiredTags) {
        return filterByTagsAndSafety(recipes, requiredTags);
    }

    private List<String> filterByTagsAndSafety(Map<String, RecipeInfo> recipes, List<String> requiredTags) {
        if (requiredTags == null || requiredTags.isEmpty()) {
            List<String> safeRecipes = new ArrayList<>();
            for (String recipeName : recipes.keySet()) {
                if (isRecipeSafeToExecute(recipeName)) {
                    safeRecipes.add(recipeName);
                }
            }
            return safeRecipes;
        }
        
        List<String> matches = new ArrayList<>();
        for (RecipeInfo info : recipes.values()) {
            if (info.tags().containsAll(requiredTags) && isRecipeSafeToExecute(info.name())) {
                matches.add(info.name());
            }
        }
        
        if (matches.isEmpty()) {
            // fall back to partial matches, but still apply safety filter
            for (RecipeInfo info : recipes.values()) {
                for (String tag : requiredTags) {
                    if (info.tags().contains(tag) && isRecipeSafeToExecute(info.name())) {
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
     * Check if a recipe is safe to execute without additional configuration.
     * Some recipes require specific parameters that may not be set and can cause NPE.
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
            LOGGER.debug("Filtering out recipe {} - requires specific configuration", recipeName);
            return false;
        }
        
        // Check for recipe patterns that typically require configuration
        if (recipeName.toLowerCase(Locale.ROOT).contains("create") && 
            (recipeName.contains("Class") || recipeName.contains("File"))) {
            LOGGER.debug("Filtering out recipe {} - creation recipes typically require specific parameters", recipeName);
            return false;
        }
        
        if (recipeName.toLowerCase(Locale.ROOT).contains("change") &&
            (recipeName.contains("Type") || recipeName.contains("Method") || recipeName.contains("Field"))) {
            LOGGER.debug("Filtering out recipe {} - change recipes typically require specific parameters", recipeName);
            return false;
        }
        
        // Also filter recipes that are known to require parameters
        RecipeInfo info = recipes.get(recipeName);
        if (info != null && hasRequiredOptions(info)) {
            LOGGER.debug("Filtering out recipe {} - has required options that may not be configured", recipeName);
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
