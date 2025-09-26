package org.shark.renovatio.provider.java.discovery;

import org.openrewrite.Recipe;
import org.openrewrite.config.Environment;
import org.openrewrite.config.OptionDescriptor;
import org.openrewrite.config.RecipeDescriptor;
import org.openrewrite.config.YamlResourceLoader;
import org.shark.renovatio.provider.java.util.RecipeSafetyUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;

import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.Method;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;

/**
 * Service responsible for discovering OpenRewrite recipes available on the
 * classpath and exposing metadata used by MCP tools.
 */
@Service
public class OpenRewriteRecipeDiscoveryService {

    private static final Logger LOGGER = LoggerFactory.getLogger(OpenRewriteRecipeDiscoveryService.class);

    private static final Set<String> UNSAFE_RECIPES = Set.of(
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
            "org.openrewrite.java.dependencies.ChangeDependency",
            "org.openrewrite.java.RecipeMarkupDemonstration"
    );

    private static final List<String> UNSAFE_RECIPE_PREFIXES = List.of(
            // Example/demo recipes frequently spin up CreateEmptyJavaClass without configuration.
            "org.openrewrite.java.recipes."
    );

    private static final Set<String> REQUIRED_OPTION_KEYWORDS = Set.of(
            "packagename",
            "classname",
            "filepath",
            "filename",
            "path",
            "methodname",
            "fieldname",
            "typename",
            "oldname",
            "newname",
            "oldtype",
            "newtype",
            "groupid",
            "artifactid",
            "version",
            "type",
            "name",
            "target",
            "source",
            // Recipes that compose other recipes almost always require explicit configuration
            "recipe",
            "recipelist",
            "template",
            "contents"
    );

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
                .map(String::trim)
                .filter(name -> !name.isBlank())
                .filter(this::isRecipeSafe)
                .distinct()
                .collect(Collectors.toList());
        if (filtered.isEmpty()) {
            return Recipe.noop();
        }
        // Only pass recipes that are actually available in the discovered map
        List<String> available = new ArrayList<>();
        List<String> missing = new ArrayList<>();
        for (String name : filtered) {
            if (recipes.containsKey(name)) {
                available.add(name);
            } else {
                missing.add(name);
            }
        }
        if (!missing.isEmpty()) {
            LOGGER.debug("Skipping missing recipes {}", missing);
        }
        if (available.isEmpty()) {
            return Recipe.noop();
        }
        try {
            return environment.activateRecipes(available.toArray(String[]::new));
        } catch (Exception ex) {
            // Fallback: try to filter activable recipes one by one
            List<String> activable = new ArrayList<>();
            List<String> notActivable = new ArrayList<>();
            for (String name : available) {
                try {
                    environment.activateRecipes(name);
                    activable.add(name);
                } catch (Exception e) {
                    notActivable.add(name);
                }
            }
            if (!notActivable.isEmpty()) {
                LOGGER.debug("Skipping not-activable recipes {} due to Environment failure", notActivable);
            }
            if (activable.isEmpty()) {
                return Recipe.noop();
            }
            return environment.activateRecipes(activable.toArray(String[]::new));
        }
    }

    /**
     * Resolve recipe names to those that are safe, present in the discovered catalog, and activable by the Environment.
     */
    public List<String> resolveActivableRecipeNames(Collection<String> recipeNames) {
        if (recipeNames == null || recipeNames.isEmpty()) {
            return List.of();
        }
        List<String> filtered = recipeNames.stream()
                .filter(Objects::nonNull)
                .map(String::trim)
                .filter(name -> !name.isBlank())
                .filter(this::isRecipeSafe)
                .distinct()
                .collect(Collectors.toList());
        if (filtered.isEmpty()) {
            return List.of();
        }
        List<String> available = new ArrayList<>();
        for (String name : filtered) {
            if (recipes.containsKey(name)) {
                available.add(name);
            } else {
                LOGGER.debug("Skipping missing recipe {}", name);
            }
        }
        if (available.isEmpty()) {
            return List.of();
        }
        List<String> activable = new ArrayList<>();
        for (String name : available) {
            try {
                environment.activateRecipes(name);
                activable.add(name);
            } catch (Exception e) {
                LOGGER.debug("Skipping not-activable recipe {}", name);
            }
        }
        return activable;
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
            if (isRecipeSafe(recipeName)) {
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
                if (isRecipeSafe(recipeName)) {
                    safeRecipes.add(recipeName);
                }
            }
            return safeRecipes;
        }

        List<String> matches = new ArrayList<>();
        for (RecipeInfo info : recipes.values()) {
            if (info.tags().containsAll(requiredTags) && isRecipeSafe(info.name())) {
                matches.add(info.name());
            }
        }

        if (matches.isEmpty()) {
            // fall back to partial matches, but still apply safety filter
            for (RecipeInfo info : recipes.values()) {
                for (String tag : requiredTags) {
                    if (info.tags().contains(tag) && isRecipeSafe(info.name())) {
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
    public boolean isRecipeSafe(String recipeName) {
        if (recipeName == null || recipeName.isBlank()) {
            return false;
        }

        // Refuerzo: si es CreateEmptyJavaClass y no se pueden inspeccionar ambos métodos, o sus valores son nulos/vacíos, márcala como insegura
        if (recipeName.contains("CreateEmptyJavaClass")) {
            try {
                Class<?> clazz = Class.forName(recipeName);
                boolean hasPackage = false;
                boolean hasClass = false;
                Object packageValue = null;
                Object classValue = null;
                try {
                    Method getPackage = clazz.getMethod("getPackageName");
                    hasPackage = true;
                    packageValue = getPackage.invoke(clazz.getDeclaredConstructor().newInstance());
                } catch (Exception e) {
                }
                try {
                    Method getClassName = clazz.getMethod("getClassName");
                    hasClass = true;
                    classValue = getClassName.invoke(clazz.getDeclaredConstructor().newInstance());
                } catch (Exception e) {
                }
                if (!hasPackage || !hasClass) {
                    return false;
                }
                // Validar valores devueltos
                if (isNullOrEmptyOrOptionalEmpty(packageValue) || isNullOrEmptyOrOptionalEmpty(classValue)) {
                    return false;
                }
            } catch (Exception e) {
                // Si no se puede inspeccionar la clase (por ser stub de test), márcala como insegura
                return false;
            }
        }

        if (UNSAFE_RECIPES.contains(recipeName)) {
            LOGGER.debug("Filtering out recipe {} - requires specific configuration", recipeName);
            return false;
        }

        for (String prefix : UNSAFE_RECIPE_PREFIXES) {
            if (recipeName.startsWith(prefix)) {
                LOGGER.debug("Filtering out recipe {} - recipes under {} require explicit configuration", recipeName, prefix);
                return false;
            }
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

        // Lógica especial para CompositeRecipe: solo insegura si requiere configuración explícita
        RecipeInfo info = recipes.get(recipeName);
        if (info != null) {
            if (isCompositeRecipe(info)) {
                // Si la lista de opciones indica que requiere configuración explícita, o si la lista de recetas está vacía o contiene inseguras
                List<String> children = getCompositeRecipeChildren(recipeName);
                if (children.isEmpty()) {
                    LOGGER.debug("Filtering out composite recipe {} - empty children list", recipeName);
                    return false;
                }
                for (String child : children) {
                    if (!isRecipeSafe(child)) {
                        LOGGER.debug("Filtering out composite recipe {} - contains unsafe child {}", recipeName, child);
                        return false;
                    }
                }
                // Si todas las hijas son seguras, la composite es segura
                return true;
            }
            if (hasRequiredOptions(info)) {
                LOGGER.debug("Filtering out recipe {} - has required options that may not be configured", recipeName);
                return false;
            }
        }

        return true;
    }

    /**
     * Checks if a Recipe instance has missing required parameters (true = faltan parámetros requeridos).
     */
    private boolean hasMissingRequiredParameters(Recipe recipe) {
        return RecipeSafetyUtils.hasMissingRequiredParameters(recipe);
    }

    /**
     * Overload: Check if a Recipe instance is safe (no missing required parameters).
     */
    public boolean isRecipeSafe(Recipe recipe) {
        return !hasMissingRequiredParameters(recipe);
    }

    // Utilidad para validar valores nulos, vacíos o Optional vacío
    private boolean isNullOrEmptyOrOptionalEmpty(Object value) {
        if (value == null) return true;
        if (value instanceof String s) return s.isBlank();
        if (value instanceof Optional<?> opt) return opt.isEmpty();
        return false;
    }

    // Determina si una receta es composite (compuesta)
    private boolean isCompositeRecipe(RecipeInfo info) {
        // Heurística: si tiene opciones tipo 'recipe', 'recipelist', 'template', o el nombre contiene 'Composite'
        if (info == null) return false;
        if (info.name().toLowerCase(Locale.ROOT).contains("composite")) return true;
        if (info.options() != null) {
            for (RecipeOption opt : info.options()) {
                String n = opt.name() != null ? opt.name().toLowerCase(Locale.ROOT) : "";
                if (n.contains("recipe") || n.contains("recipelist") || n.contains("template")) {
                    return true;
                }
            }
        }
        return false;
    }

    // Obtiene la lista de recetas hijas de una composite, si es posible
    private List<String> getCompositeRecipeChildren(String recipeName) {
        RecipeInfo info = recipes.get(recipeName);
        if (info == null) return List.of();
        // Heurística: buscar opciones tipo lista de recetas
        List<String> children = new ArrayList<>();
        if (info.options() != null) {
            for (RecipeOption opt : info.options()) {
                if (opt.type() != null && opt.type().equalsIgnoreCase("array") && opt.defaultValue() instanceof List<?> l) {
                    for (Object o : l) {
                        if (o instanceof String s) children.add(s);
                    }
                }
            }
        }
        return children;
    }

    // Determina si una receta requiere opciones obligatorias sin valor por defecto
    private boolean hasRequiredOptions(RecipeInfo info) {
        if (info == null || info.options() == null) return false;
        for (RecipeOption opt : info.options()) {
            String name = opt.name() != null ? opt.name().toLowerCase(Locale.ROOT) : "";
            if (REQUIRED_OPTION_KEYWORDS.contains(name)) {
                if (isNullOrEmptyOrOptionalEmpty(opt.defaultValue())) {
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
