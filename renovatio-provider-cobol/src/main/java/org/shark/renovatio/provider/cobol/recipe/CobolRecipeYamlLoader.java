package org.shark.renovatio.provider.cobol.recipe;

import org.yaml.snakeyaml.Yaml;

import java.io.InputStream;
import java.util.Map;

/**
 * Loads COBOL recipes defined in a YAML file and registers them in a {@link CobolRecipeRegistry}.
 */
public class CobolRecipeYamlLoader {

    /**
     * Load recipes from a YAML resource available on the classpath.
     *
     * @param resourceName resource to load, e.g. "cobol-rewrite.yml"
     * @return registry containing all discovered recipes
     */
    public CobolRecipeRegistry load(String resourceName) {
        CobolRecipeRegistry registry = new CobolRecipeRegistry();
        Yaml yaml = new Yaml();

        try (InputStream input = getClass().getClassLoader().getResourceAsStream(resourceName)) {
            if (input == null) {
                return registry;
            }
            Map<String, Object> data = yaml.load(input);
            Object recipesNode = data.get("recipes");
            if (recipesNode instanceof Map<?, ?> recipes) {
                for (Map.Entry<?, ?> entry : recipes.entrySet()) {
                    String name = String.valueOf(entry.getKey());
                    String className = String.valueOf(entry.getValue());
                    try {
                        Class<?> clazz = Class.forName(className);
                        if (CobolRecipe.class.isAssignableFrom(clazz)) {
                            CobolRecipe recipe = (CobolRecipe) clazz.getDeclaredConstructor().newInstance();
                            registry.register(name, recipe);
                        }
                    } catch (Exception ex) {
                        throw new IllegalArgumentException("Failed to load recipe: " + className, ex);
                    }
                }
            }
        } catch (Exception e) {
            throw new IllegalStateException("Unable to load recipes from " + resourceName, e);
        }

        return registry;
    }
}
