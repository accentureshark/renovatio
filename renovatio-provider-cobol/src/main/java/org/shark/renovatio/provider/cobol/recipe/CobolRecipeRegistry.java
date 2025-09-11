package org.shark.renovatio.provider.cobol.recipe;

import java.util.Collection;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Registry holding available {@link CobolRecipe} implementations.
 */
public class CobolRecipeRegistry {

    private final Map<String, CobolRecipe> recipes = new ConcurrentHashMap<>();

    /**
     * Register a recipe under the given name.
     *
     * @param name   lookup name
     * @param recipe recipe instance
     */
    public void register(String name, CobolRecipe recipe) {
        recipes.put(name, recipe);
    }

    /**
     * Find a recipe by name.
     *
     * @param name lookup name
     * @return recipe or empty if not found
     */
    public Optional<CobolRecipe> find(String name) {
        return Optional.ofNullable(recipes.get(name));
    }

    /**
     * @return all registered recipes
     */
    public Collection<CobolRecipe> getAll() {
        return recipes.values();
    }
}
