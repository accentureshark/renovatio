package org.shark.renovatio.application.recipes;

/**
 * Strategy interface for applying a specific refactoring recipe to a block of
 * source code. Implementations encapsulate the logic for a single recipe and
 * are registered as Spring components keyed by the recipe name.
 */
@FunctionalInterface
public interface RecipeHandler {

    /**
     * Apply the transformation logic to the given source code and return the
     * refactored result.
     *
     * @param source original source code
     * @return transformed source code
     */
    String apply(String source);
}
