package org.shark.renovatio.provider.cobol.recipe;

/**
 * Basic contract for COBOL refactoring recipes.
 */
public interface CobolRecipe {

    /**
     * Prepare the recipe and provide a plan of actions.
     */
    void plan();

    /**
     * Apply the recipe to the target source code or resources.
     */
    void apply();

    /**
     * @return human friendly description of what the recipe does
     */
    String description();
}
