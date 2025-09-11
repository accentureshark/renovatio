package org.shark.renovatio.provider.cobol.recipe;

/**
 * Sample recipe used for testing the registry and loader.
 */
public class NoOpCobolRecipe implements CobolRecipe {

    @Override
    public void plan() {
        // no planning required
    }

    @Override
    public void apply() {
        // nothing to apply
    }

    @Override
    public String description() {
        return "No operation recipe";
    }
}
