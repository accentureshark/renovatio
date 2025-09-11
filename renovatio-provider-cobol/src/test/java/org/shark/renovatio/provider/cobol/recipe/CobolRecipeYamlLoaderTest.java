package org.shark.renovatio.provider.cobol.recipe;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertTrue;

class CobolRecipeYamlLoaderTest {

    @Test
    void loadsRecipesFromYaml() {
        CobolRecipeYamlLoader loader = new CobolRecipeYamlLoader();
        CobolRecipeRegistry registry = loader.load("cobol-rewrite.yml");
        assertTrue(registry.find("noop").isPresent(), "NoOp recipe should be loaded");
    }
}
