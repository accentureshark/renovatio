package org.shark.renovatio.provider.java.discovery;

import org.junit.jupiter.api.Test;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

class OpenRewriteRecipeDiscoveryServiceSafetyTest {

    private static final OpenRewriteRecipeDiscoveryService DISCOVERY_SERVICE = new OpenRewriteRecipeDiscoveryService();

    @Test
    void compositeRecipeRequiresConfiguration() {
        assertFalse(DISCOVERY_SERVICE.isRecipeSafe("org.openrewrite.config.CompositeRecipe"),
            "CompositeRecipe should be treated as unsafe without explicit configuration");
    }

    @Test
    void createEmptyJavaClassRequiresConfiguration() {
        assertFalse(DISCOVERY_SERVICE.isRecipeSafe("org.openrewrite.java.CreateEmptyJavaClass"),
            "CreateEmptyJavaClass should be treated as unsafe when packageName is not provided");
    }

    @Test
    void autoFormatRecipeIsSafe() {
        assertTrue(DISCOVERY_SERVICE.isRecipeSafe("org.openrewrite.java.format.AutoFormat"),
            "AutoFormat is expected to be safe to run without additional configuration");
    }

    @Test
    void curatedProfilesExcludeUnsafeRecipes() {
        List<String> qualityProfile = DISCOVERY_SERVICE.profilesToRecipes().get("quality");
        assertNotNull(qualityProfile, "Quality profile should be available");
        assertFalse(qualityProfile.contains("org.openrewrite.config.CompositeRecipe"),
            "Quality profile should not include recipes that require additional configuration");
        assertFalse(qualityProfile.contains("org.openrewrite.java.CreateEmptyJavaClass"),
            "Quality profile should exclude CreateEmptyJavaClass to avoid NPEs");
    }
}
