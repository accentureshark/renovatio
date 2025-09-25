package org.shark.renovatio.shared.domain;

import java.util.Map;

/**
 * Universal recipe definition that works across all language providers.
 * This provides a unified format for recipes regardless of the underlying implementation
 * (OpenRewrite for Java, ANTLR for other languages, etc.)
 */
public interface Recipe {

    /**
     * Get the recipe identifier
     */
    String getId();

    /**
     * Get the recipe display name
     */
    String getDisplayName();

    /**
     * Get the recipe description
     */
    String getDescription();

    /**
     * Get the supported language for this recipe
     */
    String getLanguage();

    /**
     * Get the recipe tags/categories
     */
    String[] getTags();

    /**
     * Get recipe configuration options
     */
    Map<String, Object> getOptions();

    /**
     * Get recipe metadata
     */
    Map<String, Object> getMetadata();

    /**
     * Check if this recipe is applicable to the given scope
     */
    boolean isApplicable(Scope scope);
}