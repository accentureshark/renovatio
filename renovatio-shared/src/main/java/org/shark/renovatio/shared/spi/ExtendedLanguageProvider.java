package org.shark.renovatio.shared.spi;

import java.util.Map;

/**
 * Optional extension point for language providers that need to expose
 * additional MCP tools beyond the classic analyze/plan/apply pipeline.
 * <p>
 * Implementations may override {@link #executeExtendedTool(String, Map)}
 * to handle provider-specific capabilities. When the method returns
 * {@code null}, the registry will fall back to the legacy capability
 * routing implemented in {@link LanguageProviderRegistry}.
 */
public interface ExtendedLanguageProvider {

    /**
     * Execute a provider-specific tool capability.
     *
     * @param capability normalized capability name (lower case, underscores)
     * @param arguments  arguments passed by the MCP client
     * @return result map or {@code null} to indicate the capability is not handled
     */
    default Map<String, Object> executeExtendedTool(String capability, Map<String, Object> arguments) {
        return null;
    }
}
