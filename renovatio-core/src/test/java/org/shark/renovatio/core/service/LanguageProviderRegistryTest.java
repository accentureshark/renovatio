package org.shark.renovatio.core.service;

import org.junit.jupiter.api.Test;
import org.shark.renovatio.shared.domain.ApplyResult;
import org.shark.renovatio.shared.spi.LanguageProvider;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.Mockito.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

class LanguageProviderRegistryTest {

    @Test
    void routeToolCallExtractsRecipeIdForApplyTools() {
        LanguageProviderRegistry registry = new LanguageProviderRegistry();

        LanguageProvider provider = mock(LanguageProvider.class);
        when(provider.language()).thenReturn("java");
        when(provider.capabilities()).thenReturn(Set.of(LanguageProvider.Capabilities.APPLY));
        when(provider.apply(any(), anyBoolean(), any())).thenReturn(new ApplyResult(true, "ok"));

        registry.registerProvider(provider);

        Map<String, Object> arguments = new HashMap<>();
        arguments.put("workspacePath", "/tmp/workspace");

        Map<String, Object> result = registry.routeToolCall(
                "java.apply.org_openrewrite.migrate_to_java17",
                arguments
        );

        assertNotNull(result, "Route result should never be null");
        assertEquals("org_openrewrite.migrate_to_java17", arguments.get("recipeId"));

        verify(provider).apply(any(), eq(true), any());
    }

    @Test
    void routeToolCallSupportsUnderscoreRecipeSeparator() {
        LanguageProviderRegistry registry = new LanguageProviderRegistry();

        LanguageProvider provider = mock(LanguageProvider.class);
        when(provider.language()).thenReturn("java");
        when(provider.capabilities()).thenReturn(Set.of(LanguageProvider.Capabilities.APPLY));
        when(provider.apply(any(), anyBoolean(), any())).thenReturn(new ApplyResult(true, "ok"));

        registry.registerProvider(provider);

        Map<String, Object> arguments = new HashMap<>();
        arguments.put("workspacePath", "/tmp/workspace");

        registry.routeToolCall("java.apply_org_openrewrite", arguments);

        assertEquals("org_openrewrite", arguments.get("recipeId"));
        verify(provider).apply(any(), eq(true), any());
    }
}
