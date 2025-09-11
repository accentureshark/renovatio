package org.shark.renovatio.web;

import org.shark.renovatio.core.service.LanguageProviderRegistry;
import org.shark.renovatio.shared.spi.LanguageProvider;
import org.shark.renovatio.provider.java.JavaProvider;
import org.shark.renovatio.provider.cobol.CobolProvider;
import org.springframework.stereotype.Component;
import jakarta.annotation.PostConstruct;

/**
 * Registers language providers in the LanguageProviderRegistry when Renovatio Web starts.
 */
@Component
public class LanguageProviderRegistrar {
    private final LanguageProviderRegistry registry;
    private final JavaProvider javaProvider;
    private final CobolProvider cobolProvider;

    public LanguageProviderRegistrar(LanguageProviderRegistry registry, JavaProvider javaProvider, CobolProvider cobolProvider) {
        this.registry = registry;
        this.javaProvider = javaProvider;
        this.cobolProvider = cobolProvider;
    }

    @PostConstruct
    public void registerProviders() {
        registry.registerProvider(javaProvider);
        registry.registerProvider(cobolProvider);
    }
}

