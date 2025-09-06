package org.shark.renovatio.agent;

import org.shark.renovatio.core.service.LanguageProviderRegistry;
import org.shark.renovatio.provider.java.JavaProvider;
import org.shark.renovatio.provider.cobol.CobolProvider;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.CommandLineRunner;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.beans.factory.annotation.Autowired;

/**
 * Main Agent Gateway application
 * Integrates all providers and exposes unified API
 */
@SpringBootApplication
@ComponentScan(basePackages = {
    "org.shark.renovatio.core",
    "org.shark.renovatio.provider.java", 
    "org.shark.renovatio.provider.cobol",
    "org.shark.renovatio.agent"
})
public class AgentGatewayApplication {
    
    public static void main(String[] args) {
        SpringApplication.run(AgentGatewayApplication.class, args);
    }
    
    @Bean
    public CommandLineRunner initializeProviders(
            @Autowired LanguageProviderRegistry registry,
            @Autowired JavaProvider javaProvider,
            @Autowired CobolProvider cobolProvider) {
        return args -> {
            registry.registerProvider(javaProvider);
            registry.registerProvider(cobolProvider);
            
            System.out.println("\n================ Renovatio Agent Gateway Started ================");
            System.out.println("Registered Language Providers:");
            for (String language : registry.getSupportedLanguages()) {
                var provider = registry.getProvider(language);
                System.out.printf("- %s: %s\n", language, provider.capabilities());
            }
            
            System.out.println("\nGenerated MCP Tools:");
            var tools = registry.generateMcpTools();
            for (var tool : tools) {
                System.out.printf("- %s: %s\n", tool.getName(), tool.getDescription());
            }
            System.out.println("===============================================================\n");
        };
    }
}