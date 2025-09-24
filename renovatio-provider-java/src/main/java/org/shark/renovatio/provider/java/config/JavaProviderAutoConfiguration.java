package org.shark.renovatio.provider.java.config;

import org.shark.renovatio.provider.java.JavaProvider;
import org.shark.renovatio.provider.java.OpenRewriteRunner;
import org.shark.renovatio.provider.java.adapter.OpenRewriteAnalyzeAdapter;
import org.shark.renovatio.provider.java.adapter.OpenRewriteApplyAdapter;
import org.shark.renovatio.provider.java.discovery.OpenRewriteRecipeDiscoveryService;
import org.shark.renovatio.provider.java.execution.JavaRecipeExecutor;
import org.shark.renovatio.provider.java.planner.JavaRefactorPlanner;
import org.springframework.boot.autoconfigure.AutoConfiguration;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Primary;

/**
 * Spring auto-configuration that registers all beans required for the Java MCP provider.
 */
@AutoConfiguration
public class JavaProviderAutoConfiguration {

    @Bean
    public OpenRewriteRunner openRewriteRunner() {
        return new OpenRewriteRunner();
    }

    @Bean
    public OpenRewriteAnalyzeAdapter openRewriteAnalyzeAdapter() {
        return new OpenRewriteAnalyzeAdapter();
    }

    @Bean
    public OpenRewriteApplyAdapter openRewriteApplyAdapter() {
        return new OpenRewriteApplyAdapter();
    }

    @Bean
    public JavaRecipeExecutor javaRecipeExecutor(OpenRewriteRecipeDiscoveryService discoveryService,
                                                 OpenRewriteRunner runner) {
        return new JavaRecipeExecutor(discoveryService, runner);
    }

    @Bean
    public JavaRefactorPlanner javaRefactorPlanner(OpenRewriteRecipeDiscoveryService discoveryService) {
        return new JavaRefactorPlanner(discoveryService);
    }

    @Bean
    @Primary
    public JavaProvider javaProvider(OpenRewriteRecipeDiscoveryService discoveryService,
                                     JavaRefactorPlanner planner,
                                     JavaRecipeExecutor executor,
                                     OpenRewriteAnalyzeAdapter analyzeAdapter,
                                     OpenRewriteApplyAdapter applyAdapter) {
        return new JavaProvider(discoveryService, planner, executor, analyzeAdapter, applyAdapter);
    }
}
