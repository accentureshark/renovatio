package org.shark.renovatio.provider.cobol.infrastructure;

import org.shark.renovatio.provider.cobol.CobolLanguageProvider;
import org.shark.renovatio.provider.cobol.service.*;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.ComponentScan;

/**
 * Spring configuration for COBOL provider
 * Ensures all services are properly wired and available
 */
@Configuration
@ComponentScan(basePackages = "org.shark.renovatio.provider.cobol")
public class CobolProviderConfiguration {
    
    @Bean
    public CobolParsingService cobolParsingService(@Value("${renovatio.cobol.parser.dialect:IBM}") String dialect) {
        return new CobolParsingService(CobolParsingService.Dialect.fromString(dialect));
    }
    
    @Bean
    public JavaGenerationService javaGenerationService(
            CobolParsingService parsingService,
            TemplateCodeGenerationService templateCodeGenerationService) {
        return new JavaGenerationService(parsingService, templateCodeGenerationService);
    }
    
    @Bean
    public MigrationPlanService migrationPlanService(
            CobolParsingService parsingService,
            JavaGenerationService javaGenerationService) {
        return new MigrationPlanService(parsingService, javaGenerationService);
    }
    
    @Bean
    public IndexingService indexingService() {
        return new IndexingService();
    }
    
    @Bean
    public MetricsService metricsService() {
        return new MetricsService();
    }

    @Bean
    public CicsService cicsService(
            @Value("${renovatio.cics.mock:true}") boolean mock,
            @Value("${renovatio.cics.url:http://localhost:10080}") String url) {
        return mock ? new MockCicsService() : new RealCicsService(url);
    }

    @Bean
    public Db2MigrationService db2MigrationService(CobolParsingService parsingService) {
        return new Db2MigrationService(parsingService);
    }
    
    @Bean
    public CobolLanguageProvider cobolLanguageProvider(
            CobolParsingService parsingService,
            JavaGenerationService javaGenerationService,
            MigrationPlanService migrationPlanService,
            IndexingService indexingService,
            MetricsService metricsService,
            TemplateCodeGenerationService templateCodeGenerationService,
            Db2MigrationService db2MigrationService) {
        return new CobolLanguageProvider(
            parsingService,
            javaGenerationService,
            migrationPlanService,
            indexingService,
            metricsService,
            templateCodeGenerationService,
            db2MigrationService
        );
    }
}