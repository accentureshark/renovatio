package org.shark.renovatio.provider.cobol;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.BeforeEach;
import org.shark.renovatio.shared.domain.*;
import org.shark.renovatio.shared.nql.NqlQuery;
import org.shark.renovatio.shared.spi.LanguageProvider;
import org.shark.renovatio.provider.cobol.service.*;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.TestPropertySource;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Tests for COBOL Language Provider
 */
@SpringBootTest
@TestPropertySource(properties = "spring.main.allow-bean-definition-overriding=true")
class CobolLanguageProviderTest {
    
    private CobolLanguageProvider provider;
    private Workspace testWorkspace;
    
    @BeforeEach
    void setUp() {
        // Initialize services
        CobolParsingService parsingService = new CobolParsingService();
        JavaGenerationService javaGenerationService = new JavaGenerationService(parsingService);
        MigrationPlanService migrationPlanService = new MigrationPlanService(parsingService, javaGenerationService);
        IndexingService indexingService = new IndexingService();
        MetricsService metricsService = new MetricsService();
        
        provider = new CobolLanguageProvider(
            parsingService,
            javaGenerationService,
            migrationPlanService,
            indexingService,
            metricsService
        );
        
        // Setup test workspace
        testWorkspace = new Workspace();
        testWorkspace.setId("test-workspace");
        testWorkspace.setPath(System.getProperty("java.io.tmpdir"));
        testWorkspace.setBranch("main");
    }
    
    @Test
    void testLanguageIdentification() {
        assertEquals("cobol", provider.language());
    }
    
    @Test
    void testCapabilities() {
        var capabilities = provider.capabilities();
        
        assertTrue(capabilities.contains(LanguageProvider.Capabilities.ANALYZE));
        assertTrue(capabilities.contains(LanguageProvider.Capabilities.PLAN));
        assertTrue(capabilities.contains(LanguageProvider.Capabilities.APPLY));
        assertTrue(capabilities.contains(LanguageProvider.Capabilities.DIFF));
        assertTrue(capabilities.contains(LanguageProvider.Capabilities.STUBS));
        assertTrue(capabilities.contains(LanguageProvider.Capabilities.METRICS));
    }
    
    @Test
    void testAnalyze() {
        NqlQuery query = new NqlQuery();
        query.setType(NqlQuery.QueryType.FIND);
        query.setTarget("programs");
        query.setLanguage("cobol");
        
        AnalyzeResult result = provider.analyze(query, testWorkspace);
        
        assertNotNull(result);
        assertTrue(result.isSuccess());
        assertNotNull(result.getMessage());
    }
    
    @Test
    void testPlan() {
        NqlQuery query = new NqlQuery();
        query.setType(NqlQuery.QueryType.PLAN);
        query.setTarget("migration");
        query.setLanguage("cobol");
        
        Scope scope = new Scope();
        
        PlanResult result = provider.plan(query, scope, testWorkspace);
        
        assertNotNull(result);
        assertTrue(result.isSuccess());
        assertNotNull(result.getPlanId());
        assertNotNull(result.getMessage());
    }
    
    @Test
    void testGenerateStubs() {
        NqlQuery query = new NqlQuery();
        query.setType(NqlQuery.QueryType.FIND);
        query.setTarget("stubs");
        query.setLanguage("cobol");
        
        var result = provider.generateStubs(query, testWorkspace);
        
        assertTrue(result.isPresent());
        assertNotNull(result.get());
        assertTrue(result.get().isSuccess());
    }
    
    @Test
    void testMetrics() {
        Scope scope = new Scope();
        
        MetricsResult result = provider.metrics(scope, testWorkspace);
        
        assertNotNull(result);
        assertTrue(result.isSuccess());
        assertNotNull(result.getMetrics());
    }
    
    @Test
    void testApplyWithInvalidPlan() {
        ApplyResult result = provider.apply("invalid-plan-id", true, testWorkspace);
        
        assertNotNull(result);
        assertFalse(result.isSuccess());
        assertTrue(result.getMessage().contains("not found"));
    }
    
    @Test
    void testDiffWithInvalidRun() {
        DiffResult result = provider.diff("invalid-run-id", testWorkspace);
        
        assertNotNull(result);
        assertFalse(result.isSuccess());
        assertTrue(result.getMessage().contains("not found"));
    }
}