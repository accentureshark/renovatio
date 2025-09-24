package org.shark.renovatio.provider.cobol;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.io.TempDir;
import org.shark.renovatio.shared.domain.*;
import org.shark.renovatio.shared.nql.NqlQuery;
import org.shark.renovatio.shared.spi.LanguageProvider;
import org.shark.renovatio.provider.cobol.service.*;

import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Tests for COBOL Language Provider
 */
class CobolLanguageProviderTest {
    @TempDir
    Path tempDir;

    private CobolLanguageProvider provider;
    private Workspace testWorkspace;

    @BeforeEach
    void setUp() {
        // Initialize services
        CobolParsingService parsingService = new CobolParsingService();
        TemplateCodeGenerationService templateService = new TemplateCodeGenerationService();
        JavaGenerationService javaGenerationService = new JavaGenerationService(parsingService, templateService);
        Db2MigrationService db2Service = new Db2MigrationService(parsingService);
        MigrationPlanService migrationPlanService = new MigrationPlanService(parsingService, javaGenerationService);
        IndexingService indexingService = new IndexingService();
        MetricsService metricsService = new MetricsService();

        provider = new CobolLanguageProvider(
            parsingService,
            javaGenerationService,
            migrationPlanService,
            indexingService,
            metricsService,
            templateService,
            db2Service
        );

        // Setup test workspace
        testWorkspace = new Workspace();
        testWorkspace.setId("test-workspace");
        testWorkspace.setPath(tempDir.toString());
        testWorkspace.setBranch("main");

        // Crear archivo COBOL de ejemplo en el workspace temporal
        String cobolContent = """
            IDENTIFICATION DIVISION.
            PROGRAM-ID. SAMPLE-PROGRAM.
            DATA DIVISION.
            WORKING-STORAGE SECTION.
            01  WS-NAME       PIC X(30).
            01  WS-AGE        PIC 9(3).
            01  WS-SALARY     PIC 9(8)V99.
            PROCEDURE DIVISION.
            MAIN-PARA.
                DISPLAY \"Hello World\".
                STOP RUN.
            """;
        try {
            Path cobolFile = tempDir.resolve("sample.cob");
            Files.writeString(cobolFile, cobolContent);
        } catch (Exception e) {
            throw new RuntimeException("No se pudo crear el archivo de ejemplo COBOL para los tests", e);
        }
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