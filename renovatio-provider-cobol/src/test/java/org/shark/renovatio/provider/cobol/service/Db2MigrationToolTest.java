package org.shark.renovatio.provider.cobol.service;

import org.junit.jupiter.api.Test;
import org.shark.renovatio.provider.cobol.CobolLanguageProvider;
import org.shark.renovatio.provider.cobol.infrastructure.CobolMcpToolsProvider;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Tests for DB2 migration MCP tool.
 */
public class Db2MigrationToolTest {

    @Test
    void migrateDb2GeneratesJpaArtifacts() throws Exception {
        Path temp = Files.createTempDirectory("cobol-db2");
        String cobol = "       IDENTIFICATION DIVISION.\n" +
                "       PROGRAM-ID. SAMPLE.\n" +
                "       PROCEDURE DIVISION.\n" +
                "           EXEC SQL\n" +
                "               SELECT * FROM CUSTOMER\n" +
                "           END-EXEC.\n" +
                "           STOP RUN.";
        Path programFile = temp.resolve("SAMPLE.cob");
        Files.writeString(programFile, cobol);

        CobolParsingService parsingService = new CobolParsingService();
        JavaGenerationService javaGenerationService = new JavaGenerationService(parsingService);
        TemplateCodeGenerationService templateService = new TemplateCodeGenerationService();
        Db2MigrationService db2Service = new Db2MigrationService(parsingService);
        CobolRecipeRegistry recipeRegistry = new CobolRecipeRegistry();
        RecipeBasedMigrationPlanService migrationPlanService =
                new RecipeBasedMigrationPlanService(parsingService, javaGenerationService, recipeRegistry);
        IndexingService indexingService = new IndexingService();
        MetricsService metricsService = new MetricsService();
        CobolLanguageProvider provider = new CobolLanguageProvider(
                parsingService, javaGenerationService, migrationPlanService,
                indexingService, metricsService, templateService, db2Service);
        CobolMcpToolsProvider tools = new CobolMcpToolsProvider(provider);

        Map<String, Object> args = Map.of(
                "workspacePath", temp.toString(),
                "program", "SAMPLE.cob"
        );

        Object result = tools.executeCobolTool("cobol.db2.migrate", args);
        assertTrue(result instanceof Map);
        Map<?,?> resMap = (Map<?,?>) result;
        assertEquals(true, resMap.get("success"));
        Map<?,?> files = (Map<?,?>) resMap.get("files");
        assertTrue(files.containsKey("Customer.java"));
        assertTrue(files.containsKey("CustomerRepository.java"));
    }
}

