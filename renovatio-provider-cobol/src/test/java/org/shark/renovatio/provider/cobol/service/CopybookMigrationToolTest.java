package org.shark.renovatio.provider.cobol.service;

import org.junit.jupiter.api.Test;
import org.shark.renovatio.provider.cobol.CobolLanguageProvider;
import org.shark.renovatio.provider.cobol.infrastructure.CobolMcpToolsProvider;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

public class CopybookMigrationToolTest {

    @Test
    void migrateCopybookGeneratesArtifacts() throws Exception {
        Path temp = Files.createTempDirectory("cobol-copybook");
        String copybook = "       01 CUSTOMER-REC.\n" +
                "          05 CUSTOMER-ID PIC X(10).\n" +
                "          05 CUSTOMER-AGE PIC 9(3).";
        Path copybookFile = temp.resolve("CUSTOMER.cpy");
        Files.writeString(copybookFile, copybook);

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
                "copybook", "CUSTOMER.cpy"
        );

        Object result = tools.executeCobolTool("cobol.copybook.migrate", args);
        assertTrue(result instanceof Map);
        Map<?,?> resMap = (Map<?,?>) result;
        assertEquals(true, resMap.get("success"));
        Map<?,?> files = (Map<?,?>) resMap.get("files");
        assertTrue(files.containsKey("CustomerDTO.java"));
        assertTrue(files.containsKey("CustomerService.java"));
    }
}
