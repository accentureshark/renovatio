package org.shark.renovatio.provider.cobol.service;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.shark.renovatio.shared.domain.StubResult;
import org.shark.renovatio.shared.domain.Workspace;
import org.shark.renovatio.shared.nql.NqlQuery;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Tests for Java generation service
 */
class JavaGenerationServiceTest {

    @TempDir
    Path tempDir;

    private JavaGenerationService javaGenerationService;
    private CobolParsingService parsingService;
    private Workspace workspace;

    @BeforeEach
    void setUp() {
        parsingService = new CobolParsingService();
        TemplateCodeGenerationService templateService = new TemplateCodeGenerationService();
        javaGenerationService = new JavaGenerationService(parsingService, templateService);
        workspace = new Workspace();
        workspace.setId("test");
        workspace.setPath(tempDir.toString());
        workspace.setBranch("main");
    }

    @Test
    void testGenerateInterfaceStubsWithSampleCobol() throws IOException {
        // Create a sample COBOL file
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
                    DISPLAY "Hello World".
                    STOP RUN.
                """;

        Path cobolFile = tempDir.resolve("sample.cob");
        Files.writeString(cobolFile, cobolContent);

        NqlQuery query = new NqlQuery();
        query.setType(NqlQuery.QueryType.FIND);
        query.setTarget("stubs");
        query.setLanguage("cobol");

        StubResult result = javaGenerationService.generateInterfaceStubs(query, workspace);

        assertNotNull(result);
        assertTrue(result.isSuccess());
        assertNotNull(result.getGeneratedCode());
        assertFalse(result.getGeneratedCode().isEmpty());

        // Check that Java files were generated
        assertTrue(result.getGeneratedCode().containsKey("SampleDTO.java"));
        assertTrue(result.getGeneratedCode().containsKey("SampleService.java"));
        assertTrue(result.getGeneratedCode().containsKey("SampleServiceImpl.java"));

        // Verify DTO contains expected fields
        String dtoCode = result.getGeneratedCode().get("SampleDTO.java");
        System.out.println("DTO generado:\n" + dtoCode);
        assertTrue(dtoCode.contains("class SampleDTO"));
        assertTrue(dtoCode.contains("String wsName"));
        assertTrue(dtoCode.contains("Integer wsAge"));
        assertTrue(dtoCode.contains("BigDecimal wsSalary"));
    }

    @Test
    void testGenerateInterfaceStubsWithEmptyWorkspace() {
        NqlQuery query = new NqlQuery();
        query.setType(NqlQuery.QueryType.FIND);
        query.setTarget("stubs");

        StubResult result = javaGenerationService.generateInterfaceStubs(query, workspace);

        assertNotNull(result);
        assertFalse(result.isSuccess());
        assertNotNull(result.getGeneratedCode());
        // Should be empty but not null for empty workspace
        assertTrue(result.getGeneratedCode().isEmpty());
    }
}