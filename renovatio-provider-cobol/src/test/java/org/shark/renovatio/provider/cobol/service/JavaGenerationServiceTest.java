package org.shark.renovatio.provider.cobol.service;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.BeforeEach;
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
        javaGenerationService = new JavaGenerationService(parsingService);
        
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
        assertTrue(result.isSuccess(), "Result should be successful, but got: " + result.getMessage());
        assertNotNull(result.getGeneratedCode());
        
        // Debug output
        if (result.getGeneratedCode() != null) {
            System.out.println("Generated files: " + result.getGeneratedCode().keySet());
        }
        
        assertFalse(result.getGeneratedCode().isEmpty());
        
        // Check that Java files were generated (adjust file names based on actual generation)
        boolean hasAnyDTOFile = result.getGeneratedCode().keySet().stream()
            .anyMatch(key -> key.contains("DTO.java"));
        boolean hasAnyServiceFile = result.getGeneratedCode().keySet().stream()
            .anyMatch(key -> key.contains("Service.java"));
        boolean hasAnyServiceImplFile = result.getGeneratedCode().keySet().stream()
            .anyMatch(key -> key.contains("ServiceImpl.java"));
            
        assertTrue(hasAnyDTOFile, "Should have generated a DTO file");
        assertTrue(hasAnyServiceFile, "Should have generated a Service file");
        assertTrue(hasAnyServiceImplFile, "Should have generated a ServiceImpl file");
        
        // Verify DTO contains expected content (since parsing is basic, just check for DTO structure)
        String dtoCode = result.getGeneratedCode().values().stream()
            .filter(code -> code.contains("DTO"))
            .findFirst()
            .orElse("");
        assertTrue(dtoCode.contains("DTO"), "Should contain DTO class");
        assertTrue(dtoCode.contains("class"), "Should contain class definition");
        assertTrue(dtoCode.contains("public"), "Should contain public methods");
    }
    
    @Test
    void testGenerateInterfaceStubsWithEmptyWorkspace() {
        NqlQuery query = new NqlQuery();
        query.setType(NqlQuery.QueryType.FIND);
        query.setTarget("stubs");
        
        StubResult result = javaGenerationService.generateInterfaceStubs(query, workspace);
        
        assertNotNull(result);
        assertTrue(result.isSuccess());
        assertNotNull(result.getGeneratedCode());
        // Should be empty but not null for empty workspace
        assertTrue(result.getGeneratedCode().isEmpty());
    }
}