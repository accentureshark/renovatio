package org.shark.renovatio.mcp.server.service;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.shark.renovatio.mcp.server.model.McpTool;
import org.shark.renovatio.mcp.server.model.ToolCallResult;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.*;

@SpringBootTest(classes = org.shark.renovatio.mcp.server.McpServerApplication.class)
class McpToolingServiceIntegrationTest {

    @Autowired
    private McpToolingService toolingService;

    @TempDir
    Path workspace;

    @BeforeEach
    void setUpWorkspace() throws IOException {
        Path src = workspace.resolve("src/main/java/com/example");
        Files.createDirectories(src);
        Files.writeString(src.resolve("Example.java"), "package com.example; class Example {}");
        Files.writeString(workspace.resolve("pom.xml"), "<project><modelVersion>4.0.0</modelVersion></project>");
    }

    @Test
    void toolsListContainsCuratedCatalog() {
        List<McpTool> tools = toolingService.getMcpTools();
        assertFalse(tools.isEmpty());
        // Updated upper bound after enabling additional language providers (Java + COBOL (+ optional JCL))
        assertTrue(tools.size() <= 25, "Tool catalog unexpectedly large (" + tools.size() + ")");
        Set<String> names = tools.stream().map(McpTool::getName).collect(Collectors.toSet());
        assertTrue(names.contains("java_discover"));
        assertTrue(names.contains("java_analyze"));
        // If COBOL provider is present ensure its core analyze tool is exposed
        if (names.stream().anyMatch(n -> n.startsWith("cobol_"))) {
            assertTrue(names.contains("cobol_analyze"), "Expected cobol_analyze tool when COBOL provider active");
        }
        assertFalse(names.stream().anyMatch(name -> name.startsWith("java_apply_")), "Recipes should not be exposed individually");

        McpTool analyze = tools.stream().filter(tool -> tool.getName().equals("java_analyze")).findFirst().orElseThrow();
        assertNotNull(analyze.getInputSchema());
        assertNotNull(analyze.getOutputSchema());
    }

    @Test
    void toolCallProducesEnvelope() {
        ToolCallResult result = toolingService.executeToolWithEnvelope("java_discover",
            Map.of("workspacePath", workspace.toString()));
        assertNotNull(result);
        assertFalse(result.isError());
        assertFalse(result.content().isEmpty());
        assertTrue(result.content().get(0).text().contains("discover"));
        assertTrue(result.structuredContent() instanceof Map);
        @SuppressWarnings("unchecked")
        Map<String, Object> structured = (Map<String, Object>) result.structuredContent();
        assertTrue(structured.containsKey("modules"));
    }
}
