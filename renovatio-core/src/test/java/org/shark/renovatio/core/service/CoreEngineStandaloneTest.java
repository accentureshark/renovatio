package org.shark.renovatio.core.service;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.BeforeEach;
import org.shark.renovatio.shared.domain.Tool;
import org.shark.renovatio.shared.spi.LanguageProvider;

import java.util.Set;
import java.util.List;
import java.util.Map;
import java.util.HashMap;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Test to demonstrate that the core engine can be used as a standalone library
 * without any MCP dependencies.
 */
public class CoreEngineStandaloneTest {

    private LanguageProviderRegistry languageProviderRegistry;

    @BeforeEach
    void setUp() {
        languageProviderRegistry = new LanguageProviderRegistry();
        // The core engine can be used without any MCP dependencies
    }

    @Test
    void testCoreEngineAsStandaloneLibrary() {
        // Test that we can use the core engine without MCP
        assertNotNull(languageProviderRegistry);
        
        // Get supported languages - should work without MCP
        Set<String> languages = languageProviderRegistry.getSupportedLanguages();
        assertNotNull(languages);
        
        // Generate protocol-agnostic tools
        List<Tool> tools = languageProviderRegistry.generateTools();
        assertNotNull(tools);
        assertFalse(tools.isEmpty());
        
        // Verify tools have the expected structure
        Tool firstTool = tools.get(0);
        assertNotNull(firstTool.getName());
        assertNotNull(firstTool.getDescription());
        assertNotNull(firstTool.getInputSchema());
    }

    @Test
    void testToolGeneration() {
        // Test that tools are generated correctly
        List<Tool> tools = languageProviderRegistry.generateTools();
        
        // Should have at least the common tools
        assertTrue(tools.size() >= 3); // nql.compile, common.index, common.search
        
        // Verify common tools exist
        boolean hasNqlCompile = tools.stream()
            .anyMatch(tool -> "nql.compile".equals(tool.getName()));
        boolean hasCommonIndex = tools.stream()
            .anyMatch(tool -> "common.index".equals(tool.getName()));
        boolean hasCommonSearch = tools.stream()
            .anyMatch(tool -> "common.search".equals(tool.getName()));
            
        assertTrue(hasNqlCompile, "Should have nql.compile tool");
        assertTrue(hasCommonIndex, "Should have common.index tool");
        assertTrue(hasCommonSearch, "Should have common.search tool");
    }

    @Test
    void testToolRouting() {
        // Test that tool routing works
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("question", "test question");
        
        Map<String, Object> result = languageProviderRegistry.routeToolCall("nql.compile", arguments);
        assertNotNull(result);
    }

    @Test
    void testProtocolAgnosticDesign() {
        // Test that the core uses protocol-agnostic abstractions
        List<Tool> tools = languageProviderRegistry.generateTools();
        
        for (Tool tool : tools) {
            // Verify that tools implement the generic Tool interface
            assertInstanceOf(org.shark.renovatio.shared.domain.Tool.class, tool);
            
            // Verify required properties
            assertNotNull(tool.getName(), "Tool name should not be null");
            assertNotNull(tool.getDescription(), "Tool description should not be null");
            assertNotNull(tool.getInputSchema(), "Tool input schema should not be null");
            assertNotNull(tool.getMetadata(), "Tool metadata should not be null");
        }
    }

    @Test
    void testCoreCanBeUsedAsMavenDependency() {
        // This test demonstrates that the core can be used as a Maven dependency
        // without requiring any MCP server infrastructure
        
        // Create registry
        LanguageProviderRegistry registry = new LanguageProviderRegistry();
        
        // Use core functionality
        var tools = registry.generateTools();
        var languages = registry.getSupportedLanguages();
        
        // Verify it works without Spring Boot or MCP server
        assertNotNull(tools);
        assertNotNull(languages);
        
        // This proves the core is a clean library that can be embedded
        // in any application type (Spring Boot, standalone, etc.)
        System.out.println("✅ Core engine successfully used as standalone library");
        System.out.println("✅ Generated " + tools.size() + " tools");
        System.out.println("✅ Supports " + languages.size() + " languages");
    }
}