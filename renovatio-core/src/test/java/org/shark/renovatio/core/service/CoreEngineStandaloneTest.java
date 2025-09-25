package org.shark.renovatio.core.service;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.shark.renovatio.shared.domain.*;
import org.shark.renovatio.shared.nql.NqlQuery;
import org.shark.renovatio.shared.spi.LanguageProvider;

import java.util.*;

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

        // Register a mock provider for testing
        LanguageProvider mockProvider = new MockLanguageProvider();
        languageProviderRegistry.registerProvider(mockProvider);
    }

    @Test
    void testCoreEngineAsStandaloneLibrary() {
        // Test that we can use the core engine without MCP
        assertNotNull(languageProviderRegistry);

        // Get supported languages - should work without MCP
        Set<String> languages = languageProviderRegistry.getSupportedLanguages();
        assertNotNull(languages);
        assertFalse(languages.isEmpty()); // Should have our mock provider

        // Generate protocol-agnostic tools
        List<Tool> tools = languageProviderRegistry.generateTools();
        assertNotNull(tools);
        assertFalse(tools.isEmpty()); // Should have tools from mock provider

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

        // Should have tools from our mock provider (5 capabilities * 1 provider = 5 tools)
        assertTrue(tools.size() >= 5);

        // Check for expected tool patterns (language.capability)
        boolean hasAnalyze = tools.stream()
                .anyMatch(tool -> tool.getName().endsWith(".analyze"));

        assertTrue(hasAnalyze, "Should have analyze tool");

        // Test tool call routing with a valid tool name
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("workspacePath", "/test");
        arguments.put("nql", "test query");
        arguments.put("language", "mock");

        Map<String, Object> result = languageProviderRegistry.routeToolCall("mock.analyze", arguments);
        assertNotNull(result);
        assertTrue((Boolean) result.get("success"));
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

        // Create registry and register a provider manually
        LanguageProviderRegistry registry = new LanguageProviderRegistry();
        registry.registerProvider(new MockLanguageProvider());

        // Use core functionality
        var tools = registry.generateTools();
        var languages = registry.getSupportedLanguages();

        // Verify it works without Spring Boot or MCP server
        assertNotNull(tools);
        assertNotNull(languages);
        assertFalse(tools.isEmpty());
        assertFalse(languages.isEmpty());

        // This proves the core is a clean library that can be embedded
        // in any application type (Spring Boot, standalone, etc.)
        System.out.println("✅ Core engine successfully used as standalone library");
        System.out.println("✅ Generated " + tools.size() + " tools");
        System.out.println("✅ Supports " + languages.size() + " languages");
    }

    /**
     * Mock LanguageProvider for testing purposes
     */
    private static class MockLanguageProvider implements LanguageProvider {

        @Override
        public String language() {
            return "mock";
        }

        @Override
        public Set<LanguageProvider.Capabilities> capabilities() {
            return Set.of(
                    LanguageProvider.Capabilities.ANALYZE,
                    LanguageProvider.Capabilities.METRICS,
                    LanguageProvider.Capabilities.PLAN,
                    LanguageProvider.Capabilities.APPLY,
                    LanguageProvider.Capabilities.DIFF
            );
        }

        @Override
        public AnalyzeResult analyze(NqlQuery query, Workspace workspace) {
            AnalyzeResult result = new AnalyzeResult();
            result.setSuccess(true);
            result.setMessage("Mock analysis completed");
            result.setRunId("mock-run-" + System.currentTimeMillis());
            return result;
        }

        @Override
        public MetricsResult metrics(Scope scope, Workspace workspace) {
            MetricsResult result = new MetricsResult();
            result.setSuccess(true);
            result.setMessage("Mock metrics completed");
            Map<String, Number> metrics = new HashMap<>();
            metrics.put("files", 10.0);
            metrics.put("lines", 1000.0);
            result.setMetrics(metrics);
            return result;
        }

        @Override
        public PlanResult plan(NqlQuery query, Scope scope, Workspace workspace) {
            PlanResult result = new PlanResult();
            result.setSuccess(true);
            result.setMessage("Mock plan completed");
            result.setPlanId("mock-plan-" + System.currentTimeMillis());
            return result;
        }

        @Override
        public ApplyResult apply(String planId, boolean dryRun, Workspace workspace) {
            ApplyResult result = new ApplyResult();
            result.setSuccess(true);
            result.setMessage("Mock apply completed");
            result.setDryRun(dryRun);
            return result;
        }

        @Override
        public DiffResult diff(String runId, Workspace workspace) {
            DiffResult result = new DiffResult();
            result.setSuccess(true);
            result.setMessage("Mock diff completed");
            result.setUnifiedDiff("mock diff content");
            return result;
        }

        @Override
        public Optional<StubResult> generateStubs(NqlQuery query, Workspace workspace) {
            StubResult result = new StubResult();
            result.setSuccess(true);
            result.setMessage("Mock stubs generated");
            return Optional.of(result);
        }

        @Override
        public List<org.shark.renovatio.shared.domain.Tool> getTools() {
            Map<String, Object> inputSchema = new HashMap<>();
            inputSchema.put("type", "object");
            inputSchema.put("properties", Map.of("workspacePath", Map.of("type", "string")));
            inputSchema.put("required", List.of("workspacePath"));
            return List.of(
                    new org.shark.renovatio.shared.domain.BasicTool("mock.analyze", "Mock analyze tool", inputSchema),
                    new org.shark.renovatio.shared.domain.BasicTool("mock.metrics", "Mock metrics tool", inputSchema),
                    new org.shark.renovatio.shared.domain.BasicTool("mock.plan", "Mock plan tool", inputSchema),
                    new org.shark.renovatio.shared.domain.BasicTool("mock.apply", "Mock apply tool", inputSchema),
                    new org.shark.renovatio.shared.domain.BasicTool("mock.diff", "Mock diff tool", inputSchema)
            );
        }
    }
}