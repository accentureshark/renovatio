package org.shark.renovatio.mcp.server.service;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.BeforeEach;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.shark.renovatio.mcp.server.model.*;
import org.shark.renovatio.mcp.server.model.ToolCallResult;
import org.shark.renovatio.core.service.LanguageProviderRegistry;

import java.util.HashMap;
import java.util.Map;
import java.util.List;
import java.util.ArrayList;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

/**
 * Test for MCP Protocol Service to verify MCP compliance
 * (Unit test without Spring Boot context)
 */
public class McpProtocolServiceTest {

    private McpProtocolService mcpProtocolService;
    @Mock
    private McpToolingService mcpToolingService;
    @Mock
    private LanguageProviderRegistry languageProviderRegistry;
    @Mock
    private McpToolAdapter toolAdapter;

    @BeforeEach
    void setUp() {
        MockitoAnnotations.openMocks(this);

        // Mock the basic dependencies
        // Simulate a real MCP tool with parameters for testing
        List<McpTool> mockTools = new ArrayList<>();
        McpTool tool = new McpTool();
        tool.setName("java_analyze");
        tool.setDescription("Analyze for java");
        List<Map<String, Object>> params = new ArrayList<>();
        Map<String, Object> param = new HashMap<>();
        param.put("name", "workspacePath");
        param.put("type", "string");
        param.put("description", "Path to the workspace directory to analyze");
        param.put("required", true);
        params.add(param);
        tool.setParameters(params);
        tool.setExample(Map.of("workspacePath", "/tmp/project"));
        Map<String, Object> metadata = new HashMap<>();
        metadata.put("displayName", "Java Analyze");
        metadata.put("tags", List.of("java", "analysis"));
        tool.setMetadata(metadata);
        mockTools.add(tool);
        when(mcpToolingService.getMcpTools()).thenReturn(mockTools);
        when(mcpToolingService.getSupportedLanguages()).thenReturn(java.util.Set.of("java"));
        when(mcpToolingService.executeToolWithEnvelope(anyString(), anyMap())).thenReturn(
                ToolCallResult.ok("stub summary", Map.of("success", true))
        );
        when(mcpToolingService.getTool(anyString())).thenReturn(tool);

        mcpProtocolService = new McpProtocolService(mcpToolingService);
    }

    @Test
    void testInitializeMethod() {
        // Test MCP initialize method
        McpRequest request = new McpRequest();
        request.setId("test-1");
        request.setMethod("initialize");

        McpResponse response = mcpProtocolService.handleMcpRequest(request);

        assertNotNull(response);
        assertEquals("test-1", response.getId());
        assertNotNull(response.getResult());

        @SuppressWarnings("unchecked")
        Map<String, Object> result = (Map<String, Object>) response.getResult();
        assertEquals("2025-06-18", result.get("protocolVersion"));
        assertNotNull(result.get("capabilities"));
        assertNotNull(result.get("serverInfo"));
        assertNotNull(result.get("availableTools"));
        @SuppressWarnings("unchecked")
        List<McpTool> availableTools = (List<McpTool>) result.get("availableTools");
        assertFalse(availableTools.isEmpty());
        assertEquals("Java Analyze", availableTools.get(0).getMetadata().get("displayName"));
    }

    @Test
    void testPingMethod() {
        // Test MCP ping method
        McpRequest request = new McpRequest();
        request.setId("test-2");
        request.setMethod("ping");

        McpResponse response = mcpProtocolService.handleMcpRequest(request);

        assertNotNull(response);
        assertEquals("test-2", response.getId());
        assertNotNull(response.getResult());

        @SuppressWarnings("unchecked")
        Map<String, Object> result = (Map<String, Object>) response.getResult();
        assertEquals("pong", result.get("status"));
        assertNotNull(result.get("timestamp"));
    }

    @Test
    void testToolsList() {
        // Test MCP tools/list method
        McpRequest request = new McpRequest();
        request.setId("test-3");
        request.setMethod("tools/list");

        McpResponse response = mcpProtocolService.handleMcpRequest(request);

        assertNotNull(response);
        assertEquals("test-3", response.getId());
        assertNotNull(response.getResult());

        @SuppressWarnings("unchecked")
        Map<String, Object> result = (Map<String, Object>) response.getResult();
        assertNotNull(result.get("tools"));

        @SuppressWarnings("unchecked")
        List<McpTool> tools = (List<McpTool>) result.get("tools");
        assertFalse(tools.isEmpty(), "Tools list should not be empty");
        for (McpTool tool : tools) {
            assertNotNull(tool.getName());
            assertNotNull(tool.getParameters());
            assertTrue(tool.getParameters() instanceof List);
            List<Map<String, Object>> params = tool.getParameters();
            for (Map<String, Object> param : params) {
                assertNotNull(param.get("name"));
                assertNotNull(param.get("type"));
                assertNotNull(param.get("description"));
                assertTrue(param.containsKey("required"));
            }
            assertEquals("Java Analyze", tool.getMetadata().get("displayName"));
        }
    }

    @Test
    void testToolsDescribeIncludesMetadata() {
        McpRequest request = new McpRequest();
        request.setId("test-8");
        request.setMethod("tools/describe");
        Map<String, Object> params = new HashMap<>();
        params.put("name", "java.analyze");
        request.setParams(params);

        McpResponse response = mcpProtocolService.handleMcpRequest(request);

        assertNotNull(response);
        assertEquals("test-8", response.getId());
        assertNotNull(response.getResult());

        @SuppressWarnings("unchecked")
        Map<String, Object> result = (Map<String, Object>) response.getResult();
        McpTool describedTool = (McpTool) result.get("tool");
        assertNotNull(describedTool);
        assertEquals("Java Analyze", describedTool.getMetadata().get("displayName"));
        assertEquals(List.of("java", "analysis"), describedTool.getMetadata().get("tags"));
    }

    @Test
    void testCapabilities() {
        // Test MCP capabilities method
        McpRequest request = new McpRequest();
        request.setId("test-4");
        request.setMethod("capabilities");

        McpResponse response = mcpProtocolService.handleMcpRequest(request);

        assertNotNull(response);
        assertEquals("test-4", response.getId());
        assertNotNull(response.getResult());

        @SuppressWarnings("unchecked")
        Map<String, Object> result = (Map<String, Object>) response.getResult();
        assertNotNull(result.get("capabilities"));
    }

    @Test
    void testServerInfo() {
        // Test MCP server/info method
        McpRequest request = new McpRequest();
        request.setId("test-5");
        request.setMethod("server/info");

        McpResponse response = mcpProtocolService.handleMcpRequest(request);

        assertNotNull(response);
        assertEquals("test-5", response.getId());
        assertNotNull(response.getResult());

        @SuppressWarnings("unchecked")
        Map<String, Object> result = (Map<String, Object>) response.getResult();
        @SuppressWarnings("unchecked")
        Map<String, Object> serverInfo = (Map<String, Object>) result.get("serverInfo");
        assertEquals("Renovatio MCP Server", serverInfo.get("name"));
        assertEquals("1.0.0", serverInfo.get("version"));
        assertEquals("2025-06-18", serverInfo.get("protocolVersion"));
    }

    @Test
    void testInvalidMethod() {
        // Test invalid method handling
        McpRequest request = new McpRequest();
        request.setId("test-6");
        request.setMethod("invalid/method");

        McpResponse response = mcpProtocolService.handleMcpRequest(request);

        assertNotNull(response);
        assertEquals("test-6", response.getId());
        assertNotNull(response.getError());
        assertEquals(-32601, response.getError().getCode());
        assertTrue(response.getError().getMessage().contains("Method not found"));
    }

    @Test
    void testToolsCallEnvelope() {
        McpRequest request = new McpRequest();
        request.setId("test-7");
        request.setMethod("tools/call");

        Map<String, Object> params = new HashMap<>();
        params.put("name", "java.analyze");
        params.put("arguments", Map.of("workspacePath", "/tmp/project"));
        request.setParams(params);

        ToolCallResult expected = ToolCallResult.ok("Analyzed 10 files", Map.of("success", true));
        when(mcpToolingService.executeToolWithEnvelope(eq("java_analyze"), anyMap())).thenReturn(expected);

        McpResponse response = mcpProtocolService.handleMcpRequest(request);

        assertNotNull(response);
        assertEquals("test-7", response.getId());
        assertNull(response.getError());
        assertEquals(expected, response.getResult());
    }
}