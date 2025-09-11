package org.shark.renovatio.mcp.server.service;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.BeforeEach;
import org.shark.renovatio.mcp.server.model.*;
import org.shark.renovatio.core.service.LanguageProviderRegistry;

import java.util.HashMap;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Test for MCP Protocol Service to verify MCP compliance
 * (Unit test without Spring Boot context)
 */
public class McpProtocolServiceTest {

    private McpProtocolService mcpProtocolService;
    private McpToolingService mcpToolingService;
    private LanguageProviderRegistry languageProviderRegistry;

    @BeforeEach
    void setUp() {
        languageProviderRegistry = new LanguageProviderRegistry();
        McpToolAdapter toolAdapter = new McpToolAdapter();
        mcpToolingService = new McpToolingService(languageProviderRegistry, toolAdapter);
        mcpProtocolService = new McpProtocolService(mcpToolingService, languageProviderRegistry);
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
}