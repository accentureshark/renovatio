package org.shark.renovatio.web.controller;

import org.shark.renovatio.core.application.McpService;
import org.shark.renovatio.core.mcp.McpRequest;
import org.shark.renovatio.core.mcp.McpResponse;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.ResponseEntity;
import com.fasterxml.jackson.databind.ObjectMapper;

/**
 * MCPController exposes the /mcp endpoint for MCP-compliant (JSON-RPC 2.0) requests.
 * Delegates all logic to McpService in renovatio-core.
 */
@RestController
@RequestMapping("/mcp")
public class MCPController {

    private static final Logger logger = LoggerFactory.getLogger(MCPController.class);
    private final McpService mcpService;

    @Autowired
    public MCPController(McpService mcpService) {
        this.mcpService = mcpService;
    }

    /**
     * Handles MCP (JSON-RPC 2.0) requests and delegates to core MCP logic.
     * @param requestBody MCP JSON-RPC 2.0 request
     * @return MCP-compliant JSON-RPC 2.0 response
     */
    @PostMapping
    public ResponseEntity<String> handleMcpRequest(@RequestBody McpRequest requestBody) {
        logger.info("Received MCP request: {}", requestBody);
        McpResponse response = mcpService.handleMcpRequest(requestBody);
        // Log the result map for debugging
        logger.info("DEBUG MCP result: {}", response.getResult());
        try {
            ObjectMapper mapper = new ObjectMapper();
            // Serializar SIEMPRE la respuesta completa MCP/JSON-RPC
            String json = mapper.writeValueAsString(response);
            logger.info("Serialized MCP response JSON: {}", json);
            return ResponseEntity.ok().header("Content-Type", "application/json").body(json);
        } catch (Exception e) {
            logger.error("Error serializing MCP response", e);
            return ResponseEntity.internalServerError().body("{\"error\":\"Serialization error\"}");
        }
    }

    /**
     * Handles GET requests to /mcp and returns a message indicating POST is required.
     */
    @org.springframework.web.bind.annotation.GetMapping
    public String handleGetMcp() {
        return "MCP endpoint only supports POST (JSON-RPC 2.0).";
    }
}
