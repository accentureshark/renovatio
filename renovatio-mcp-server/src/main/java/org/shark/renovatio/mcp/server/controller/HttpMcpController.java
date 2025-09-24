package org.shark.renovatio.mcp.server.controller;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.shark.renovatio.mcp.server.service.McpProtocolService;
import org.shark.renovatio.mcp.server.model.McpRequest;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.*;

import java.util.HashMap;
import java.util.Map;

/**
 * HTTP MCP Controller - handles MCP JSON-RPC 2.0 requests via HTTP transport.
 * This enables HTTP-based MCP clients to communicate with the server.
 */
@RestController
@RequestMapping("/mcp")
public class HttpMcpController {
    
    @Autowired
    private McpProtocolService mcpProtocolService;
    
    private final ObjectMapper objectMapper = new ObjectMapper();
    
    /**
     * Handle MCP JSON-RPC 2.0 requests via HTTP POST
     */
    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
    public Object handleMcpRequest(@RequestBody Map<String, Object> body) {
        try {
            // Convert HTTP request to MCP request format
            McpRequest mcpRequest = new McpRequest();
            mcpRequest.setJsonrpc((String) body.getOrDefault("jsonrpc", "2.0"));

            // Handle id field - can be String, Integer, or null according to JSON-RPC 2.0
            Object idObject = body.get("id");
            String id = null;
            if (idObject != null) {
                id = idObject.toString(); // Convert Integer, String, or other types to String
            }
            mcpRequest.setId(id);

            mcpRequest.setMethod((String) body.get("method"));
            mcpRequest.setParams(body.get("params"));
            
            // Process the request through the MCP protocol service
            var response = mcpProtocolService.handleMcpRequest(mcpRequest);
            
            // Convert MCP response to HTTP JSON response
            Map<String, Object> jsonResponse = new HashMap<>();
            jsonResponse.put("jsonrpc", response.getJsonrpc());

            // Return id in the same format as received (preserve original type if possible)
            if (response.getId() != null && idObject instanceof Integer) {
                try {
                    jsonResponse.put("id", Integer.valueOf(response.getId()));
                } catch (NumberFormatException e) {
                    jsonResponse.put("id", response.getId()); // Fallback to string
                }
            } else {
                jsonResponse.put("id", response.getId());
            }

            if (response.getError() != null) {
                jsonResponse.put("error", response.getError());
            } else {
                jsonResponse.put("result", response.getResult());
            }
            
            return jsonResponse;
            
        } catch (Exception e) {
            // Return JSON-RPC 2.0 error response
            Map<String, Object> errorResponse = new HashMap<>();
            errorResponse.put("jsonrpc", "2.0");

            // Handle id in error response as well
            Object idObject = body.get("id");
            if (idObject instanceof Integer) {
                errorResponse.put("id", idObject);
            } else if (idObject != null) {
                errorResponse.put("id", idObject.toString());
            } else {
                errorResponse.put("id", null);
            }

            Map<String, Object> error = new HashMap<>();
            error.put("code", -32603);
            error.put("message", "Internal error: " + e.getMessage());
            errorResponse.put("error", error);
            
            return errorResponse;
        }
    }
    
    /**
     * Health check endpoint
     */
    @GetMapping("/health")
    public Map<String, Object> health() {
        Map<String, Object> health = new HashMap<>();
        health.put("status", "UP");
        health.put("server", "Renovatio MCP Server");
        health.put("timestamp", System.currentTimeMillis());
        return health;
    }
}
