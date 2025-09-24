package org.shark.renovatio.mcp.server.controller;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.shark.renovatio.mcp.server.model.*;
import org.shark.renovatio.mcp.server.service.McpProtocolService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

/**
 * MCP Protocol Controller - handles JSON-RPC 2.0 requests according to MCP specification.
 * 
 * This controller implements the Model Content Protocol specification available at:
 * https://modelcontextprotocol.io/docs/develop/build-server
 */
@RestController
@RequestMapping("/")
@Tag(name = "MCP Protocol", description = "Model Content Protocol JSON-RPC 2.0 endpoint")
public class McpProtocolController {

    @Autowired
    private McpProtocolService mcpProtocolService;

    /**
     * Main MCP endpoint - handles all MCP JSON-RPC 2.0 requests.
     * This is the primary entry point for MCP clients.
     */
    @PostMapping
    @Operation(
        summary = "MCP JSON-RPC 2.0 endpoint", 
        description = "Handles all Model Content Protocol requests according to JSON-RPC 2.0 specification"
    )
    public McpResponse handleMcpRequest(@RequestBody McpRequest request) {
        return mcpProtocolService.handleMcpRequest(request);
    }
    
    /**
     * Health check endpoint for MCP server.
     */
    @GetMapping("/health")
    @Operation(summary = "Health check", description = "Check if MCP server is running")
    public String health() {
        return "MCP Server is running";
    }
}