package org.shark.renovatio.core.infrastructure;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.shark.renovatio.core.application.McpService;
import org.shark.renovatio.core.mcp.*;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/")
@Tag(name = "MCP Protocol")
public class McpProtocolController {

    @Autowired
    private McpService mcpService;

    @PostMapping("/")
    @Operation(summary = "MCP JSON-RPC 2.0 endpoint")
    public McpResponse handleMcpRequest(@RequestBody McpRequest request) {
        return mcpService.handleMcpRequest(request);
    }
}