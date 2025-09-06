package org.shark.renovatio.core.infrastructure;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.shark.renovatio.core.application.McpToolingService;
import org.shark.renovatio.core.mcp.McpPrompt;
import org.shark.renovatio.core.mcp.McpResource;
import org.shark.renovatio.core.mcp.McpTool;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.Map;

@RestController
@RequestMapping("/mcp")
@Tag(name = "MCP")
public class McpController {

    @Autowired
    private McpToolingService mcpToolingService;

    @GetMapping("/spec")
    @Operation(summary = "Get MCP specification")
    public String getSpec() {
        return mcpToolingService.getSpec();
    }

    @GetMapping("/tools")
    @Operation(summary = "List available tools")
    public List<McpTool> listTools() {
        return mcpToolingService.getMcpTools();
    }

    @PostMapping("/run/{toolName}")
    @Operation(summary = "Execute a specific tool")
    public Map<String, Object> runTool(@PathVariable String toolName, @RequestBody Map<String, Object> arguments) {
        return mcpToolingService.executeTool(toolName, arguments);
    }

    @GetMapping("/prompts")
    @Operation(summary = "List available prompts")
    public List<McpPrompt> listPrompts() {
        return mcpToolingService.getPrompts();
    }

    @GetMapping("/prompts/{name}")
    @Operation(summary = "Get specific prompt")
    public McpPrompt getPrompt(@PathVariable String name) {
        return mcpToolingService.getPrompt(name);
    }

    @GetMapping("/resources")
    @Operation(summary = "List available resources")
    public List<McpResource> listResources() {
        return mcpToolingService.getResources();
    }

    @GetMapping("/resources/{uri}")
    @Operation(summary = "Get specific resource")
    public McpResource getResource(@PathVariable String uri) {
        return mcpToolingService.getResource("file://" + uri);
    }
}
