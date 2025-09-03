package org.shark.renovatio.infrastructure;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.shark.renovatio.application.McpToolingService;
import org.shark.renovatio.domain.RefactorRequest;
import org.shark.renovatio.domain.RefactorResponse;
import org.shark.renovatio.domain.Tool;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@RequestMapping("/mcp")
@Tag(name = "MCP")
public class McpController {

    @Autowired
    private McpToolingService mcpToolingService;

    @GetMapping("/spec")
    @Operation(summary = "Obtiene la especificación MCP")
    public String getSpec() {
        return mcpToolingService.getSpec();
    }

    @GetMapping("/tools")
    @Operation(summary = "Lista las herramientas disponibles")
    public List<Tool> listTools() {
        return mcpToolingService.getTools();
    }

    @PostMapping("/run/{toolName}")
    @Operation(summary = "Ejecuta una herramienta específica")
    public RefactorResponse runTool(@PathVariable String toolName, @RequestBody RefactorRequest request) {
        return mcpToolingService.runTool(toolName, request);
    }

    @GetMapping("/initialize")
    @Operation(summary = "Inicializa el cliente MCP")
    public Object initialize() {
        return new java.util.HashMap<String, Object>() {{
            put("name", "Renovatio MCP Server");
            put("version", "1.0");
            put("tools", mcpToolingService.getTools());
            put("resources", mcpToolingService.getTools());
            put("capabilities", new java.util.HashMap<String, Object>() {{
                put("tools", true);
                put("resources", true);
            }});
            put("specUrl", "/mcp/spec");
        }};
    }

    @GetMapping("/resources")
    @Operation(summary = "Lista los recursos disponibles (tools)")
    public Object listResources() {
        return new java.util.HashMap<String, Object>() {{
            put("resources", mcpToolingService.getTools());
        }};
    }

    @GetMapping("/resource")
    @Operation(summary = "Lista los recursos disponibles (tools, alias singular)")
    public Object listResource() {
        return listResources();
    }

    @GetMapping("")
    @Operation(summary = "Inicializa el cliente MCP (root)")
    public Object initializeRoot() {
        return initialize();
    }
}
