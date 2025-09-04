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

    @GetMapping("/run")
    @Operation(summary = "Devuelve la lista de herramientas disponibles o ayuda para /run")
    public Object getRunHelp() {
        return new java.util.HashMap<String, Object>() {{
            put("message", "Usa POST /mcp/run/{toolName} para ejecutar una herramienta. Usa GET /mcp/run/{toolName} para ver la definición de la herramienta.");
            put("tools", mcpToolingService.getTools());
        }};
    }

    @GetMapping("/run/{toolName}")
    @Operation(summary = "Devuelve la definición de una herramienta específica")
    public Object getToolDefinition(@PathVariable String toolName) {
        Tool found = mcpToolingService.getTools().stream()
            .filter(t -> t.getName().equalsIgnoreCase(toolName))
            .findFirst().orElse(null);
        if (found == null) {
            return new java.util.HashMap<String, Object>() {{
                put("error", "Tool not found: " + toolName);
            }};
        }
        return found;
    }

    @GetMapping("/tool")
    @Operation(summary = "Lista las herramientas disponibles (alias singular)")
    public Object listTool() {
        return mcpToolingService.getTools();
    }

    @GetMapping("/list")
    @Operation(summary = "Lista las herramientas disponibles (alias /list)")
    public Object listList() {
        return new java.util.HashMap<String, Object>() {{
            put("items", mcpToolingService.getTools());
        }};
    }

    @GetMapping("/available")
    @Operation(summary = "Lista las herramientas disponibles (alias /available)")
    public Object listAvailable() {
        return new java.util.HashMap<String, Object>() {{
            put("resources", mcpToolingService.getTools());
        }};
    }

    @PostMapping("/tools")
    @Operation(summary = "Lista las herramientas disponibles (POST)")
    public Object listToolsPost() {
        return mcpToolingService.getTools();
    }

    @PostMapping("/resources")
    @Operation(summary = "Lista los recursos disponibles (POST)")
    public Object listResourcesPost() {
        return new java.util.HashMap<String, Object>() {{
            put("resources", mcpToolingService.getTools());
        }};
    }

    @PostMapping("/tool")
    @Operation(summary = "Lista las herramientas disponibles (POST, alias singular)")
    public Object listToolPost() {
        return mcpToolingService.getTools();
    }

    @PostMapping("/list")
    @Operation(summary = "Lista las herramientas disponibles (POST, alias /list)")
    public Object listListPost() {
        return new java.util.HashMap<String, Object>() {{
            put("items", mcpToolingService.getTools());
        }};
    }

    @PostMapping("/available")
    @Operation(summary = "Lista las herramientas disponibles (POST, alias /available)")
    public Object listAvailablePost() {
        return new java.util.HashMap<String, Object>() {{
            put("resources", mcpToolingService.getTools());
        }};
    }

    @PostMapping("/")
    public Object handleJsonRpc(@RequestBody java.util.Map<String, Object> body) {
        String jsonrpc = (String) body.getOrDefault("jsonrpc", "2.0");
        Object id = body.get("id");
        String method = (String) body.get("method");
        java.util.Map<String, Object> response = new java.util.HashMap<>();
        response.put("jsonrpc", jsonrpc);
        response.put("id", id);
        if ("tools/list".equals(method)) {
            java.util.Map<String, Object> result = new java.util.HashMap<>();
            result.put("tools", mcpToolingService.getTools());
            response.put("result", result);
        } else {
            java.util.Map<String, Object> error = new java.util.HashMap<>();
            error.put("code", -32601);
            error.put("message", "Method not found: " + method);
            response.put("error", error);
        }
        return response;
    }
}
