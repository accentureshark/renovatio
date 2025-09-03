package org.shark.renovatio.infrastructure;

import org.shark.renovatio.application.McpToolingService;
import org.shark.renovatio.domain.RefactorRequest;
import org.shark.renovatio.domain.RefactorResponse;
import org.shark.renovatio.domain.Tool;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@RequestMapping("/mcp")
public class McpController {

    @Autowired
    private McpToolingService mcpToolingService;

    @GetMapping("/tools")
    public List<Tool> listTools() {
        return mcpToolingService.getTools();
    }

    @PostMapping("/run/{toolName}")
    public RefactorResponse runTool(@PathVariable String toolName, @RequestBody RefactorRequest request) {
        return mcpToolingService.runTool(toolName, request);
    }
}
