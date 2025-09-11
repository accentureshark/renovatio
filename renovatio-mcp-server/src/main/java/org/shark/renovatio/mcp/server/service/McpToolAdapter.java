package org.shark.renovatio.mcp.server.service;

import org.shark.renovatio.mcp.server.model.McpTool;
import org.shark.renovatio.shared.domain.Tool;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.stream.Collectors;

/**
 * Adapter that converts between protocol-agnostic Tool objects and MCP-specific McpTool objects.
 * This allows the core engine to remain protocol-agnostic while the MCP server can work with
 * MCP-specific objects.
 */
@Component
public class McpToolAdapter {
    
    /**
     * Convert a generic Tool to an MCP-specific McpTool
     */
    public McpTool toMcpTool(Tool tool) {
        McpTool mcpTool = new McpTool();
        mcpTool.setName(tool.getName());
        mcpTool.setDescription(tool.getDescription());
        mcpTool.setInputSchema(tool.getInputSchema());
        return mcpTool;
    }
    
    /**
     * Convert a list of generic Tools to MCP-specific McpTools
     */
    public List<McpTool> toMcpTools(List<Tool> tools) {
        return tools.stream()
                .map(this::toMcpTool)
                .collect(Collectors.toList());
    }
    
    /**
     * Convert an MCP-specific McpTool to a generic Tool
     */
    public Tool fromMcpTool(McpTool mcpTool) {
        return new org.shark.renovatio.shared.domain.BasicTool(
            mcpTool.getName(),
            mcpTool.getDescription(),
            mcpTool.getInputSchema()
        );
    }
    
    /**
     * Convert a list of MCP-specific McpTools to generic Tools
     */
    public List<Tool> fromMcpTools(List<McpTool> mcpTools) {
        return mcpTools.stream()
                .map(this::fromMcpTool)
                .collect(Collectors.toList());
    }
}