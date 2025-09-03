package org.shark.renovatio.domain.mcp;

import io.swagger.v3.oas.annotations.media.Schema;
import java.util.List;

@Schema(description = "MCP Server capabilities")
public class McpCapabilities {
    @Schema(description = "Tools capability")
    private ToolsCapability tools;

    public McpCapabilities() {
        this.tools = new ToolsCapability();
    }

    public ToolsCapability getTools() {
        return tools;
    }

    public void setTools(ToolsCapability tools) {
        this.tools = tools;
    }

    public static class ToolsCapability {
        @Schema(description = "Whether server supports listing tools")
        private boolean listChanged = true;

        public boolean isListChanged() {
            return listChanged;
        }

        public void setListChanged(boolean listChanged) {
            this.listChanged = listChanged;
        }
    }
}