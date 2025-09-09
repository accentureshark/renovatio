package org.shark.renovatio.core.mcp;

import io.swagger.v3.oas.annotations.media.Schema;

@Schema(description = "MCP Server capabilities")
public class McpCapabilities {
    @Schema(description = "Tools capability")
    private ToolsCapability tools;

    @Schema(description = "Prompts capability")
    private PromptsCapability prompts;

    @Schema(description = "Resources capability")
    private ResourcesCapability resources;

    public McpCapabilities() {
        this.tools = new ToolsCapability();
        this.prompts = new PromptsCapability();
        this.resources = new ResourcesCapability();
    }

    public ToolsCapability getTools() {
        return tools;
    }

    public void setTools(ToolsCapability tools) {
        this.tools = tools;
    }

    public PromptsCapability getPrompts() {
        return prompts;
    }

    public void setPrompts(PromptsCapability prompts) {
        this.prompts = prompts;
    }

    public ResourcesCapability getResources() {
        return resources;
    }

    public void setResources(ResourcesCapability resources) {
        this.resources = resources;
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

    public static class PromptsCapability {
        @Schema(description = "Whether server supports listing prompts")
        private boolean listChanged = true;

        public boolean isListChanged() {
            return listChanged;
        }

        public void setListChanged(boolean listChanged) {
            this.listChanged = listChanged;
        }
    }

    public static class ResourcesCapability {
        @Schema(description = "Whether server supports listing resources")
        private boolean listChanged = true;

        public boolean isListChanged() {
            return listChanged;
        }

        public void setListChanged(boolean listChanged) {
            this.listChanged = listChanged;
        }
    }
}