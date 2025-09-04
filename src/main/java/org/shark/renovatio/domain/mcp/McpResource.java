package org.shark.renovatio.domain.mcp;

import io.swagger.v3.oas.annotations.media.Schema;

@Schema(description = "MCP Resource metadata")
public class McpResource {
    @Schema(description = "Resource URI")
    private String uri;

    @Schema(description = "Resource name")
    private String name;

    @Schema(description = "MIME type")
    private String mimeType;

    @Schema(description = "Resource text content")
    private String text;

    public McpResource() {
    }

    public McpResource(String uri, String name, String mimeType, String text) {
        this.uri = uri;
        this.name = name;
        this.mimeType = mimeType;
        this.text = text;
    }

    public String getUri() {
        return uri;
    }

    public void setUri(String uri) {
        this.uri = uri;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getMimeType() {
        return mimeType;
    }

    public void setMimeType(String mimeType) {
        this.mimeType = mimeType;
    }

    public String getText() {
        return text;
    }

    public void setText(String text) {
        this.text = text;
    }
}
