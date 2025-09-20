package org.shark.renovatio.mcp.server.model;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

import java.util.List;
import java.util.Objects;

/**
 * Wrapper for tool execution results returned to MCP clients.
 */
public record ToolCallResult(@JsonProperty("content") List<TextContent> content,
                             @JsonProperty("structuredContent") Object structuredContent,
                             @JsonProperty("isError") boolean isError) {

    @JsonCreator
    public ToolCallResult {
        content = Objects.requireNonNullElse(content, List.of(new TextContent("")));
    }

    public static ToolCallResult ok(String summary, Object structured) {
        return new ToolCallResult(List.of(new TextContent(summary)), structured, false);
    }

    public static ToolCallResult error(String message) {
        return new ToolCallResult(List.of(new TextContent(message)), null, true);
    }

    public static ToolCallResult error(String message, Object structured) {
        return new ToolCallResult(List.of(new TextContent(message)), structured, true);
    }
}
