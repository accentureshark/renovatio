package org.shark.renovatio.mcp.server.model;

import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * Representation of an issue reported by MCP tools.
 */
public record Issue(@JsonProperty("file") String file,
                    @JsonProperty("line") Integer line,
                    @JsonProperty("severity") String severity,
                    @JsonProperty("type") String type,
                    @JsonProperty("message") String message,
                    @JsonProperty("recipe") String recipe) {
}
