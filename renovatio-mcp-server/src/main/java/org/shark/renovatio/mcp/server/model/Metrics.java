package org.shark.renovatio.mcp.server.model;

import com.fasterxml.jackson.annotation.JsonProperty;

import java.util.Map;

/**
 * Aggregated metrics reported by tools.
 */
public record Metrics(@JsonProperty("values") Map<String, Object> values) {
}
