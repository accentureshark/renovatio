package org.shark.renovatio.mcp.server.model;

import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * Representation of a change preview or applied modification.
 */
public record Change(@JsonProperty("file") String file,
                     @JsonProperty("diff") String diff) {
}
