package org.shark.renovatio.shared.domain;

import java.util.Map;

/**
 * Universal tool definition that can be used across different protocol implementations.
 * This abstraction allows the core engine to be protocol-agnostic.
 */
public interface Tool {
    
    /**
     * Get the tool name/identifier
     */
    String getName();
    
    /**
     * Get the tool description
     */
    String getDescription();
    
    /**
     * Get the input schema for the tool (JSON Schema format)
     */
    Map<String, Object> getInputSchema();
    
    /**
     * Get additional metadata for the tool
     */
    Map<String, Object> getMetadata();
}