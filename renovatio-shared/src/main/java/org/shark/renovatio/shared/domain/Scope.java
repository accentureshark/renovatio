package org.shark.renovatio.shared.domain;

import java.util.List;
import java.util.Map;

/**
 * Scope definition for operations
 */
public class Scope {
    private List<String> paths;
    private List<String> includePatterns;
    private List<String> excludePatterns;
    private Map<String, Object> properties;
    
    public Scope() {}
    
    public Scope(List<String> paths) {
        this.paths = paths;
    }
    
    // Getters and setters
    public List<String> getPaths() { return paths; }
    public void setPaths(List<String> paths) { this.paths = paths; }
    
    public List<String> getIncludePatterns() { return includePatterns; }
    public void setIncludePatterns(List<String> includePatterns) { this.includePatterns = includePatterns; }
    
    public List<String> getExcludePatterns() { return excludePatterns; }
    public void setExcludePatterns(List<String> excludePatterns) { this.excludePatterns = excludePatterns; }
    
    public Map<String, Object> getProperties() { return properties; }
    public void setProperties(Map<String, Object> properties) { this.properties = properties; }
}