package org.shark.renovatio.shared.nql;

import java.util.List;
import java.util.Map;

/**
 * Result of NQL compilation (Natural Language -> NQL)
 */
public class NqlCompileResult {
    private boolean success;
    private NqlQuery query;
    private String reasoning;
    private List<String> errors;
    private Map<String, Object> metadata;
    
    public NqlCompileResult() {}
    
    public NqlCompileResult(boolean success, NqlQuery query, String reasoning) {
        this.success = success;
        this.query = query;
        this.reasoning = reasoning;
    }
    
    // Getters and setters
    public boolean isSuccess() { return success; }
    public void setSuccess(boolean success) { this.success = success; }
    
    public NqlQuery getQuery() { return query; }
    public void setQuery(NqlQuery query) { this.query = query; }
    
    public String getReasoning() { return reasoning; }
    public void setReasoning(String reasoning) { this.reasoning = reasoning; }
    
    public List<String> getErrors() { return errors; }
    public void setErrors(List<String> errors) { this.errors = errors; }
    
    public Map<String, Object> getMetadata() { return metadata; }
    public void setMetadata(Map<String, Object> metadata) { this.metadata = metadata; }
}