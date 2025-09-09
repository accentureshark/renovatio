package org.shark.renovatio.shared.domain;

import java.util.Map;

/**
 * Result of diff operation
 */
public class DiffResult extends ProviderResult {
    private String unifiedDiff;
    private Map<String, Object> semanticDiff;
    private Map<String, Object> hunks;
    
    public DiffResult() {}
    
    public DiffResult(boolean success, String message) {
        super(success, message);
    }
    
    public String getUnifiedDiff() { return unifiedDiff; }
    public void setUnifiedDiff(String unifiedDiff) { this.unifiedDiff = unifiedDiff; }
    
    public Map<String, Object> getSemanticDiff() { return semanticDiff; }
    public void setSemanticDiff(Map<String, Object> semanticDiff) { this.semanticDiff = semanticDiff; }
    
    public Map<String, Object> getHunks() { return hunks; }
    public void setHunks(Map<String, Object> hunks) { this.hunks = hunks; }
}