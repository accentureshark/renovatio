package org.shark.renovatio.shared.domain;

import java.util.Map;

/**
 * Result of apply operation
 */
public class ApplyResult extends ProviderResult {
    private String diff;
    private Map<String, Object> changes;
    private boolean dryRun;
    
    public ApplyResult() {}
    
    public ApplyResult(boolean success, String message) {
        super(success, message);
    }
    
    public String getDiff() { return diff; }
    public void setDiff(String diff) { this.diff = diff; }
    
    public Map<String, Object> getChanges() { return changes; }
    public void setChanges(Map<String, Object> changes) { this.changes = changes; }
    
    public boolean isDryRun() { return dryRun; }
    public void setDryRun(boolean dryRun) { this.dryRun = dryRun; }
}