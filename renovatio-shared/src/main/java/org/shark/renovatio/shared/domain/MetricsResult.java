package org.shark.renovatio.shared.domain;

import java.util.Map;

/**
 * Result of metrics calculation
 */
public class MetricsResult extends ProviderResult {
    private Map<String, Number> metrics;
    private Map<String, Object> details;
    
    public MetricsResult() {}
    
    public MetricsResult(boolean success, String message) {
        super(success, message);
    }
    
    public Map<String, Number> getMetrics() { return metrics; }
    public void setMetrics(Map<String, Number> metrics) { this.metrics = metrics; }
    
    public Map<String, Object> getDetails() { return details; }
    public void setDetails(Map<String, Object> details) { this.details = details; }
}