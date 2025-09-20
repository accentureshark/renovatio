package org.shark.renovatio.shared.domain;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

public class MetricsResult {
    private boolean success;
    private String message;
    private Map<String, Number> metrics = new HashMap<>();
    private Map<String, Object> details = new HashMap<>();
    private String runId;

    public MetricsResult() {}

    public MetricsResult(boolean success, String message) {
        this.success = success;
        this.message = message;
    }

    public boolean isSuccess() {
        return success;
    }
    public void setSuccess(boolean success) {
        this.success = success;
    }
    public String getMessage() {
        return message;
    }
    public void setMessage(String message) {
        this.message = message;
    }
    public Map<String, Number> getMetrics() {
        return metrics;
    }
    public void setMetrics(Map<String, Number> metrics) {
        this.metrics.clear();
        if (metrics != null) this.metrics.putAll(metrics);
    }
    public Map<String, Object> getDetails() {
        return details;
    }
    public void setDetails(Map<String, Object> details) {
        this.details = (details != null) ? details : new HashMap<>();
    }
    public String getRunId() {
        return runId;
    }
    public void setRunId(String runId) {
        this.runId = runId;
    }
}
