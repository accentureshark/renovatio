package org.shark.renovatio.shared.domain;

import java.util.Map;

/**
 * Base class for provider operation results
 */
public abstract class ProviderResult {
    private boolean success;
    private String message;
    private String runId;
    private Map<String, Object> metadata;
    private long timestamp;

    public ProviderResult() {
        this.timestamp = System.currentTimeMillis();
    }

    public ProviderResult(boolean success, String message) {
        this();
        this.success = success;
        this.message = message;
    }

    // Getters and setters
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

    public String getRunId() {
        return runId;
    }

    public void setRunId(String runId) {
        this.runId = runId;
    }

    public Map<String, Object> getMetadata() {
        return metadata;
    }

    public void setMetadata(Map<String, Object> metadata) {
        this.metadata = metadata;
    }

    public long getTimestamp() {
        return timestamp;
    }

    public void setTimestamp(long timestamp) {
        this.timestamp = timestamp;
    }
}