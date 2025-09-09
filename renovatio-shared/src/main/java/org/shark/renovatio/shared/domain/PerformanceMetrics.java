package org.shark.renovatio.shared.domain;

/**
 * Simple container for execution performance metrics.
 */
public class PerformanceMetrics {
    private long executionTimeMs;

    public PerformanceMetrics() {
    }

    public PerformanceMetrics(long executionTimeMs) {
        this.executionTimeMs = executionTimeMs;
    }

    public long getExecutionTimeMs() {
        return executionTimeMs;
    }

    public void setExecutionTimeMs(long executionTimeMs) {
        this.executionTimeMs = executionTimeMs;
    }
}
