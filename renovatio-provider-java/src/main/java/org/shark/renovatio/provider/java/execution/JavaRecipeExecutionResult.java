package org.shark.renovatio.provider.java.execution;

import java.util.List;
import java.util.Map;

/**
 * Aggregated outcome of executing one or more OpenRewrite recipes.
 */
public record JavaRecipeExecutionResult(boolean success,
                                        boolean applied,
                                        List<JavaChange> changes,
                                        List<Map<String, Object>> issues,
                                        Map<String, Object> metrics,
                                        List<String> analyzedFiles,
                                        long durationMs,
                                        List<String> recipes,
                                        String summary) {
}
