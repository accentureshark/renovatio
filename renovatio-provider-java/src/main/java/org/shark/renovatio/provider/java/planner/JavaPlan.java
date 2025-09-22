package org.shark.renovatio.provider.java.planner;

import java.time.Instant;
import java.util.List;

/**
 * Immutable representation of a refactoring plan computed from goals and recipes.
 */
public record JavaPlan(String id,
                       String workspacePath,
                       List<String> goals,
                       List<String> recipes,
                       List<String> scope,
                       List<JavaPlanStep> steps,
                       Instant createdAt) {
}
