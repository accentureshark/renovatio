package org.shark.renovatio.shared.util;

import org.shark.renovatio.shared.domain.AnalyzeResult;
import org.shark.renovatio.shared.domain.PerformanceMetrics;

/**
 * Utility methods for benchmarking baseline and migrated executions.
 */
public final class BenchmarkUtils {

    private BenchmarkUtils() {
    }

    /**
     * Measure execution time of the given task in milliseconds.
     */
    public static PerformanceMetrics measure(Runnable task) {
        long start = System.nanoTime();
        task.run();
        long elapsed = System.nanoTime() - start;
        return new PerformanceMetrics(elapsed / 1_000_000);
    }

    /**
     * Compare performance metrics from baseline and migrated analyze results and
     * produce a human readable summary.
     */
    public static String compare(AnalyzeResult baseline, AnalyzeResult migrated) {
        PerformanceMetrics base = baseline.getPerformance();
        PerformanceMetrics mig = migrated.getPerformance();
        if (base == null || mig == null) {
            return "Performance metrics not available";
        }
        long baseMs = base.getExecutionTimeMs();
        long migMs = mig.getExecutionTimeMs();
        long diff = migMs - baseMs;
        double pct = baseMs == 0 ? 0.0 : (diff * 100.0) / baseMs;
        return String.format(
                "Baseline: %d ms, Migrated: %d ms, Change: %d ms (%.2f%%)",
                baseMs, migMs, diff, pct);
    }
}
