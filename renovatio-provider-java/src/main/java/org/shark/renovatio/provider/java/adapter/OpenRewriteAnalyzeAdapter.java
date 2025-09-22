package org.shark.renovatio.provider.java.adapter;

import org.shark.renovatio.provider.java.execution.JavaChange;
import org.shark.renovatio.provider.java.execution.JavaRecipeExecutionResult;
import org.shark.renovatio.shared.domain.AnalyzeResult;
import org.shark.renovatio.shared.domain.PerformanceMetrics;
import org.shark.renovatio.shared.domain.Workspace;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * Adapts execution results into {@link AnalyzeResult} objects understood by the MCP layer.
 */
public class OpenRewriteAnalyzeAdapter {

    public AnalyzeResult adapt(JavaRecipeExecutionResult execution,
                               Workspace workspace,
                               String profile,
                               int maxFindings) {
        AnalyzeResult result = new AnalyzeResult(execution.success(), execution.summary());
        result.setRunId(profile != null ? profile + "-" + System.currentTimeMillis() : "java-analyze-" + System.currentTimeMillis());
        result.setMessage(execution.summary());
        result.setPerformance(new PerformanceMetrics(execution.durationMs()));

        Map<String, Object> data = new LinkedHashMap<>();
        data.put("summary", execution.summary());
        data.put("workspace", workspace != null ? workspace.getPath() : null);
        data.put("profile", profile);
        data.put("metrics", execution.metrics());
        data.put("analyzedFiles", execution.analyzedFiles());
        data.put("recipes", execution.recipes());
        data.put("issues", limitIssues(execution.issues(), maxFindings));
        data.put("diffs", toDiffs(execution.changes(), maxFindings));

        result.setData(data);
        return result;
    }

    private List<Map<String, Object>> limitIssues(List<Map<String, Object>> issues, int maxFindings) {
        if (issues == null) {
            return List.of();
        }
        if (maxFindings <= 0 || issues.size() <= maxFindings) {
            return new ArrayList<>(issues);
        }
        return new ArrayList<>(issues.subList(0, maxFindings));
    }

    private List<Map<String, Object>> toDiffs(List<JavaChange> changes, int maxFindings) {
        List<Map<String, Object>> diffs = new ArrayList<>();
        if (changes == null) {
            return diffs;
        }
        int limit = maxFindings > 0 ? Math.min(maxFindings, changes.size()) : changes.size();
        for (int index = 0; index < limit; index++) {
            JavaChange change = changes.get(index);
            Map<String, Object> diff = new LinkedHashMap<>();
            diff.put("file", change.file());
            diff.put("diff", change.diff());
            diffs.add(diff);
        }
        return diffs;
    }
}
