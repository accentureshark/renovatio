package org.shark.renovatio.provider.java.adapter;

import org.shark.renovatio.provider.java.execution.JavaChange;
import org.shark.renovatio.provider.java.execution.JavaRecipeExecutionResult;
import org.shark.renovatio.shared.domain.ApplyResult;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * Converts execution results into {@link ApplyResult} instances.
 */
public class OpenRewriteApplyAdapter {

    public ApplyResult adapt(JavaRecipeExecutionResult execution,
                             boolean dryRun,
                             String checkpointRef) {
        ApplyResult result = new ApplyResult(execution.success(), execution.summary());
        result.setDryRun(dryRun);
        result.setDiff(buildUnifiedDiff(execution.changes()));
        result.setModifiedFiles(execution.changes().stream().map(JavaChange::file).collect(Collectors.toList()));

        Map<String, Object> changes = new LinkedHashMap<>();
        changes.put("changes", execution.changes());
        changes.put("issues", execution.issues());
        changes.put("metrics", execution.metrics());
        if (checkpointRef != null) {
            changes.put("checkpointRef", checkpointRef);
        }
        result.setChanges(changes);
        return result;
    }

    private String buildUnifiedDiff(List<JavaChange> changes) {
        if (changes == null || changes.isEmpty()) {
            return "";
        }
        StringBuilder builder = new StringBuilder();
        for (JavaChange change : changes) {
            builder.append(change.diff()).append('\n');
        }
        return builder.toString();
    }
}
