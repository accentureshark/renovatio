package org.shark.renovatio.provider.java.execution;

import org.openrewrite.ExecutionContext;
import org.openrewrite.InMemoryExecutionContext;
import org.openrewrite.Result;
import org.openrewrite.SourceFile;
import org.openrewrite.java.JavaParser;
import org.shark.renovatio.provider.java.OpenRewriteRunner;
import org.shark.renovatio.provider.java.discovery.OpenRewriteRecipeDiscoveryService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.PathMatcher;
import java.nio.file.Paths;
import java.time.Duration;
import java.time.Instant;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * Executes OpenRewrite recipes and adapts the results to simple DTOs used by MCP tools.
 */
@Service
public class JavaRecipeExecutor {

    private static final Logger LOGGER = LoggerFactory.getLogger(JavaRecipeExecutor.class);

    private final OpenRewriteRecipeDiscoveryService discoveryService;
    private final OpenRewriteRunner runner;

    public JavaRecipeExecutor(OpenRewriteRecipeDiscoveryService discoveryService, OpenRewriteRunner runner) {
        this.discoveryService = discoveryService;
        this.runner = runner;
    }

    public JavaRecipeExecutionResult preview(String workspacePath,
                                             List<String> recipes,
                                             List<String> scopePatterns) {
        return execute(workspacePath, recipes, scopePatterns, true);
    }

    public JavaRecipeExecutionResult apply(String workspacePath,
                                           List<String> recipes,
                                           List<String> scopePatterns,
                                           boolean dryRun) {
        return execute(workspacePath, recipes, scopePatterns, dryRun);
    }

    private JavaRecipeExecutionResult execute(String workspacePath,
                                              List<String> recipes,
                                              List<String> scopePatterns,
                                              boolean dryRun) {
        Instant start = Instant.now();
        try {
            Path workspace = workspacePath != null ? Paths.get(workspacePath) : null;
            if (workspace == null || !Files.exists(workspace)) {
                return new JavaRecipeExecutionResult(false, false, List.of(), List.of(), Map.of(), List.of(),
                    Duration.between(start, Instant.now()).toMillis(),
                    recipes != null ? List.copyOf(recipes) : List.of(),
                    "Workspace path is required");
            }

            List<Path> javaFiles = collectJavaFiles(workspace, scopePatterns);
            if (javaFiles.isEmpty()) {
                return new JavaRecipeExecutionResult(false, false, List.of(), List.of(), Map.of(), List.of(),
                    Duration.between(start, Instant.now()).toMillis(),
                    recipes != null ? List.copyOf(recipes) : List.of(),
                    "No Java files found in workspace");
            }

            List<String> recipeList = recipes != null ? recipes.stream()
                .filter(Objects::nonNull)
                .filter(name -> !name.isBlank())
                .distinct()
                .collect(Collectors.toList()) : List.of();

            ExecutionContext ctx = new InMemoryExecutionContext(Throwable::printStackTrace);
            List<SourceFile> sourceFiles = parseSources(javaFiles, ctx);

            if (recipeList.isEmpty()) {
                Map<String, Object> metrics = buildMetrics(javaFiles, List.of(), false, start, recipeList);
                return new JavaRecipeExecutionResult(true, false, List.of(), List.of(), metrics,
                    relativize(workspace, javaFiles),
                    Duration.between(start, Instant.now()).toMillis(),
                    recipeList,
                    "No recipes selected");
            }

            List<Result> results = runner.runRecipe(
                discoveryService.buildCompositeRecipe(recipeList),
                ctx,
                sourceFiles
            );

            List<JavaChange> changes = new ArrayList<>();
            List<Map<String, Object>> issues = new ArrayList<>();
            Set<String> touchedFiles = new LinkedHashSet<>();

            for (Result result : results) {
                String before = result.getBefore() != null ? result.getBefore().printAll() : "";
                String after = result.getAfter() != null ? result.getAfter().printAll() : "";
                String sourcePath = resolveSourcePath(result, workspace);
                touchedFiles.add(sourcePath);
                changes.add(new JavaChange(sourcePath, buildDiff(sourcePath, before, after)));
                issues.add(buildIssue(sourcePath, recipeList, dryRun));

                if (!dryRun && result.getAfter() != null) {
                    Path file = workspace.resolve(result.getAfter().getSourcePath());
                    Files.createDirectories(file.getParent());
                    Files.writeString(file, after, StandardCharsets.UTF_8);
                }
            }

            Map<String, Object> metrics = buildMetrics(javaFiles, changes, !dryRun, start, recipeList);
            long duration = Duration.between(start, Instant.now()).toMillis();
            String summary = String.format(Locale.ROOT,
                "%s %d recipe(s), %d file(s) analysed, %d change(s)",
                dryRun ? "Previewed" : "Applied",
                recipeList.size(),
                javaFiles.size(),
                changes.size());

            return new JavaRecipeExecutionResult(true, !dryRun, List.copyOf(changes), List.copyOf(issues), metrics,
                new ArrayList<>(touchedFiles.isEmpty() ? relativize(workspace, javaFiles) : new ArrayList<>(touchedFiles)),
                duration,
                recipeList,
                summary);
        } catch (Exception ex) {
            LOGGER.warn("OpenRewrite execution failed: {}", ex.getMessage(), ex);
            return new JavaRecipeExecutionResult(false, false, List.of(), List.of(), Map.of(), List.of(),
                Duration.between(start, Instant.now()).toMillis(),
                recipes != null ? List.copyOf(recipes) : List.of(),
                ex.getMessage());
        }
    }

    private List<Path> collectJavaFiles(Path workspace, List<String> scopePatterns) throws IOException {
        List<PathMatcher> matchers = new ArrayList<>();
        if (scopePatterns != null && !scopePatterns.isEmpty()) {
            for (String pattern : scopePatterns) {
                if (pattern == null || pattern.isBlank()) {
                    continue;
                }
                String normalized = pattern.startsWith("glob:") ? pattern : "glob:" + pattern;
                matchers.add(FileSystems.getDefault().getPathMatcher(normalized));
            }
        }

        List<Path> javaFiles = new ArrayList<>();
        try (var stream = Files.walk(workspace)) {
            stream.filter(path -> path.toString().endsWith(".java"))
                .filter(path -> matchers.isEmpty() || matches(workspace, path, matchers))
                .forEach(javaFiles::add);
        }
        return javaFiles;
    }

    private boolean matches(Path workspace, Path path, List<PathMatcher> matchers) {
        Path relative = workspace.relativize(path);
        for (PathMatcher matcher : matchers) {
            if (matcher.matches(relative)) {
                return true;
            }
        }
        return false;
    }

    private List<SourceFile> parseSources(List<Path> javaFiles, ExecutionContext ctx) throws IOException {
        JavaParser parser = JavaParser.fromJavaVersion().build();
        return parser.parse(ctx, javaFiles.toArray(Path[]::new));
    }

    private Map<String, Object> buildMetrics(List<Path> javaFiles,
                                             List<JavaChange> changes,
                                             boolean applied,
                                             Instant start,
                                             List<String> recipes) {
        Map<String, Object> metrics = new LinkedHashMap<>();
        metrics.put("totalFiles", javaFiles.size());
        metrics.put("filesChanged", changes.size());
        metrics.put("issuesFound", changes.size());
        metrics.put("recipes", recipes);
        metrics.put("applied", applied);
        metrics.put("durationMs", Duration.between(start, Instant.now()).toMillis());
        return metrics;
    }

    private List<String> relativize(Path workspace, List<Path> files) {
        return files.stream()
            .map(workspace::relativize)
            .map(Path::toString)
            .collect(Collectors.toList());
    }

    private String resolveSourcePath(Result result, Path workspace) {
        if (result.getAfter() != null) {
            return result.getAfter().getSourcePath().toString();
        }
        if (result.getBefore() != null) {
            return result.getBefore().getSourcePath().toString();
        }
        return workspace.relativize(workspace).toString();
    }

    private Map<String, Object> buildIssue(String file,
                                           List<String> recipes,
                                           boolean dryRun) {
        Map<String, Object> issue = new LinkedHashMap<>();
        issue.put("file", file);
        issue.put("line", 0);
        issue.put("severity", dryRun ? "WARNING" : "INFO");
        issue.put("type", dryRun ? "PREVIEW" : "CHANGE");
        issue.put("message", dryRun
            ? "Recipe(s) would update this file"
            : "Recipe(s) applied to this file");
        issue.put("recipe", String.join(", ", recipes));
        return issue;
    }

    private String buildDiff(String file, String before, String after) {
        String header = String.format("--- %s%n+++ %s%n", file, file);
        StringBuilder builder = new StringBuilder(header);
        builder.append("@@ BEFORE @@\n").append(before).append('\n');
        builder.append("@@ AFTER @@\n").append(after).append('\n');
        return builder.toString();
    }
}
