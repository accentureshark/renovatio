package org.shark.renovatio.provider.java;

import org.eclipse.jgit.api.Git;
import org.eclipse.jgit.api.errors.GitAPIException;
import org.eclipse.jgit.diff.DiffEntry;
import org.eclipse.jgit.diff.DiffFormatter;
import org.eclipse.jgit.lib.ObjectId;
import org.eclipse.jgit.lib.ObjectReader;
import org.eclipse.jgit.lib.Repository;
import org.eclipse.jgit.revwalk.RevCommit;
import org.eclipse.jgit.revwalk.RevTree;
import org.eclipse.jgit.revwalk.RevWalk;
import org.eclipse.jgit.treewalk.AbstractTreeIterator;
import org.eclipse.jgit.treewalk.CanonicalTreeParser;
import org.shark.renovatio.provider.java.adapter.OpenRewriteAnalyzeAdapter;
import org.shark.renovatio.provider.java.adapter.OpenRewriteApplyAdapter;
import org.shark.renovatio.provider.java.discovery.OpenRewriteRecipeDiscoveryService;
import org.shark.renovatio.provider.java.discovery.OpenRewriteRecipeDiscoveryService.RecipeInfo;
import org.shark.renovatio.provider.java.execution.JavaRecipeExecutionResult;
import org.shark.renovatio.provider.java.execution.JavaRecipeExecutor;
import org.shark.renovatio.provider.java.planner.JavaPlan;
import org.shark.renovatio.provider.java.planner.JavaRefactorPlanner;
import org.shark.renovatio.shared.domain.AnalyzeResult;
import org.shark.renovatio.shared.domain.ApplyResult;
import org.shark.renovatio.shared.domain.BasicTool;
import org.shark.renovatio.shared.domain.DiffResult;
import org.shark.renovatio.shared.domain.MetricsResult;
import org.shark.renovatio.shared.domain.PlanResult;
import org.shark.renovatio.shared.domain.Scope;
import org.shark.renovatio.shared.domain.StubResult;
import org.shark.renovatio.shared.domain.Tool;
import org.shark.renovatio.shared.domain.Workspace;
import org.shark.renovatio.shared.nql.NqlQuery;
import org.shark.renovatio.shared.spi.BaseLanguageProvider;
import org.springframework.util.CollectionUtils;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.EnumSet;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;

/**
 * Modern Java MCP provider exposing a curated set of OpenRewrite-based tools.
 */
public class JavaProvider extends BaseLanguageProvider {

    private static final List<String> DEFAULT_SCOPE = List.of("**/*.java");

    private final OpenRewriteRecipeDiscoveryService discoveryService;
    private final JavaRefactorPlanner planner;
    private final JavaRecipeExecutor executor;
    private final OpenRewriteAnalyzeAdapter analyzeAdapter;
    private final OpenRewriteApplyAdapter applyAdapter;

    private final Map<String, JavaRecipeExecutionResult> executions = new ConcurrentHashMap<>();
    private final Map<String, String> checkpoints = new ConcurrentHashMap<>();

    public JavaProvider(OpenRewriteRecipeDiscoveryService discoveryService,
                        JavaRefactorPlanner planner,
                        JavaRecipeExecutor executor,
                        OpenRewriteAnalyzeAdapter analyzeAdapter,
                        OpenRewriteApplyAdapter applyAdapter) {
        this.discoveryService = discoveryService;
        this.planner = planner;
        this.executor = executor;
        this.analyzeAdapter = analyzeAdapter;
        this.applyAdapter = applyAdapter;
    }

    @Override
    public String language() {
        return "java";
    }

    @Override
    public Set<Capabilities> capabilities() {
        return EnumSet.of(Capabilities.ANALYZE, Capabilities.PLAN, Capabilities.APPLY, Capabilities.METRICS, Capabilities.DIFF);
    }

    @Override
    public AnalyzeResult analyze(NqlQuery query, Workspace workspace) {
        Map<String, Object> params = optionalParameters(query);
        String profile = stringParam(params, "profile", "quality");
        List<String> goals = combineLists(listParam(params, "goals"), List.of(profile));
        List<String> include = combineLists(listParam(params, "include"), listParam(params, "includeRecipes"));
        List<String> exclude = combineLists(listParam(params, "exclude"), listParam(params, "excludeRecipes"));
        int maxFindings = intParam(params, "maxFindings", 200);

        List<String> recipes = sanitizeRecipes(planner.resolveRecipes(goals, include, exclude));
        List<String> scopePatterns = listParam(params, "scope");
        if (scopePatterns.isEmpty()) {
            scopePatterns = DEFAULT_SCOPE;
        }

        JavaRecipeExecutionResult execution = executor.preview(workspace.getPath(), recipes, scopePatterns);
        AnalyzeResult analyzeResult = analyzeAdapter.adapt(execution, workspace, profile, maxFindings);
        executions.put(analyzeResult.getRunId(), execution);
        return analyzeResult;
    }

    @Override
    public PlanResult plan(NqlQuery query, Scope scope, Workspace workspace) {
        Map<String, Object> params = optionalParameters(query);
        List<String> goals = combineLists(listParam(params, "goals"), List.of(stringParam(params, "profile", null)));
        goals = goals.stream().filter(Objects::nonNull).filter(goal -> !goal.isBlank()).toList();
        List<String> include = combineLists(listParam(params, "include"), listParam(params, "includeRecipes"));
        List<String> exclude = combineLists(listParam(params, "exclude"), listParam(params, "excludeRecipes"));
        List<String> scopePatterns = scope != null && !CollectionUtils.isEmpty(scope.getIncludePatterns())
            ? scope.getIncludePatterns()
            : DEFAULT_SCOPE;

        JavaPlan plan = planner.createPlan(workspace.getPath(), goals, include, exclude, scopePatterns);
        PlanResult result = new PlanResult(true, String.format(Locale.ROOT,
            "Generated plan %s with %d recipe(s)", plan.id(), plan.recipes().size()));
        result.setPlanId(plan.id());
        result.setRunId(plan.id());

        Map<String, Object> steps = new LinkedHashMap<>();
        steps.put("steps", planner.describePlanSteps(plan));
        steps.put("recipes", plan.recipes());
        steps.put("goals", plan.goals());
        steps.put("scope", plan.scope());
        result.setSteps(steps);

        Map<String, Object> metadata = new LinkedHashMap<>();
        metadata.put("planId", plan.id());
        metadata.put("createdAt", plan.createdAt().toString());
        metadata.put("workspacePath", plan.workspacePath());
        result.setMetadata(metadata);
        return result;
    }

    @Override
    public ApplyResult apply(String planId, boolean dryRun, Workspace workspace) {
        Map<String, Object> args = new LinkedHashMap<>();
        args.put("workspacePath", workspace != null ? workspace.getPath() : null);
        args.put("planId", planId);
        args.put("dryRun", dryRun);
        Map<String, Object> response = handleApply(args);
        if (!Boolean.TRUE.equals(response.get("success"))) {
            return new ApplyResult(false, Objects.toString(response.get("error"), "Apply failed"));
        }
        JavaRecipeExecutionResult execution = executions.get(response.get("runId"));
        ApplyResult applyResult = applyAdapter.adapt(execution, dryRun, checkpoints.get(response.get("runId")));
        applyResult.setRunId((String) response.get("runId"));
        applyResult.setMessage((String) response.getOrDefault("message", execution.summary()));
        return applyResult;
    }

    @Override
    public DiffResult diff(String runId, Workspace workspace) {
        JavaRecipeExecutionResult execution = executions.get(runId);
        DiffResult result;
        if (execution == null) {
            result = new DiffResult(false, "No execution found for runId " + runId);
        } else {
            result = new DiffResult(true, execution.summary());
            result.setUnifiedDiff(execution.changes().stream()
                .map(change -> change.diff() + System.lineSeparator())
                .collect(Collectors.joining()));
        }
        result.setRunId(runId);
        return result;
    }

    @Override
    public MetricsResult metrics(Scope scope, Workspace workspace) {
        List<String> scopePatterns = scope != null && !CollectionUtils.isEmpty(scope.getIncludePatterns())
            ? scope.getIncludePatterns()
            : DEFAULT_SCOPE;
        JavaRecipeExecutionResult execution = executor.preview(workspace.getPath(), List.of(), scopePatterns);
        MetricsResult metricsResult = new MetricsResult(execution.success(), execution.summary());
        Map<String, Number> metrics = new LinkedHashMap<>();
        execution.metrics().forEach((key, value) -> {
            if (value instanceof Number number) {
                metrics.put(key, number);
            }
        });
        metricsResult.setMetrics(metrics);
        Map<String, Object> details = new LinkedHashMap<>();
        details.put("recipes", execution.recipes());
        details.put("analyzedFiles", execution.analyzedFiles());
        metricsResult.setDetails(details);
        return metricsResult;
    }

    @Override
    public Optional<StubResult> generateStubs(NqlQuery query, Workspace workspace) {
        return Optional.empty();
    }

    @Override
    public List<Tool> getTools() {
        List<Tool> tools = new ArrayList<>();
        tools.add(createTool("java.discover", "Inspect workspace structure", workspaceSchema(), discoverOutputSchema()));
        tools.add(createTool("java.analyze", "Analyze Java sources with OpenRewrite", analyzeSchema(), analyzeOutputSchema()));
        tools.add(createTool("java.plan", "Plan refactoring based on goals", planSchema(), planOutputSchema()));
        tools.add(createTool("java.apply", "Apply OpenRewrite recipes", applySchema(), applyOutputSchema()));
        tools.add(createTool("java.diff", "Generate git diff between revisions", diffSchema(), diffOutputSchema()));
        tools.add(createTool("java.review", "Summarize refactoring outcome", reviewSchema(), reviewOutputSchema()));
        tools.add(createTool("java.format", "Format sources and remove unused imports", formatSchema(), applyOutputSchema()));
        tools.add(createTool("java.test", "Run project tests", testSchema(), testOutputSchema()));
        tools.add(createTool("java.metrics", "Collect high level metrics", metricsSchema(), metricsOutputSchema()));
        tools.add(createTool("java.recipe_list", "List available OpenRewrite recipes", Map.of("type", "object"), recipeListOutputSchema()));
        tools.add(createTool("java.recipe_describe", "Describe a specific recipe", recipeDescribeSchema(), recipeDescribeOutputSchema()));
        tools.add(createTool("java.pipeline", "Execute preset modernization pipeline", pipelineSchema(), pipelineOutputSchema()));
        return tools;
    }

    @Override
    public Map<String, Object> executeExtendedTool(String capability, Map<String, Object> arguments) {
        if (capability == null) {
            return null;
        }
        return switch (capability.toLowerCase(Locale.ROOT)) {
            case "discover" -> handleDiscover(arguments);
            case "apply" -> handleApply(arguments);
            case "diff" -> handleDiff(arguments);
            case "review" -> handleReview(arguments);
            case "format" -> handleFormat(arguments);
            case "test" -> handleTest(arguments);
            case "recipe_list" -> handleRecipeList(arguments);
            case "recipe_describe" -> handleRecipeDescribe(arguments);
            case "pipeline" -> handlePipeline(arguments);
            default -> null;
        };
    }

    private Map<String, Object> handleDiscover(Map<String, Object> arguments) {
        Path workspace = workspace(arguments);
        Map<String, Object> result = baseResponse("discover");
        if (workspace == null) {
            return error(result, "workspacePath is required");
        }
        try {
            result.put("modules", discoverModules(workspace));
            result.put("dependencies", discoverDependencies(workspace));
            result.put("files", listFiles(workspace, 200));
            result.put("message", String.format(Locale.ROOT, "Discovered %d module(s)", ((List<?>) result.get("modules")).size()));
            return success(result);
        } catch (IOException ex) {
            return error(result, ex.getMessage());
        }
    }

    private Map<String, Object> handleApply(Map<String, Object> arguments) {
        Path workspace = workspace(arguments);
        Map<String, Object> result = baseResponse("apply");
        if (workspace == null) {
            return error(result, "workspacePath is required");
        }

        String planId = stringParam(arguments, "planId", null);
        List<String> recipes = listParam(arguments, "recipes");
        boolean dryRun = booleanParam(arguments, "dryRun", true);
        List<String> scope = listParam(arguments, "scope");

        if (planId != null) {
            Optional<JavaPlan> plan = planner.findPlan(planId);
            if (plan.isPresent()) {
                if (recipes.isEmpty()) {
                    recipes = plan.get().recipes();
                }
                if (scope.isEmpty()) {
                    scope = plan.get().scope();
                }
            }
        }
        if (recipes.isEmpty()) {
            return error(result, "No recipes specified. Provide planId or recipes[]");
        }
        if (scope.isEmpty()) {
            scope = DEFAULT_SCOPE;
        }

        List<String> sanitizedRecipes = sanitizeRecipes(recipes);
        if (sanitizedRecipes.isEmpty()) {
            return error(result, "All requested recipes require configuration. Provide safe recipes or a curated profile");
        }

        JavaRecipeExecutionResult execution = executor.apply(workspace.toString(), sanitizedRecipes, scope, dryRun);
        String runId = planId != null ? planId + "-run" : generateRunId();
        executions.put(runId, execution);
        result.put("runId", runId);
        result.put("dryRun", dryRun);
        result.put("changes", execution.changes());
        result.put("issues", execution.issues());
        result.put("metrics", execution.metrics());
        result.put("recipes", execution.recipes());
        result.put("message", execution.summary());

        if (!execution.success()) {
            return error(result, execution.summary());
        }

        if (!dryRun) {
            String checkpoint = createCheckpoint(workspace, execution.summary());
            if (checkpoint != null) {
                checkpoints.put(runId, checkpoint);
                result.put("checkpointRef", checkpoint);
            }
        }
        return success(result);
    }

    private Map<String, Object> handleDiff(Map<String, Object> arguments) {
        Path workspace = workspace(arguments);
        Map<String, Object> result = baseResponse("diff");
        if (workspace == null) {
            return error(result, "workspacePath is required");
        }
        String fromRef = stringParam(arguments, "fromRef", null);
        String toRef = stringParam(arguments, "toRef", "HEAD");
        try {
            List<Map<String, Object>> changes = gitDiff(workspace, fromRef, toRef);
            result.put("changes", changes);
            result.put("message", String.format(Locale.ROOT, "Generated diff with %d change(s)", changes.size()));
            return success(result);
        } catch (Exception ex) {
            return error(result, ex.getMessage());
        }
    }

    private Map<String, Object> handleReview(Map<String, Object> arguments) {
        Map<String, Object> diff = handleDiff(arguments);
        Map<String, Object> result = baseResponse("review");
        if (Boolean.FALSE.equals(diff.get("success"))) {
            return diff;
        }
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> changes = (List<Map<String, Object>>) diff.getOrDefault("changes", List.of());
        List<Map<String, Object>> highlights = new ArrayList<>();
        for (Map<String, Object> change : changes) {
            Map<String, Object> highlight = new LinkedHashMap<>();
            highlight.put("file", change.get("file"));
            highlight.put("summary", "Updated file " + change.get("file"));
            highlights.add(highlight);
        }
        result.put("summaryMarkdown", String.format(Locale.ROOT, "### Review\n- %d change(s) detected", changes.size()));
        result.put("highlights", highlights);
        result.put("diff", diff.get("changes"));
        return success(result);
    }

    private Map<String, Object> handleFormat(Map<String, Object> arguments) {
        arguments = new LinkedHashMap<>(arguments);
        arguments.put("recipes", List.of(
            "org.openrewrite.java.format.AutoFormat",
            "org.openrewrite.java.cleanup.RemoveUnusedImports"
        ));
        arguments.put("dryRun", false);
        return handleApply(arguments);
    }

    private Map<String, Object> handleTest(Map<String, Object> arguments) {
        Path workspace = workspace(arguments);
        Map<String, Object> result = baseResponse("test");
        if (workspace == null) {
            return error(result, "workspacePath is required");
        }
        List<String> command = determineTestCommand(workspace);
        if (command.isEmpty()) {
            return error(result, "No supported build tool (mvn/gradle) found");
        }
        try {
            ProcessBuilder builder = new ProcessBuilder(command);
            builder.directory(workspace.toFile());
            Process process = builder.start();
            int exit = process.waitFor();
            result.put("passed", exit == 0);
            result.put("failed", exit == 0 ? 0 : 1);
            if (exit != 0) {
                try (BufferedReader reader = new BufferedReader(new InputStreamReader(process.getErrorStream()))) {
                    String failureMessage = reader.lines().limit(50).collect(Collectors.joining(System.lineSeparator()));
                    Map<String, Object> failure = new LinkedHashMap<>();
                    failure.put("message", failureMessage);
                    result.put("failures", List.of(failure));
                }
            } else {
                result.put("failures", List.of());
            }
            result.put("message", exit == 0 ? "Tests passed" : "Tests failed (exit code " + exit + ")");
            return success(result);
        } catch (IOException | InterruptedException ex) {
            Thread.currentThread().interrupt();
            return error(result, ex.getMessage());
        }
    }

    private Map<String, Object> handleRecipeList(Map<String, Object> arguments) {
        Map<String, Object> result = baseResponse("recipe_list");
        List<Map<String, Object>> recipes = discoveryService.listAllRecipes().stream()
            .map(this::toRecipeMap)
            .collect(Collectors.toList());
        result.put("recipes", recipes);
        result.put("message", String.format(Locale.ROOT, "Found %d recipes", recipes.size()));
        return success(result);
    }

    private Map<String, Object> handleRecipeDescribe(Map<String, Object> arguments) {
        Map<String, Object> result = baseResponse("recipe_describe");
        String name = stringParam(arguments, "name", null);
        if (name == null || name.isBlank()) {
            return error(result, "name parameter is required");
        }
        Optional<RecipeInfo> info = discoveryService.describeRecipe(name);
        if (info.isEmpty()) {
            return error(result, "Recipe not found: " + name);
        }
        result.put("recipe", toRecipeMap(info.get()));
        return success(result);
    }

    private Map<String, Object> handlePipeline(Map<String, Object> arguments) {
        Path workspace = workspace(arguments);
        Map<String, Object> result = baseResponse("pipeline");
        if (workspace == null) {
            return error(result, "workspacePath is required");
        }
        String preset = stringParam(arguments, "preset", "modernize_java17");
        boolean dryRun = booleanParam(arguments, "dryRun", true);

        List<String> goals = List.of(preset);
        JavaPlan plan = planner.createPlan(workspace.toString(), goals, List.of(), List.of(), DEFAULT_SCOPE);
        JavaRecipeExecutionResult analyze = executor.preview(workspace.toString(), plan.recipes(), DEFAULT_SCOPE);
        JavaRecipeExecutionResult apply = executor.apply(workspace.toString(), plan.recipes(), DEFAULT_SCOPE, dryRun);

        result.put("planId", plan.id());
        result.put("issues", analyze.issues());
        result.put("changes", apply.changes());
        result.put("message", String.format(Locale.ROOT,
            "Pipeline '%s' executed with %d recipe(s)", preset, plan.recipes().size()));
        return success(result);
    }

    private Map<String, Object> baseResponse(String type) {
        Map<String, Object> response = new LinkedHashMap<>();
        response.put("type", type);
        response.put("success", false);
        return response;
    }

    private Map<String, Object> success(Map<String, Object> response) {
        response.put("success", true);
        return response;
    }

    private Map<String, Object> error(Map<String, Object> response, String message) {
        response.put("success", false);
        response.put("error", message);
        response.put("message", message);
        return response;
    }

    private Path workspace(Map<String, Object> arguments) {
        String path = stringParam(arguments, "workspacePath", null);
        return path != null ? Paths.get(path) : null;
    }

    private Map<String, Object> toRecipeMap(RecipeInfo info) {
        Map<String, Object> map = new LinkedHashMap<>();
        map.put("name", info.name());
        map.put("displayName", info.displayName());
        map.put("description", info.description());
        map.put("tags", info.tags());
        map.put("options", info.options());
        return map;
    }

    private String createCheckpoint(Path workspace, String summary) {
        try (Git git = Git.open(workspace.toFile())) {
            if (git.status().call().isClean()) {
                return null;
            }
            git.add().addFilepattern(".").call();
            RevCommit commit = git.commit().setMessage("Renovatio: " + summary).call();
            return commit.getName();
        } catch (IOException | GitAPIException ex) {
            return null;
        }
    }

    private List<Map<String, Object>> gitDiff(Path workspace, String fromRef, String toRef) throws Exception {
        try (Git git = Git.open(workspace.toFile())) {
            Repository repository = git.getRepository();
            ObjectId to = repository.resolve(toRef);
            ObjectId from = fromRef != null ? repository.resolve(fromRef) : repository.resolve(toRef + "^");
            if (to == null || from == null) {
                throw new IllegalArgumentException("Unable to resolve git references");
            }
            try (ObjectReader reader = repository.newObjectReader();
                 DiffFormatter formatter = new DiffFormatter(new java.io.ByteArrayOutputStream());
                 RevWalk walk = new RevWalk(repository)) {
                formatter.setRepository(repository);
                RevCommit fromCommit = walk.parseCommit(from);
                RevCommit toCommit = walk.parseCommit(to);
                AbstractTreeIterator oldTree = prepareTreeParser(reader, fromCommit.getTree());
                AbstractTreeIterator newTree = prepareTreeParser(reader, toCommit.getTree());
                List<Map<String, Object>> changes = new ArrayList<>();
                for (DiffEntry entry : formatter.scan(oldTree, newTree)) {
                    java.io.ByteArrayOutputStream out = new java.io.ByteArrayOutputStream();
                    try (DiffFormatter entryFormatter = new DiffFormatter(out)) {
                        entryFormatter.setRepository(repository);
                        entryFormatter.format(entry);
                        Map<String, Object> change = new LinkedHashMap<>();
                        change.put("file", entry.getNewPath());
                        change.put("diff", out.toString(StandardCharsets.UTF_8));
                        changes.add(change);
                    }
                }
                return changes;
            }
        }
    }

    private AbstractTreeIterator prepareTreeParser(ObjectReader reader, RevTree tree) throws IOException {
        CanonicalTreeParser parser = new CanonicalTreeParser();
        parser.reset(reader, tree.getId());
        return parser;
    }

    private List<String> discoverModules(Path workspace) throws IOException {
        List<String> modules = new ArrayList<>();
        try (var stream = Files.list(workspace)) {
            stream.filter(Files::isDirectory).forEach(path -> {
                if (Files.exists(path.resolve("pom.xml")) || Files.exists(path.resolve("build.gradle"))) {
                    modules.add(path.getFileName().toString());
                }
            });
        }
        if (modules.isEmpty()) {
            modules.add(workspace.getFileName().toString());
        }
        return modules;
    }

    private List<Map<String, String>> discoverDependencies(Path workspace) throws IOException {
        Path pom = workspace.resolve("pom.xml");
        if (!Files.exists(pom)) {
            return List.of();
        }
        List<Map<String, String>> dependencies = new ArrayList<>();
        List<String> lines = Files.readAllLines(pom);
        String group = null;
        String artifact = null;
        String version = null;
        for (String line : lines) {
            String trimmed = line.trim();
            if (trimmed.startsWith("<groupId>")) {
                group = trimmed.replace("<groupId>", "").replace("</groupId>", "").trim();
            } else if (trimmed.startsWith("<artifactId>")) {
                artifact = trimmed.replace("<artifactId>", "").replace("</artifactId>", "").trim();
            } else if (trimmed.startsWith("<version>")) {
                version = trimmed.replace("<version>", "").replace("</version>", "").trim();
            } else if (trimmed.startsWith("</dependency>")) {
                if (artifact != null) {
                    Map<String, String> dependency = new LinkedHashMap<>();
                    dependency.put("groupId", group);
                    dependency.put("artifactId", artifact);
                    dependency.put("version", version);
                    dependencies.add(dependency);
                }
                group = artifact = version = null;
            }
        }
        return dependencies;
    }

    private List<String> listFiles(Path workspace, int limit) throws IOException {
        List<String> files = new ArrayList<>();
        try (var stream = Files.walk(workspace)) {
            stream.filter(Files::isRegularFile)
                .limit(limit)
                .forEach(path -> files.add(workspace.relativize(path).toString()));
        }
        return files;
    }

    private List<String> determineTestCommand(Path workspace) {
        if (Files.exists(workspace.resolve("mvnw"))) {
            return Arrays.asList("./mvnw", "-q", "test");
        }
        if (Files.exists(workspace.resolve("mvnw.cmd"))) {
            return Arrays.asList("mvnw.cmd", "-q", "test");
        }
        if (Files.exists(workspace.resolve("pom.xml"))) {
            return Arrays.asList("mvn", "-q", "test");
        }
        if (Files.exists(workspace.resolve("gradlew"))) {
            return Arrays.asList("./gradlew", "test");
        }
        if (Files.exists(workspace.resolve("gradlew.bat"))) {
            return Arrays.asList("gradlew.bat", "test");
        }
        if (Files.exists(workspace.resolve("build.gradle"))) {
            return Arrays.asList("gradle", "test");
        }
        return List.of();
    }

    private Map<String, Object> workspaceSchema() {
        return schema(builder -> {
            builder.put("workspacePath", property("string", "Workspace root", true));
        });
    }

    private Map<String, Object> analyzeSchema() {
        return schema(builder -> {
            builder.put("workspacePath", property("string", "Workspace root", true));
            builder.put("profile", property("string", "Analysis profile", false,
                Map.of("enum", List.of("quality", "style", "modernize_java17", "security", "testing_support", "all"))));
            builder.put("include", arrayProperty("Include specific recipes"));
            builder.put("exclude", arrayProperty("Exclude recipes"));
            builder.put("maxFindings", property("integer", "Maximum issues returned", false));
        });
    }

    private Map<String, Object> planSchema() {
        return schema(builder -> {
            builder.put("workspacePath", property("string", "Workspace root", true));
            builder.put("goals", arrayProperty("High level goals"));
            builder.put("includeRecipes", arrayProperty("Force include recipes"));
            builder.put("excludeRecipes", arrayProperty("Remove recipes"));
            builder.put("scope", arrayProperty("Glob patterns"));
        });
    }

    private Map<String, Object> applySchema() {
        return schema(builder -> {
            builder.put("workspacePath", property("string", "Workspace root", true));
            builder.put("planId", property("string", "Existing plan identifier", false));
            builder.put("recipes", arrayProperty("Recipes to execute"));
            builder.put("dryRun", property("boolean", "Preview changes only", false));
            builder.put("scope", arrayProperty("Glob patterns"));
        });
    }

    private Map<String, Object> diffSchema() {
        return schema(builder -> {
            builder.put("workspacePath", property("string", "Workspace root", true));
            builder.put("fromRef", property("string", "Git reference to diff from", false));
            builder.put("toRef", property("string", "Git reference to diff to", false));
        });
    }

    private Map<String, Object> reviewSchema() {
        return diffSchema();
    }

    private Map<String, Object> formatSchema() {
        return schema(builder -> builder.put("workspacePath", property("string", "Workspace root", true)));
    }

    private Map<String, Object> testSchema() {
        return schema(builder -> builder.put("workspacePath", property("string", "Workspace root", true)));
    }

    private Map<String, Object> metricsSchema() {
        return schema(builder -> builder.put("workspacePath", property("string", "Workspace root", true)));
    }

    private Map<String, Object> recipeDescribeSchema() {
        return schema(builder -> builder.put("name", property("string", "Recipe name", true)));
    }

    private Map<String, Object> pipelineSchema() {
        return schema(builder -> {
            builder.put("workspacePath", property("string", "Workspace root", true));
            builder.put("preset", property("string", "Preset pipeline", false,
                Map.of("enum", List.of("modernize_java17", "cleanup_style", "remove_deprecations", "format_only"))));
            builder.put("dryRun", property("boolean", "Preview changes", false));
        });
    }

    private Map<String, Object> discoverOutputSchema() {
        Map<String, Object> dependency = new LinkedHashMap<>();
        dependency.put("type", "object");
        dependency.put("properties", Map.of(
            "groupId", Map.of("type", "string"),
            "artifactId", Map.of("type", "string"),
            "version", Map.of("type", "string")
        ));

        Map<String, Object> schema = new LinkedHashMap<>();
        schema.put("type", "object");
        schema.put("properties", Map.of(
            "modules", Map.of("type", "array", "items", Map.of("type", "string")),
            "dependencies", Map.of("type", "array", "items", dependency),
            "files", Map.of("type", "array", "items", Map.of("type", "string")),
            "message", Map.of("type", "string")
        ));
        return schema;
    }

    private Map<String, Object> analyzeOutputSchema() {
        Map<String, Object> issue = new LinkedHashMap<>();
        issue.put("type", "object");
        issue.put("properties", Map.of(
            "file", Map.of("type", "string"),
            "line", Map.of("type", "integer"),
            "severity", Map.of("type", "string"),
            "type", Map.of("type", "string"),
            "message", Map.of("type", "string"),
            "recipe", Map.of("type", "string")
        ));
        Map<String, Object> schema = new LinkedHashMap<>();
        schema.put("type", "object");
        schema.put("properties", Map.of(
            "issues", Map.of("type", "array", "items", issue),
            "metrics", Map.of("type", "object")
        ));
        return schema;
    }

    private Map<String, Object> planOutputSchema() {
        Map<String, Object> schema = new LinkedHashMap<>();
        schema.put("type", "object");
        schema.put("properties", Map.of(
            "planId", Map.of("type", "string"),
            "recipes", Map.of("type", "array", "items", Map.of("type", "string")),
            "steps", Map.of("type", "array", "items", Map.of("type", "object"))
        ));
        return schema;
    }

    private Map<String, Object> applyOutputSchema() {
        Map<String, Object> schema = new LinkedHashMap<>();
        schema.put("type", "object");
        schema.put("properties", Map.of(
            "changes", Map.of("type", "array", "items", Map.of("type", "object")),
            "issues", Map.of("type", "array", "items", Map.of("type", "object")),
            "metrics", Map.of("type", "object")
        ));
        return schema;
    }

    private Map<String, Object> diffOutputSchema() {
        Map<String, Object> schema = new LinkedHashMap<>();
        schema.put("type", "object");
        schema.put("properties", Map.of(
            "changes", Map.of("type", "array", "items", Map.of("type", "object"))
        ));
        return schema;
    }

    private Map<String, Object> reviewOutputSchema() {
        Map<String, Object> schema = new LinkedHashMap<>();
        schema.put("type", "object");
        schema.put("properties", Map.of(
            "summaryMarkdown", Map.of("type", "string"),
            "highlights", Map.of("type", "array", "items", Map.of("type", "object"))
        ));
        return schema;
    }

    private Map<String, Object> testOutputSchema() {
        Map<String, Object> schema = new LinkedHashMap<>();
        schema.put("type", "object");
        schema.put("properties", Map.of(
            "passed", Map.of("type", "boolean"),
            "failed", Map.of("type", "integer"),
            "failures", Map.of("type", "array", "items", Map.of("type", "object"))
        ));
        return schema;
    }

    private Map<String, Object> metricsOutputSchema() {
        Map<String, Object> schema = new LinkedHashMap<>();
        schema.put("type", "object");
        schema.put("properties", Map.of(
            "metrics", Map.of("type", "object"),
            "details", Map.of("type", "object")
        ));
        return schema;
    }

    private Map<String, Object> recipeListOutputSchema() {
        Map<String, Object> schema = new LinkedHashMap<>();
        schema.put("type", "object");
        schema.put("properties", Map.of(
            "recipes", Map.of("type", "array", "items", Map.of("type", "object"))
        ));
        return schema;
    }

    private Map<String, Object> recipeDescribeOutputSchema() {
        Map<String, Object> schema = new LinkedHashMap<>();
        schema.put("type", "object");
        schema.put("properties", Map.of("recipe", Map.of("type", "object")));
        return schema;
    }

    private Map<String, Object> pipelineOutputSchema() {
        Map<String, Object> schema = new LinkedHashMap<>();
        schema.put("type", "object");
        schema.put("properties", Map.of(
            "planId", Map.of("type", "string"),
            "issues", Map.of("type", "array", "items", Map.of("type", "object")),
            "changes", Map.of("type", "array", "items", Map.of("type", "object"))
        ));
        return schema;
    }

    private BasicTool createTool(String name, String description, Map<String, Object> inputSchema, Map<String, Object> outputSchema) {
        BasicTool tool = new BasicTool(name, description, inputSchema);
        tool.getMetadata().put("outputSchema", outputSchema);
        return tool;
    }

    private Map<String, Object> schema(java.util.function.Consumer<Map<String, Object>> consumer) {
        Map<String, Object> properties = new LinkedHashMap<>();
        consumer.accept(properties);
        Map<String, Object> schema = new LinkedHashMap<>();
        schema.put("type", "object");
        schema.put("properties", properties);
        List<String> required = properties.entrySet().stream()
            .filter(entry -> Boolean.TRUE.equals(((Map<?, ?>) entry.getValue()).get("required")))
            .map(Map.Entry::getKey)
            .collect(Collectors.toList());
        if (!required.isEmpty()) {
            schema.put("required", required);
        }
        return schema;
    }

    private Map<String, Object> property(String type, String description, boolean required) {
        return property(type, description, required, Map.of());
    }

    private Map<String, Object> property(String type, String description, boolean required, Map<String, Object> extra) {
        Map<String, Object> prop = new LinkedHashMap<>();
        prop.put("type", type);
        if (description != null) {
            prop.put("description", description);
        }
        if (!extra.isEmpty()) {
            prop.putAll(extra);
        }
        if (required) {
            prop.put("required", true);
        }
        return prop;
    }

    private Map<String, Object> arrayProperty(String description) {
        return property("array", description, false,
            Map.of("items", Map.of("type", "string")));
    }

    private Map<String, Object> optionalParameters(NqlQuery query) {
        return query != null && query.getParameters() != null
            ? query.getParameters()
            : Map.of();
    }

    @SuppressWarnings("unchecked")
    private List<String> listParam(Map<String, ?> params, String key) {
        Object value = params.get(key);
        if (value == null) {
            return new ArrayList<>();
        }
        if (value instanceof List<?>) {
            return ((List<?>) value).stream().map(Objects::toString).collect(Collectors.toList());
        }
        if (value instanceof String str) {
            if (str.isBlank()) {
                return new ArrayList<>();
            }
            return Arrays.stream(str.split(",")).map(String::trim).filter(s -> !s.isBlank()).collect(Collectors.toList());
        }
        return new ArrayList<>();
    }

    private String stringParam(Map<String, ?> params, String key, String defaultValue) {
        Object value = params.get(key);
        return value != null ? value.toString() : defaultValue;
    }

    private boolean booleanParam(Map<String, ?> params, String key, boolean defaultValue) {
        Object value = params.get(key);
        if (value == null) {
            return defaultValue;
        }
        if (value instanceof Boolean bool) {
            return bool;
        }
        return Boolean.parseBoolean(value.toString());
    }

    private int intParam(Map<String, ?> params, String key, int defaultValue) {
        Object value = params.get(key);
        if (value == null) {
            return defaultValue;
        }
        if (value instanceof Number number) {
            return number.intValue();
        }
        try {
            return Integer.parseInt(value.toString());
        } catch (NumberFormatException ex) {
            return defaultValue;
        }
    }

    private List<String> sanitizeRecipes(List<String> recipes) {
        if (recipes == null || recipes.isEmpty()) {
            return List.of();
        }

        LinkedHashSet<String> safe = new LinkedHashSet<>();
        for (String recipe : recipes) {
            if (recipe == null || recipe.isBlank()) {
                continue;
            }
            String trimmed = recipe.trim();
            if (discoveryService.isRecipeSafe(trimmed)) {
                safe.add(trimmed);
            } else {
                // Skipped unsafe recipe
            }
        }
        return List.copyOf(safe);
    }

    private List<String> combineLists(List<String> first, List<String> second) {
        LinkedHashSet<String> set = new LinkedHashSet<>();
        if (first != null) {
            set.addAll(first.stream().filter(Objects::nonNull).filter(s -> !s.isBlank()).toList());
        }
        if (second != null) {
            set.addAll(second.stream().filter(Objects::nonNull).filter(s -> !s.isBlank()).toList());
        }
        return new ArrayList<>(set);
    }
}
