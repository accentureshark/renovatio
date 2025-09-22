package org.shark.renovatio.provider.java;

import org.openrewrite.LargeSourceSet;
import org.openrewrite.config.Environment;
import org.openrewrite.config.OptionDescriptor;
import org.openrewrite.config.RecipeDescriptor;
import org.openrewrite.config.YamlResourceLoader;
import org.shark.renovatio.shared.domain.AnalyzeResult;
import org.shark.renovatio.shared.domain.ApplyResult;
import org.shark.renovatio.shared.domain.BasicTool;
import org.shark.renovatio.shared.domain.StubResult;
import org.shark.renovatio.shared.domain.Tool;
import org.shark.renovatio.shared.domain.Workspace;
import org.shark.renovatio.shared.domain.Scope;
import org.shark.renovatio.shared.domain.MetricsResult;
import org.shark.renovatio.shared.domain.DiffResult;
import org.shark.renovatio.shared.domain.PerformanceMetrics;
import org.shark.renovatio.shared.nql.NqlQuery;
import org.shark.renovatio.shared.spi.BaseLanguageProvider;
import java.io.File;
import java.io.InputStream;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.nio.file.*;
import java.util.*;
import java.util.concurrent.TimeUnit;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import org.shark.renovatio.shared.domain.PlanResult;

public class JavaLanguageProvider extends BaseLanguageProvider {
    @Override
    public String language() {
        return "java";
    }

    @Override
    public Set<Capabilities> capabilities() {
        return EnumSet.of(Capabilities.ANALYZE, Capabilities.PLAN, Capabilities.APPLY, Capabilities.DIFF, Capabilities.METRICS);
    }

    @Override
    public List<Tool> getTools() {
        // Solo publica tools MCP para recetas OpenRewrite descubiertas dinámicamente
        return discoverRecipeTools();
    }


    private Environment getOpenRewriteEnvironment() {
        try {
            Properties properties = new Properties();
            Environment.Builder builder = Environment.builder(properties)
                .scanRuntimeClasspath();
            File rewriteConfig = new File("rewrite.yml");
            if (rewriteConfig.exists()) {
                try (InputStream inputStream = Files.newInputStream(rewriteConfig.toPath())) {
                    builder.load(new YamlResourceLoader(inputStream, rewriteConfig.toURI(), properties, Thread.currentThread().getContextClassLoader()));
                }
            }
            return builder.build();
        } catch (Exception e) {
            throw new RuntimeException("Failed to initialize OpenRewrite Environment", e);
        }
    }

    // Métodos MCP fijos: solo para cumplir la interfaz, no deben usarse directamente
    @Override
    public AnalyzeResult analyze(NqlQuery query, Workspace workspace) {
        String recipeId = extractRecipeIdFromNql(query);
        if (recipeId == null) {
            throw new IllegalArgumentException("No recipe specified in NQL query for analyze().");
        }
        return executeAnalyzeRecipe(recipeId, workspace, query);
    }

    @Override
    public MetricsResult metrics(Scope scope, Workspace workspace) {
        // Busca una receta de métricas específica, si existe
        String recipeId = "metrics"; // Ajustar si hay una receta concreta para métricas
        return executeMetricsRecipe(recipeId, workspace, scope);
    }

    @Override
    public Optional<StubResult> generateStubs(NqlQuery query, Workspace workspace) {
        // OpenRewrite no soporta generación de stubs para Java
        return Optional.empty();
    }

    @Override
    public DiffResult diff(String runId, Workspace workspace) {
        // runId se asume como recipeId para simplificar
        return executeDiffRecipe(runId, workspace);
    }

    @Override
    public ApplyResult apply(String runId, boolean dryRun, Workspace workspace) {
        // runId se asume como recipeId para simplificar
        return executeApplyRecipe(runId, workspace, dryRun);
    }

    @Override
    public PlanResult plan(NqlQuery query, Scope scope, Workspace workspace) {
        String recipeId = extractRecipeIdFromNql(query);
        if (recipeId == null) {
            throw new IllegalArgumentException("No recipe specified in NQL query for plan().");
        }
        return executePlanRecipe(recipeId, workspace, scope, query);
    }

    // --- Métodos privados para ejecutar recetas OpenRewrite y construir resultados MCP ---

    private AnalyzeResult executeAnalyzeRecipe(String recipeId, Workspace workspace, NqlQuery query) {
        RecipeExecutionResult result = runOpenRewriteRecipe(recipeId, workspace, false, false);
        AnalyzeResult ar = new AnalyzeResult();
        ar.setSuccess(result.success);
        ar.setMessage(result.summary);
        ar.setAst(Collections.emptyMap());
        ar.setDependencies(Collections.emptyMap());
        ar.setSymbols(Collections.emptyMap());

        Map<String, Object> data = new LinkedHashMap<>();
        data.put("recipeId", recipeId);
        data.put("summary", result.summary);
        data.put("issues", result.issues);
        data.put("diffs", result.diffs);
        data.put("analyzedFiles", result.analyzedFiles);
        data.put("applied", result.applied);

        Map<String, Object> metrics = new LinkedHashMap<>();
        metrics.put("totalFiles", result.totalFiles);
        metrics.put("issuesFound", result.issues.size());
        metrics.put("filesChanged", result.findings.size());
        metrics.put("durationMs", result.durationMs);
        data.put("metrics", metrics);

        ar.setData(data);
        ar.setPerformance(new PerformanceMetrics(result.durationMs));
        return ar;
    }

    private MetricsResult executeMetricsRecipe(String recipeId, Workspace workspace, Scope scope) {
        RecipeExecutionResult result = runOpenRewriteRecipe(recipeId, workspace, true, true);
        MetricsResult mr = new MetricsResult();
        mr.setSuccess(result.success);
        mr.setMessage(result.summary);
        // Adaptar a Map<String, Number>
        Map<String, Number> metrics = new HashMap<>();
        for (Map.Entry<String, Object> entry : result.metrics.entrySet()) {
            if (entry.getValue() instanceof Number) {
                metrics.put(entry.getKey(), (Number) entry.getValue());
            }
        }
        mr.setMetrics(metrics);
        return mr;
    }

    private DiffResult executeDiffRecipe(String recipeId, Workspace workspace) {
        RecipeExecutionResult result = runOpenRewriteRecipe(recipeId, workspace, true, false);
        DiffResult dr = new DiffResult();
        dr.setSuccess(result.success);
        dr.setMessage(result.summary);
        dr.setUnifiedDiff(String.join("\n\n", result.diffs));
        return dr;
    }

    private ApplyResult executeApplyRecipe(String recipeId, Workspace workspace, boolean dryRun) {
        RecipeExecutionResult result = runOpenRewriteRecipe(recipeId, workspace, !dryRun, false);
        ApplyResult ap = new ApplyResult();
        ap.setSuccess(result.success);
        ap.setMessage(result.summary);
        ap.setDiff(String.join("\n\n", result.diffs));
        ap.setModifiedFiles(result.findings);
        ap.setDryRun(dryRun);
        return ap;
    }

    private PlanResult executePlanRecipe(String recipeId, Workspace workspace, Scope scope, NqlQuery query) {
        RecipeExecutionResult result = runOpenRewriteRecipe(recipeId, workspace, false, true);
        PlanResult pr = new PlanResult();
        pr.setSuccess(result.success);
        pr.setMessage(result.summary);
        Map<String, Object> steps = new HashMap<>();
        steps.put("steps", result.plan);
        pr.setSteps(steps);
        return pr;
    }

    /**
     * Ejecuta una receta OpenRewrite sobre los archivos del workspace.
     * @param recipeId ID de la receta OpenRewrite
     * @param workspace Workspace Renovatio
     * @param apply Si true, aplica los cambios; si false, solo simula
     * @param collectMetrics Si true, recolecta métricas
     * @return Resultado de la ejecución de la receta
     */
    private RecipeExecutionResult runOpenRewriteRecipe(String recipeId, Workspace workspace, boolean apply, boolean collectMetrics) {
        RecipeExecutionResult result = new RecipeExecutionResult();
        result.applied = apply;
        long startNanos = System.nanoTime();
        try {
            if (workspace == null || workspace.getPath() == null || workspace.getPath().isBlank()) {
                result.success = false;
                result.summary = "Workspace path is required.";
                result.metrics.put("totalFiles", 0);
                result.metrics.put("issuesFound", 0);
                return result;
            }

            Environment env = getOpenRewriteEnvironment();
            org.openrewrite.config.RecipeDescriptor descriptor = env.listRecipeDescriptors().stream()
                .filter(d -> d.getName().equals(recipeId) || d.getDisplayName().equals(recipeId))
                .findFirst().orElse(null);
            if (descriptor == null || descriptor.getRecipeList() == null || descriptor.getRecipeList().isEmpty()) {
                result.success = false;
                result.summary = "Recipe not found: " + recipeId;
                return result;
            }

            org.openrewrite.Recipe recipe = null;
            for (RecipeDescriptor child : descriptor.getRecipeList()) {
                try {
                    Class<?> clazz = Class.forName(child.getName());
                    Object instance = clazz.getDeclaredConstructor().newInstance();
                    if (instance instanceof org.openrewrite.Recipe) {
                        recipe = (org.openrewrite.Recipe) instance;
                        break;
                    }
                } catch (Exception ignored) {
                    // Continue searching for a concrete recipe implementation
                }
            }
            if (recipe == null) {
                result.success = false;
                result.summary = "Could not instantiate recipe: " + recipeId;
                return result;
            }

            Path workspacePath = Paths.get(workspace.getPath());
            List<Path> javaFiles;
            try (var stream = Files.walk(workspacePath)) {
                javaFiles = stream.filter(p -> p.toString().endsWith(".java")).collect(Collectors.toList());
            }
            result.totalFiles = javaFiles.size();
            for (Path file : javaFiles) {
                result.analyzedFiles.add(relativizePath(workspacePath, file));
            }
            if (javaFiles.isEmpty()) {
                result.success = false;
                result.summary = "No Java files found in workspace.";
                return result;
            }

            org.openrewrite.ExecutionContext ctx = new org.openrewrite.InMemoryExecutionContext(Throwable::printStackTrace);

            org.openrewrite.java.JavaParser parser = org.openrewrite.java.JavaParser.fromJavaVersion().build();
            List<String> sources = new ArrayList<>();
            for (Path p : javaFiles) {
                sources.add(Files.readString(p));
            }
            List<org.openrewrite.SourceFile> sourceFileList = parser.parse(ctx, sources.toArray(new String[0])).collect(Collectors.toList());
            List<org.openrewrite.Result> results = executeRecipeWithCompatibility(recipe, ctx, sourceFileList);

            if (apply) {
                for (org.openrewrite.Result r : results) {
                    if (r.getAfter() != null) {
                        Path filePath = workspacePath.resolve(r.getAfter().getSourcePath());
                        Files.createDirectories(filePath.getParent());
                        Files.writeString(filePath, r.getAfter().printAll());
                    }
                }
            }

            for (org.openrewrite.Result r : results) {
                String before = r.getBefore() != null ? r.getBefore().printAll() : "";
                String after = r.getAfter() != null ? r.getAfter().printAll() : "";
                String diff = "--- BEFORE ---\n" + before + "\n--- AFTER ---\n" + after;
                String sourcePath = "";
                if (r.getAfter() != null) {
                    sourcePath = r.getAfter().getSourcePath().toString();
                } else if (r.getBefore() != null) {
                    sourcePath = r.getBefore().getSourcePath().toString();
                }

                result.diffs.add(diff);
                result.findings.add(sourcePath);
                result.plan.add(after);

                Map<String, Object> issue = new LinkedHashMap<>();
                issue.put("file", sourcePath);
                issue.put("message", "Recipe '" + recipeId + "' would modify this file.");
                issue.put("severity", "INFO");
                issue.put("type", "MODIFICATION");
                issue.put("diff", diff);
                issue.put("applied", apply);
                result.issues.add(issue);
            }

            result.metrics.put("totalFiles", result.totalFiles);
            result.metrics.put("filesChanged", results.size());
            result.metrics.put("affectedFiles", results.size());
            result.metrics.put("issuesFound", result.issues.size());
            if (collectMetrics) {
                result.metrics.put("metricsCollected", Boolean.TRUE);
            }

            result.success = true;
        } catch (Exception e) {
            result.success = false;
            result.summary = "Error executing recipe: " + e.getMessage();
        } finally {
            result.durationMs = TimeUnit.NANOSECONDS.toMillis(System.nanoTime() - startNanos);
            result.metrics.put("durationMs", result.durationMs);
        }

        if (result.success) {
            StringBuilder summary = new StringBuilder();
            summary.append("Recipe '").append(recipeId).append("' ");
            summary.append(apply ? "applied" : "evaluated").append(' ');
            if (result.issues.isEmpty()) {
                summary.append("without modifying any files");
            } else {
                summary.append("changes to ").append(result.issues.size())
                    .append(result.issues.size() == 1 ? " file" : " files");
            }
            if (result.durationMs > 0) {
                summary.append(" in ").append(result.durationMs).append(" ms");
            }
            summary.append('.');
            result.summary = summary.toString();
        }

        return result;
    }

    private List<org.openrewrite.Result> executeRecipeWithCompatibility(org.openrewrite.Recipe recipe,
                                                                        org.openrewrite.ExecutionContext ctx,
                                                                        List<org.openrewrite.SourceFile> sourceFiles) {
        try {
            return runRecipeWithLargeSourceSet(recipe, ctx, sourceFiles);
        } catch (ClassNotFoundException | NoSuchMethodException ex) {
            try {
                return runRecipeWithLegacyIterable(recipe, ctx, sourceFiles);
            } catch (InvocationTargetException legacyInvocation) {
                throw propagateInvocationException(legacyInvocation);
            } catch (IllegalAccessException | NoSuchMethodException reflectionFailure) {
                throw new RuntimeException(reflectionFailure);
            }
        } catch (InvocationTargetException ex) {
            throw propagateInvocationException(ex);
        } catch (IllegalAccessException ex) {
            throw new RuntimeException(ex);
        }
    }

    @SuppressWarnings("unchecked")
    private List<org.openrewrite.Result> runRecipeWithLargeSourceSet(org.openrewrite.Recipe recipe,
                                                                     org.openrewrite.ExecutionContext ctx,
                                                                     List<org.openrewrite.SourceFile> sourceFiles)
        throws ClassNotFoundException, NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        Class<?> largeSourceSetClass = Class.forName("org.openrewrite.LargeSourceSet");
        Object largeSourceSet = createLargeSourceSet(largeSourceSetClass, sourceFiles);
        Method runMethod;
        boolean contextFirst = false;
        try {
            runMethod = org.openrewrite.Recipe.class.getMethod("run", largeSourceSetClass, org.openrewrite.ExecutionContext.class);
        } catch (NoSuchMethodException ex) {
            runMethod = org.openrewrite.Recipe.class.getMethod("run", org.openrewrite.ExecutionContext.class, largeSourceSetClass);
            contextFirst = true;
        }
        Object recipeRun = contextFirst
            ? runMethod.invoke(recipe, ctx, largeSourceSet)
            : runMethod.invoke(recipe, largeSourceSet, ctx);
        return extractResultsFromRecipeRun(recipeRun);
    }

    @SuppressWarnings("unchecked")
    private List<org.openrewrite.Result> runRecipeWithLegacyIterable(org.openrewrite.Recipe recipe,
                                                                     org.openrewrite.ExecutionContext ctx,
                                                                     List<org.openrewrite.SourceFile> sourceFiles)
        throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        Method runMethod = findLegacyRunMethod(sourceFiles);
        Class<?>[] parameterTypes = runMethod.getParameterTypes();
        boolean contextFirst = org.openrewrite.ExecutionContext.class.isAssignableFrom(parameterTypes[0]);
        Class<?> sourceParamType = contextFirst ? parameterTypes[1] : parameterTypes[0];
        Object argument = adaptLegacyArgument(sourceParamType, sourceFiles);
        Object recipeRun = contextFirst
            ? runMethod.invoke(recipe, ctx, argument)
            : runMethod.invoke(recipe, argument, ctx);
        if (recipeRun instanceof List<?>) {
            return (List<org.openrewrite.Result>) recipeRun;
        }
        return extractResultsFromRecipeRun(recipeRun);
    }

    private Method findLegacyRunMethod(List<org.openrewrite.SourceFile> sourceFiles) throws NoSuchMethodException {
        for (Method method : org.openrewrite.Recipe.class.getMethods()) {
            if (!"run".equals(method.getName()) || method.getParameterCount() != 2) {
                continue;
            }
            Class<?>[] parameterTypes = method.getParameterTypes();
            boolean firstIsContext = org.openrewrite.ExecutionContext.class.isAssignableFrom(parameterTypes[0]);
            boolean secondIsContext = org.openrewrite.ExecutionContext.class.isAssignableFrom(parameterTypes[1]);
            if (firstIsContext == secondIsContext) {
                continue;
            }
            Class<?> sourceParam = firstIsContext ? parameterTypes[1] : parameterTypes[0];
            if (isLegacyIterableCompatible(sourceParam, sourceFiles)) {
                return method;
            }
        }
        throw new NoSuchMethodException("No compatible Recipe.run signature for SourceFile inputs");
    }

    private boolean isLegacyIterableCompatible(Class<?> parameterType, List<org.openrewrite.SourceFile> sourceFiles) {
        return parameterType.isAssignableFrom(sourceFiles.getClass())
            || parameterType.isAssignableFrom(List.class)
            || parameterType.isAssignableFrom(Collection.class)
            || parameterType.isAssignableFrom(Iterable.class)
            || (parameterType.isArray()
                && parameterType.getComponentType() != null
                && parameterType.getComponentType().isAssignableFrom(org.openrewrite.SourceFile.class));
    }

    private Object adaptLegacyArgument(Class<?> parameterType, List<org.openrewrite.SourceFile> sourceFiles) {
        if (parameterType.isAssignableFrom(sourceFiles.getClass())
            || parameterType.isAssignableFrom(List.class)
            || parameterType.isAssignableFrom(Collection.class)
            || parameterType.isAssignableFrom(Iterable.class)) {
            return sourceFiles;
        }
        if (parameterType.isArray()
            && parameterType.getComponentType() != null
            && parameterType.getComponentType().isAssignableFrom(org.openrewrite.SourceFile.class)) {
            return sourceFiles.toArray(new org.openrewrite.SourceFile[0]);
        }
        throw new IllegalArgumentException("Unsupported Recipe.run parameter type: " + parameterType.getName());
    }

    private String relativizePath(Path workspacePath, Path filePath) {
        if (filePath == null) {
            return "";
        }
        try {
            if (workspacePath != null) {
                Path normalizedWorkspace = workspacePath.toAbsolutePath().normalize();
                Path normalizedFile = filePath.toAbsolutePath().normalize();
                if (normalizedFile.startsWith(normalizedWorkspace)) {
                    return normalizedWorkspace.relativize(normalizedFile).toString();
                }
            }
        } catch (Exception ignored) {
            // Fall back to absolute representation below
        }
        return filePath.toString();
    }

    private RuntimeException propagateInvocationException(InvocationTargetException ex) {
        Throwable cause = ex.getCause();
        if (cause instanceof RuntimeException runtimeException) {
            return runtimeException;
        }
        return new RuntimeException(cause != null ? cause : ex);
    }

    private Object createLargeSourceSet(Class<?> largeSourceSetClass,
                                        List<org.openrewrite.SourceFile> sourceFiles)
        throws InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        for (Method method : largeSourceSetClass.getMethods()) {
            if (Modifier.isStatic(method.getModifiers())
                && largeSourceSetClass.isAssignableFrom(method.getReturnType())
                && method.getParameterCount() == 1
                && method.getParameterTypes()[0].isAssignableFrom(sourceFiles.getClass())) {
                return method.invoke(null, sourceFiles);
            }
        }

        Method builderMethod = null;
        for (Method candidate : largeSourceSetClass.getMethods()) {
            if (Modifier.isStatic(candidate.getModifiers())
                && candidate.getParameterCount() == 0
                && candidate.getReturnType() != null
                && candidate.getReturnType().getSimpleName().toLowerCase(Locale.ROOT).contains("builder")) {
                builderMethod = candidate;
                break;
            }
        }
        if (builderMethod == null) {
            throw new NoSuchMethodException("No factory method found to create LargeSourceSet");
        }

        Object builder = builderMethod.invoke(null);
        boolean added = tryInvokeBuilderAddAll(builder, sourceFiles);
        if (!added) {
            Method addMethod = builder.getClass().getMethod("add", org.openrewrite.SourceFile.class);
            for (org.openrewrite.SourceFile sourceFile : sourceFiles) {
                addMethod.invoke(builder, sourceFile);
            }
        }
        Method buildMethod = builder.getClass().getMethod("build");
        return buildMethod.invoke(builder);
    }

    private boolean tryInvokeBuilderAddAll(Object builder, List<org.openrewrite.SourceFile> sourceFiles)
        throws InvocationTargetException, IllegalAccessException {
        for (Method method : builder.getClass().getMethods()) {
            if ("addAll".equals(method.getName()) && method.getParameterCount() == 1) {
                Class<?> paramType = method.getParameterTypes()[0];
                if (paramType.isAssignableFrom(sourceFiles.getClass())
                    || paramType.isAssignableFrom(List.class)
                    || paramType.isAssignableFrom(Collection.class)
                    || paramType.isAssignableFrom(Iterable.class)) {
                    method.invoke(builder, sourceFiles);
                    return true;
                }
            }
        }
        return false;
    }

    @SuppressWarnings("unchecked")
    private List<org.openrewrite.Result> extractResultsFromRecipeRun(Object recipeRun)
        throws InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        if (recipeRun == null) {
            return Collections.emptyList();
        }

        Class<?> runClass = recipeRun.getClass();
        try {
            Method getResults = runClass.getMethod("getResults");
            Object value = getResults.invoke(recipeRun);
            if (value instanceof List<?>) {
                return (List<org.openrewrite.Result>) value;
            }
        } catch (NoSuchMethodException ignored) {
            // Continue to alternate access pattern
        }

        try {
            Method getChangeset = runClass.getMethod("getChangeset");
            Object changeSet = getChangeset.invoke(recipeRun);
            if (changeSet != null) {
                Method getResults = changeSet.getClass().getMethod("getResults");
                Object value = getResults.invoke(changeSet);
                if (value instanceof List<?>) {
                    return (List<org.openrewrite.Result>) value;
                }
            }
        } catch (NoSuchMethodException ignored) {
            // No alternate representation available
        }

        throw new NoSuchMethodException("Unable to extract results from RecipeRun type: " + runClass.getName());
    }

    // Clase interna para encapsular el resultado de ejecución de receta
    private static class RecipeExecutionResult {
        boolean success = false;
        String summary;
        List<String> diffs = new ArrayList<>();
        List<String> findings = new ArrayList<>();
        List<Map<String, Object>> issues = new ArrayList<>();
        List<String> analyzedFiles = new ArrayList<>();
        Map<String, Object> metrics = new HashMap<>();
        List<String> plan = new ArrayList<>();
        boolean applied = false;
        int totalFiles = 0;
        long durationMs = 0L;
    }

    private String extractRecipeIdFromNql(NqlQuery query) {
        // Buscar "recipe: <id>" en el NQL original
        if (query == null || query.getOriginalQuery() == null) return null;
        String nql = query.getOriginalQuery();
        // El guion va al final del grupo y no se escapa
        Matcher matcher = Pattern.compile("recipe\\s*:\\s*([\\w.-]+)").matcher(nql);
        if (matcher.find()) {
            return matcher.group(1);
        }
        return null;
    }

    private List<Tool> discoverRecipeTools() {
        List<Tool> recipeTools = new ArrayList<>();
        try {
            Properties properties = new Properties();
            Environment.Builder builder = Environment.builder(properties)
                .scanRuntimeClasspath();
            File rewriteConfig = new File("rewrite.yml");
            if (rewriteConfig.exists()) {
                try (InputStream inputStream = Files.newInputStream(rewriteConfig.toPath())) {
                    builder.load(new YamlResourceLoader(inputStream, rewriteConfig.toURI(), properties, Thread.currentThread().getContextClassLoader()));
                }
            }
            Environment environment = builder.build();
            Collection<RecipeDescriptor> descriptors = environment.listRecipeDescriptors();
            if (descriptors == null || descriptors.isEmpty()) {
                System.out.println("[JavaLanguageProvider] No OpenRewrite recipes discovered on the classpath.");
                return recipeTools;
            }
            Set<String> seenRecipes = new LinkedHashSet<>();
            Set<String> seenToolNames = new LinkedHashSet<>();
            for (RecipeDescriptor descriptor : descriptors) {
                collectRecipeTools(descriptor, recipeTools, seenRecipes, seenToolNames);
            }
            System.out.println("[JavaLanguageProvider] Exposing " + recipeTools.size() + " OpenRewrite recipe tool(s).");
        } catch (Exception e) {
            System.err.println("[WARN] Unable to discover OpenRewrite recipes: " + e.getMessage());
            e.printStackTrace(System.err);
        }
        return recipeTools;
    }

    private void collectRecipeTools(RecipeDescriptor descriptor, List<Tool> tools, Set<String> seenRecipes, Set<String> seenToolNames) {
        if (descriptor == null) return;
        String recipeId = descriptor.getName();
        String displayName = descriptor.getDisplayName();
        String toolName = "java." + recipeId;
        if (!seenRecipes.add(recipeId) || !seenToolNames.add(toolName)) {
            return;
        }
        Map<String, Object> metadata = new LinkedHashMap<>();
        metadata.put("recipeId", recipeId);
        metadata.put("displayName", displayName);
        metadata.put("description", descriptor.getDescription());
        metadata.put("tags", descriptor.getTags());
        metadata.put("options", extractRecipeOptions(descriptor));

        Map<String, Object> inputSchema = buildRecipeInputSchema(descriptor);

        BasicTool tool = new BasicTool();
        tool.setName(toolName);
        tool.setDescription(descriptor.getDescription() != null
            ? descriptor.getDescription()
            : "OpenRewrite recipe: " + recipeId);
        tool.setInputSchema(inputSchema);
        tool.setMetadata(metadata);
        tools.add(tool);
        if (descriptor.getRecipeList() != null) {
            for (RecipeDescriptor child : descriptor.getRecipeList()) {
                collectRecipeTools(child, tools, seenRecipes, seenToolNames);
            }
        }
    }

    private List<Map<String, Object>> extractRecipeOptions(RecipeDescriptor descriptor) {
        List<Map<String, Object>> options = new ArrayList<>();
        if (descriptor.getOptions() != null) {
            for (OptionDescriptor opt : descriptor.getOptions()) {
                Map<String, Object> optMap = new LinkedHashMap<>();
                optMap.put("name", opt.getName());
                optMap.put("type", opt.getType());
                optMap.put("required", opt.isRequired());
                optMap.put("description", opt.getDescription());
                optMap.put("example", opt.getExample());
                options.add(optMap);
            }
        }
        return options;
    }

    private Map<String, Object> buildRecipeInputSchema(RecipeDescriptor descriptor) {
        Map<String, Object> schema = new LinkedHashMap<>();
        schema.put("type", "object");

        Map<String, Object> properties = new LinkedHashMap<>();
        Map<String, Object> workspacePath = new LinkedHashMap<>();
        workspacePath.put("type", "string");
        workspacePath.put("description", "Absolute path to the workspace directory containing sources to refactor");
        properties.put("workspacePath", workspacePath);

        List<String> required = new ArrayList<>();
        required.add("workspacePath");

        if (descriptor.getOptions() != null) {
            for (OptionDescriptor option : descriptor.getOptions()) {
                if (option.getName() == null || option.getName().isBlank()) {
                    continue;
                }
                Map<String, Object> optionSchema = new LinkedHashMap<>();
                optionSchema.put("type", mapOptionType(option.getType()));
                if (option.getDescription() != null && !option.getDescription().isBlank()) {
                    optionSchema.put("description", option.getDescription());
                }
                List<Object> examples = buildExamples(option.getExample());
                if (!examples.isEmpty()) {
                    optionSchema.put("examples", examples);
                }
                if (optionSchema.get("type").equals("array")) {
                    optionSchema.putIfAbsent("items", Map.of("type", "string"));
                }
                properties.put(option.getName(), optionSchema);
                if (option.isRequired()) {
                    required.add(option.getName());
                }
            }
        }

        schema.put("properties", properties);
        if (!required.isEmpty()) {
            schema.put("required", required);
        }
        schema.put("additionalProperties", false);
        return schema;
    }

    private List<Object> buildExamples(Object example) {
        if (example == null) {
            return Collections.emptyList();
        }
        if (example instanceof Collection<?> collection) {
            return new ArrayList<>(collection);
        }
        if (example.getClass().isArray()) {
            int length = java.lang.reflect.Array.getLength(example);
            List<Object> examples = new ArrayList<>(length);
            for (int i = 0; i < length; i++) {
                examples.add(java.lang.reflect.Array.get(example, i));
            }
            return examples;
        }
        return List.of(example);
    }

    private String mapOptionType(String optionType) {
        if (optionType == null || optionType.isBlank()) {
            return "string";
        }
        String normalized = optionType.toLowerCase(Locale.ROOT);
        if (normalized.contains("boolean")) {
            return "boolean";
        }
        if (normalized.contains("int") || normalized.contains("long") || normalized.contains("short")
            || normalized.contains("byte")) {
            return "integer";
        }
        if (normalized.contains("double") || normalized.contains("float") || normalized.contains("bigdecimal")
            || normalized.contains("number")) {
            return "number";
        }
        if (normalized.contains("list") || normalized.contains("set") || normalized.contains("collection")
            || normalized.contains("array")) {
            return "array";
        }
        if (normalized.contains("map")) {
            return "object";
        }
        return "string";
    }
}
