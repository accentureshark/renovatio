package org.shark.renovatio.provider.java;

import org.openrewrite.config.Environment;
import org.openrewrite.config.OptionDescriptor;
import org.openrewrite.config.RecipeDescriptor;
import org.openrewrite.config.YamlResourceLoader;
import org.shark.renovatio.shared.domain.AnalyzeResult;
import org.shark.renovatio.shared.domain.ApplyResult;
import org.shark.renovatio.shared.domain.StubResult;
import org.shark.renovatio.shared.domain.Tool;
import org.shark.renovatio.shared.domain.Workspace;
import org.shark.renovatio.shared.domain.Scope;
import org.shark.renovatio.shared.domain.MetricsResult;
import org.shark.renovatio.shared.domain.DiffResult;
import org.shark.renovatio.shared.nql.NqlQuery;
import org.shark.renovatio.shared.spi.BaseLanguageProvider;
import java.io.File;
import java.io.InputStream;
import java.nio.file.*;
import java.util.*;
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
        RecipeExecutionResult result = runOpenRewriteRecipe(recipeId, workspace, true, false);
        AnalyzeResult ar = new AnalyzeResult();
        ar.setSuccess(result.success);
        ar.setMessage(result.summary);
        Map<String, Object> data = new HashMap<>();
        data.put("findings", result.findings);
        ar.setData(data);
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
        try {
            Environment env = getOpenRewriteEnvironment();
            org.openrewrite.config.RecipeDescriptor descriptor = env.listRecipeDescriptors().stream()
                .filter(d -> d.getName().equals(recipeId) || d.getDisplayName().equals(recipeId))
                .findFirst().orElse(null);
            if (descriptor == null || descriptor.getRecipeList() == null || descriptor.getRecipeList().isEmpty()) {
                result.success = false;
                result.summary = "Recipe not found: " + recipeId;
                return result;
            }
            // Tomar la primera receta real del descriptor y castear a Recipe
            // Buscar la clase de la receta por nombre y crear instancia por reflection
            org.openrewrite.Recipe recipe = null;
            for (RecipeDescriptor child : descriptor.getRecipeList()) {
                try {
                    Class<?> clazz = Class.forName(child.getName());
                    Object instance = clazz.getDeclaredConstructor().newInstance();
                    if (instance instanceof org.openrewrite.Recipe) {
                        recipe = (org.openrewrite.Recipe) instance;
                        break;
                    }
                } catch (Exception ignored) {}
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
            if (javaFiles.isEmpty()) {
                result.success = false;
                result.summary = "No Java files found in workspace.";
                return result;
            }
            org.openrewrite.ExecutionContext ctx = new org.openrewrite.InMemoryExecutionContext(Throwable::printStackTrace);
            List<String> sources = new ArrayList<>();
            for (Path p : javaFiles) {
                sources.add(Files.readString(p));
            }
            org.openrewrite.java.JavaParser parser = org.openrewrite.java.JavaParser.fromJavaVersion().build();
            List<org.openrewrite.SourceFile> sourceFileList = parser.parse(ctx, sources.toArray(new String[0])).collect(Collectors.toList());
            // OpenRewrite 8.x: run() acepta List<SourceFile> directamente
            List<org.openrewrite.Result> results = recipe.run(sourceFileList, ctx);
            if (apply) {
                for (org.openrewrite.Result r : results) {
                    if (r.getAfter() != null) {
                        Path filePath = workspacePath.resolve(r.getAfter().getSourcePath());
                        Files.createDirectories(filePath.getParent());
                        Files.writeString(filePath, r.getAfter().printAll());
                    }
                }
            }
            result.success = true;
            result.summary = "Recipe executed: " + recipeId + ", files changed: " + results.size();
            result.diffs = new ArrayList<>();
            result.findings = new ArrayList<>();
            result.metrics = new HashMap<>();
            result.plan = new ArrayList<>();
            result.applied = apply;
            for (org.openrewrite.Result r : results) {
                String before = r.getBefore() != null ? r.getBefore().printAll() : "";
                String after = r.getAfter() != null ? r.getAfter().printAll() : "";
                String diff = "--- BEFORE ---\n" + before + "\n--- AFTER ---\n" + after;
                result.diffs.add(diff);
                result.findings.add(r.getAfter() != null ? r.getAfter().getSourcePath().toString() : "");
                result.plan.add(after);
            }
            if (collectMetrics) {
                result.metrics.put("filesChanged", results.size());
            }
        } catch (Exception e) {
            result.success = false;
            result.summary = "Error executing recipe: " + e.getMessage();
        }
        return result;
    }

    // Clase interna para encapsular el resultado de ejecución de receta
    private static class RecipeExecutionResult {
        boolean success = false;
        String summary;
        List<String> diffs = new ArrayList<>();
        List<String> findings = new ArrayList<>();
        Map<String, Object> metrics = new HashMap<>();
        List<String> plan = new ArrayList<>();
        boolean applied = false;
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
        ToolImpl tool = new ToolImpl(
            toolName,
            descriptor.getDescription() != null ? descriptor.getDescription() : "OpenRewrite recipe: " + recipeId,
            null,
            metadata
        );
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
}
