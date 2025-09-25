package org.shark.renovatio.provider.cobol;

import org.shark.renovatio.provider.cobol.service.*;
import org.shark.renovatio.shared.domain.*;
import org.shark.renovatio.shared.nql.NqlQuery;
import org.shark.renovatio.shared.spi.BaseLanguageProvider;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.*;

/**
 * COBOL Language Provider implementation supporting COBOL to Java migration
 * Implements parsing, analysis, code generation and migration capabilities
 */
public class CobolLanguageProvider extends BaseLanguageProvider {

    private final CobolParsingService parsingService;
    private final JavaGenerationService javaGenerationService;
    private final MigrationPlanService migrationPlanService;
    private final IndexingService indexingService;
    private final MetricsService metricsService;
    private final TemplateCodeGenerationService templateCodeGenerationService;
    private final Db2MigrationService db2MigrationService;

    public CobolLanguageProvider(
            CobolParsingService parsingService,
            JavaGenerationService javaGenerationService,
            MigrationPlanService migrationPlanService,
            IndexingService indexingService,
            MetricsService metricsService,
            TemplateCodeGenerationService templateCodeGenerationService,
            Db2MigrationService db2MigrationService) {
        this.parsingService = parsingService;
        this.javaGenerationService = javaGenerationService;
        this.migrationPlanService = migrationPlanService;
        this.indexingService = indexingService;
        this.metricsService = metricsService;
        this.templateCodeGenerationService = templateCodeGenerationService;
        this.db2MigrationService = db2MigrationService;
    }

    @Override
    public String language() {
        return "cobol";
    }

    @Override
    public Set<Capabilities> capabilities() {
        return EnumSet.of(
                Capabilities.ANALYZE,
                Capabilities.PLAN,
                Capabilities.APPLY,
                Capabilities.DIFF,
                Capabilities.STUBS,
                Capabilities.METRICS
        );
    }

    @Override
    public AnalyzeResult analyze(NqlQuery query, Workspace workspace) {
        try {
            return parsingService.analyzeCOBOL(query, workspace);
        } catch (Exception e) {
            AnalyzeResult result = new AnalyzeResult(false, "COBOL analysis failed: " + e.getMessage());
            return result;
        }
    }

    @Override
    public PlanResult plan(NqlQuery query, Scope scope, Workspace workspace) {
        try {
            return migrationPlanService.createMigrationPlan(query, scope, workspace);
        } catch (Exception e) {
            return new PlanResult(false, "Migration planning failed: " + e.getMessage());
        }
    }

    @Override
    public ApplyResult apply(String planId, boolean dryRun, Workspace workspace) {
        try {
            return migrationPlanService.applyMigrationPlan(planId, dryRun, workspace);
        } catch (Exception e) {
            return new ApplyResult(false, "Migration application failed: " + e.getMessage());
        }
    }

    @Override
    public DiffResult diff(String runId, Workspace workspace) {
        try {
            return migrationPlanService.generateDiff(runId, workspace);
        } catch (Exception e) {
            return new DiffResult(false, "Diff generation failed: " + e.getMessage());
        }
    }

    @Override
    public Optional<StubResult> generateStubs(NqlQuery query, Workspace workspace) {
        try {
            return Optional.of(javaGenerationService.generateInterfaceStubs(query, workspace));
        } catch (Exception e) {
            StubResult result = new StubResult(false, "Stub generation failed: " + e.getMessage());
            return Optional.of(result);
        }
    }

    @Override
    public MetricsResult metrics(Scope scope, Workspace workspace) {
        try {
            return metricsService.calculateMetrics(scope, workspace);
        } catch (Exception e) {
            return new MetricsResult(false, "Metrics calculation failed: " + e.getMessage());
        }
    }

    /**
     * Migrate a specific COBOL copybook to Java artifacts using templates.
     */
    public StubResult migrateCopybook(NqlQuery query, Workspace workspace) {
        try {
            String copybookName = null;
            if (query.getParameters() != null) {
                Object cb = query.getParameters().get("copybook");
                if (cb != null) {
                    copybookName = cb.toString();
                }
            }
            if (copybookName == null) {
                return new StubResult(false, "No copybook specified");
            }
            final String finalCopybookName = copybookName;
            Path root = Paths.get(workspace.getPath());
            java.util.List<Path> copybooks = parsingService.findCopybooks(root);
            Optional<Path> copybookPath = copybooks.stream()
                    .filter(p -> p.getFileName().toString().equalsIgnoreCase(finalCopybookName))
                    .findFirst();
            if (copybookPath.isEmpty()) {
                return new StubResult(false, "Copybook not found: " + copybookName);
            }

            Map<String, Object> metadata = parsingService.parseCopybook(copybookPath.get(), parsingService.getDefaultDialect());
            metadata.put("filePath", copybookPath.get().toString());

            Map<String, String> generated = templateCodeGenerationService.generateFromCopybook(
                    copybookName.replaceFirst("\\.[^.]+$", ""), metadata);

            boolean success = !generated.isEmpty();
            StubResult result = new StubResult(success,
                    success ? "Generated " + generated.size() + " artifacts" : "No artifacts generated");
            result.setGeneratedCode(generated);
            return result;
        } catch (Exception e) {
            return new StubResult(false, "Copybook migration failed: " + e.getMessage());
        }
    }

    /**
     * Generate JPA artifacts from embedded DB2 EXEC SQL statements.
     */
    public StubResult migrateDb2(NqlQuery query, Workspace workspace) {
        try {
            String programName = null;
            if (query.getParameters() != null) {
                Object p = query.getParameters().get("program");
                if (p != null) {
                    programName = p.toString();
                }
            }
            if (programName == null) {
                return new StubResult(false, "No COBOL program specified");
            }
            final String finalProgramName = programName;
            Path root = Paths.get(workspace.getPath());
            java.util.List<Path> cobolFiles = parsingService.findCobolFiles(root);
            Optional<Path> programPath = cobolFiles.stream()
                    .filter(p -> p.getFileName().toString().equalsIgnoreCase(finalProgramName))
                    .findFirst();
            if (programPath.isEmpty()) {
                return new StubResult(false, "COBOL program not found: " + programName);
            }

            Map<String, String> generated = db2MigrationService.migrateCobolFile(programPath.get());
            boolean success = !generated.isEmpty();
            StubResult result = new StubResult(success,
                    success ? "Generated " + generated.size() + " artifacts" : "No SQL statements found");
            result.setGeneratedCode(generated);
            return result;
        } catch (Exception e) {
            return new StubResult(false, "DB2 migration failed: " + e.getMessage());
        }
    }

    @Override
    public java.util.List<Tool> getTools() {
        // Publish COBOL tools for MCP clients
        List<Tool> tools = new ArrayList<>();
        tools.add(new BasicTool("cobol.analyze", "Analyze COBOL sources (parsing, AST, dependencies)", baseSchema()));
        tools.add(new BasicTool("cobol.metrics", "Collect high-level COBOL metrics (files, lines, copybooks)", baseSchema()));
        tools.add(new BasicTool("cobol.plan", "Create migration plan from COBOL to Java", planSchema()));
        tools.add(new BasicTool("cobol.apply", "Apply migration plan (code generation, transforms)", applySchema()));
        tools.add(new BasicTool("cobol.diff", "Generate diff for last migration run", diffSchema()));
        // Extended provider-specific tools
        tools.add(new BasicTool("cobol.migrate_copybook", "Generate Java artifacts from a COBOL copybook (templates)", migrateCopybookSchema()));
        tools.add(new BasicTool("cobol.migrate_db2", "Generate JPA code from embedded DB2 EXEC SQL in COBOL program", migrateDb2Schema()));
        return tools;
    }

    @Override
    public Map<String, Object> executeExtendedTool(String capability, Map<String, Object> arguments) {
        if (capability == null) {
            return null;
        }
        String cap = capability.toLowerCase(Locale.ROOT);
        switch (cap) {
            case "migrate_copybook":
                return handleMigrateCopybook(arguments);
            case "migrate_db2":
                return handleMigrateDb2(arguments);
            default:
                return null; // Not handled here, allow default routing
        }
    }

    // ---- Extended tool handlers ----
    private Map<String, Object> handleMigrateCopybook(Map<String, Object> args) {
        String workspacePath = asString(args.get("workspacePath"));
        String copybook = asString(args.get("copybook"));
        Map<String, Object> response = baseResponse("stubs");
        if (workspacePath == null || workspacePath.isBlank()) {
            return error(response, "workspacePath is required");
        }
        if (copybook == null || copybook.isBlank()) {
            return error(response, "copybook is required (e.g., CUSTOMER.cpy)");
        }
        Workspace ws = new Workspace();
        ws.setId("default");
        ws.setPath(workspacePath);
        NqlQuery query = new NqlQuery();
        query.setLanguage(language());
        Map<String, Object> params = new LinkedHashMap<>();
        params.put("copybook", copybook);
        query.setParameters(params);
        StubResult result = migrateCopybook(query, ws);
        response.put("success", result.isSuccess());
        response.put("message", result.getMessage());
        Map<String, Object> data = new LinkedHashMap<>();
        data.put("generated", result.getGeneratedCode());
        response.put("data", data);
        return success(response);
    }

    private Map<String, Object> handleMigrateDb2(Map<String, Object> args) {
        String workspacePath = asString(args.get("workspacePath"));
        String program = asString(args.get("program"));
        Map<String, Object> response = baseResponse("stubs");
        if (workspacePath == null || workspacePath.isBlank()) {
            return error(response, "workspacePath is required");
        }
        if (program == null || program.isBlank()) {
            return error(response, "program is required (e.g., ORDERPROC.cbl)");
        }
        Workspace ws = new Workspace();
        ws.setId("default");
        ws.setPath(workspacePath);
        NqlQuery query = new NqlQuery();
        query.setLanguage(language());
        Map<String, Object> params = new LinkedHashMap<>();
        params.put("program", program);
        query.setParameters(params);
        StubResult result = migrateDb2(query, ws);
        response.put("success", result.isSuccess());
        response.put("message", result.getMessage());
        Map<String, Object> data = new LinkedHashMap<>();
        data.put("generated", result.getGeneratedCode());
        response.put("data", data);
        return success(response);
    }

    // ---- Schemas ----
    private Map<String, Object> baseSchema() {
        Map<String, Object> schema = new LinkedHashMap<>();
        schema.put("type", "object");
        Map<String, Object> props = new LinkedHashMap<>();
        props.put("nql", Map.of(
                "type", "string",
                "description", "NQL query to select COBOL elements or recipes"
        ));
        props.put("scope", Map.of(
                "type", "string",
                "description", "Glob pattern for files to include (e.g., **/*.cbl)"
        ));
        schema.put("properties", props);
        return schema;
    }

    private Map<String, Object> planSchema() {
        Map<String, Object> schema = baseSchema();
        @SuppressWarnings("unchecked")
        Map<String, Object> props = (Map<String, Object>) schema.get("properties");
        props.put("goals", Map.of(
                "type", "array",
                "description", "High-level migration goals (e.g., db2, jpa, rest)",
                "items", Map.of("type", "string")
        ));
        return schema;
    }

    private Map<String, Object> applySchema() {
        Map<String, Object> schema = new LinkedHashMap<>();
        schema.put("type", "object");
        Map<String, Object> props = new LinkedHashMap<>();
        props.put("planId", Map.of("type", "string", "description", "Plan id returned by cobol.plan"));
        props.put("dryRun", Map.of("type", "boolean", "description", "Simulate without writing files"));
        schema.put("properties", props);
        return schema;
    }

    private Map<String, Object> diffSchema() {
        Map<String, Object> schema = new LinkedHashMap<>();
        schema.put("type", "object");
        Map<String, Object> props = new LinkedHashMap<>();
        props.put("runId", Map.of("type", "string", "description", "Run id from previous operation"));
        schema.put("properties", props);
        return schema;
    }

    private Map<String, Object> migrateCopybookSchema() {
        Map<String, Object> schema = new LinkedHashMap<>();
        schema.put("type", "object");
        Map<String, Object> props = new LinkedHashMap<>();
        props.put("copybook", Map.of("type", "string", "description", "Copybook file name (e.g., CUSTOMER.cpy)"));
        schema.put("properties", props);
        schema.put("required", java.util.List.of("copybook"));
        return schema;
    }

    private Map<String, Object> migrateDb2Schema() {
        Map<String, Object> schema = new LinkedHashMap<>();
        schema.put("type", "object");
        Map<String, Object> props = new LinkedHashMap<>();
        props.put("program", Map.of("type", "string", "description", "COBOL program with EXEC SQL (e.g., ORDERPROC.cbl)"));
        schema.put("properties", props);
        schema.put("required", java.util.List.of("program"));
        return schema;
    }

    // ---- helpers ----
    private String asString(Object o) {
        return o == null ? null : String.valueOf(o);
    }

    private Map<String, Object> baseResponse(String type) {
        Map<String, Object> r = new LinkedHashMap<>();
        r.put("type", type);
        r.put("success", false);
        r.put("message", "");
        return r;
    }

    private Map<String, Object> success(Map<String, Object> r) {
        r.put("success", true);
        return r;
    }

    private Map<String, Object> error(Map<String, Object> r, String msg) {
        r.put("success", false);
        r.put("message", msg);
        return r;
    }
}