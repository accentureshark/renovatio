package org.shark.renovatio.provider.cobol;

import org.shark.renovatio.shared.spi.LanguageProvider;
import org.shark.renovatio.shared.domain.*;
import org.shark.renovatio.shared.nql.NqlQuery;
import org.shark.renovatio.provider.cobol.service.*;
import org.springframework.stereotype.Component;

import java.util.Optional;
import java.util.Set;
import java.util.EnumSet;
import java.util.List;
import java.nio.file.Path;
import java.nio.file.Paths;

/**
 * COBOL Language Provider implementation supporting COBOL to Java migration
 * Implements parsing, analysis, code generation and migration capabilities
 */
@Component
public class CobolLanguageProvider implements LanguageProvider {
    
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

            Path root = Paths.get(workspace.getPath());
            List<Path> copybooks = parsingService.findCopybooks(root);
            Optional<Path> copybookPath = copybooks.stream()
                    .filter(p -> p.getFileName().toString().equalsIgnoreCase(copybookName))
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

            Path root = Paths.get(workspace.getPath());
            List<Path> cobolFiles = parsingService.findCobolFiles(root);
            Optional<Path> programPath = cobolFiles.stream()
                    .filter(p -> p.getFileName().toString().equalsIgnoreCase(programName))
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
}