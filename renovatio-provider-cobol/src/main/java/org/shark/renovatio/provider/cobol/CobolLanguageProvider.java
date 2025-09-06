package org.shark.renovatio.provider.cobol;

import org.shark.renovatio.shared.spi.LanguageProvider;
import org.shark.renovatio.shared.domain.*;
import org.shark.renovatio.shared.nql.NqlQuery;
import org.shark.renovatio.provider.cobol.service.*;
import org.springframework.stereotype.Component;

import java.util.Optional;
import java.util.Set;
import java.util.EnumSet;

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
    
    public CobolLanguageProvider(
            CobolParsingService parsingService,
            JavaGenerationService javaGenerationService,
            MigrationPlanService migrationPlanService,
            IndexingService indexingService,
            MetricsService metricsService) {
        this.parsingService = parsingService;
        this.javaGenerationService = javaGenerationService;
        this.migrationPlanService = migrationPlanService;
        this.indexingService = indexingService;
        this.metricsService = metricsService;
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
}