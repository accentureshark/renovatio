package org.shark.renovatio.provider.cobol.service;

import org.shark.renovatio.shared.domain.*;
import org.shark.renovatio.shared.nql.NqlQuery;
import org.shark.renovatio.shared.util.BenchmarkUtils;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;
import java.util.*;

/**
 * Migration planning service for COBOL to Java transformation
 * Creates execution plans, applies migrations, and generates diffs
 */
@Service
public class MigrationPlanService {

    private final CobolParsingService parsingService;
    private final JavaGenerationService javaGenerationService;
    private final Map<String, MigrationPlan> activePlans = new HashMap<>();
    private final Map<String, MigrationRun> completedRuns = new HashMap<>();

    public MigrationPlanService(CobolParsingService parsingService, JavaGenerationService javaGenerationService) {
        this.parsingService = parsingService;
        this.javaGenerationService = javaGenerationService;
    }

    /**
     * Creates a migration plan for COBOL to Java transformation
     */
    public PlanResult createMigrationPlan(NqlQuery query, Scope scope, Workspace workspace) {
        try {
            String planId = UUID.randomUUID().toString();

            // Analyze COBOL programs in scope
            AnalyzeResult analyzeResult = parsingService.analyzeCOBOL(query, workspace);
            if (!analyzeResult.isSuccess()) {
                return new PlanResult(false, "Failed to analyze COBOL for planning: " + analyzeResult.getMessage());
            }

            MigrationPlan plan = new MigrationPlan();
            plan.setId(planId);
            plan.setCreatedAt(LocalDateTime.now());
            plan.setQuery(query);
            plan.setScope(scope);
            plan.setWorkspace(workspace);

            // Create migration steps
            List<MigrationStep> steps = createMigrationSteps(analyzeResult, query);
            plan.setSteps(steps);

            activePlans.put(planId, plan);

            PlanResult result = new PlanResult(true, "Migration plan created successfully");
            result.setPlanId(planId);
            result.setPlanContent(generatePlanSummary(plan));

            Map<String, Object> stepsMap = new HashMap<>();
            for (int i = 0; i < steps.size(); i++) {
                stepsMap.put("step" + (i + 1), steps.get(i).getDescription());
            }
            result.setSteps(stepsMap);

            return result;

        } catch (Exception e) {
            return new PlanResult(false, "Migration planning failed: " + e.getMessage());
        }
    }

    /**
     * Applies a migration plan
     */
    public ApplyResult applyMigrationPlan(String planId, boolean dryRun, Workspace workspace) {
        try {
            MigrationPlan plan = activePlans.get(planId);
            if (plan == null) {
                return new ApplyResult(false, "Migration plan not found: " + planId);
            }

            String runId = UUID.randomUUID().toString();
            MigrationRun run = new MigrationRun();
            run.setId(runId);
            run.setPlanId(planId);
            run.setStartedAt(LocalDateTime.now());
            run.setDryRun(dryRun);

            List<String> executedSteps = new ArrayList<>();
            Map<String, String> generatedFiles = new HashMap<>();

            AnalyzeResult baseline = parsingService.analyzeCOBOL(plan.getQuery(), workspace);

            long migrationStart = System.nanoTime();
            for (MigrationStep step : plan.getSteps()) {
                try {
                    if (step.getType() == StepType.GENERATE_JAVA_STUBS) {
                        // Generate Java stubs
                        StubResult stubResult = javaGenerationService.generateInterfaceStubs(plan.getQuery(), workspace);
                        if (stubResult.isSuccess() && stubResult.getGeneratedCode() != null) {
                            generatedFiles.putAll(stubResult.getGeneratedCode());
                        }
                    }

                    executedSteps.add(step.getDescription());

                } catch (Exception e) {
                    run.setError("Failed to execute step: " + step.getDescription() + " - " + e.getMessage());
                    break;
                }
            }
            long migratedElapsed = System.nanoTime() - migrationStart;
            AnalyzeResult migrated = new AnalyzeResult(true, "Migration execution");
            migrated.setPerformance(new PerformanceMetrics(migratedElapsed / 1_000_000));

            run.setCompletedAt(LocalDateTime.now());
            run.setExecutedSteps(executedSteps);
            run.setGeneratedFiles(generatedFiles);
            completedRuns.put(runId, run);

            ApplyResult result = new ApplyResult(true, "Migration plan applied successfully");
            result.setRunId(runId);
            result.setModifiedFiles(new ArrayList<>(generatedFiles.keySet()));

            Map<String, Object> changes = new HashMap<>();
            changes.put("generatedFiles", generatedFiles.size());
            changes.put("executedSteps", executedSteps.size());
            changes.put("dryRun", dryRun);
            changes.put("performance", BenchmarkUtils.compare(baseline, migrated));
            result.setChanges(changes);

            return result;

        } catch (Exception e) {
            return new ApplyResult(false, "Migration application failed: " + e.getMessage());
        }
    }

    /**
     * Generates a diff for a completed migration run
     */
    public DiffResult generateDiff(String runId, Workspace workspace) {
        try {
            MigrationRun run = completedRuns.get(runId);
            if (run == null) {
                return new DiffResult(false, "Migration run not found: " + runId);
            }

            StringBuilder unifiedDiff = new StringBuilder();
            unifiedDiff.append("Migration Run: ").append(runId).append("\n");
            unifiedDiff.append("Plan ID: ").append(run.getPlanId()).append("\n");
            unifiedDiff.append("Started: ").append(run.getStartedAt()).append("\n");
            unifiedDiff.append("Completed: ").append(run.getCompletedAt()).append("\n");
            unifiedDiff.append("\nGenerated Files:\n");

            if (run.getGeneratedFiles() != null) {
                for (Map.Entry<String, String> entry : run.getGeneratedFiles().entrySet()) {
                    unifiedDiff.append("+ ").append(entry.getKey()).append("\n");
                }
            }

            DiffResult result = new DiffResult(true, "Diff generated successfully");
            result.setUnifiedDiff(unifiedDiff.toString());

            Map<String, Object> semanticDiff = new HashMap<>();
            semanticDiff.put("addedFiles", run.getGeneratedFiles() != null ? run.getGeneratedFiles().size() : 0);
            semanticDiff.put("modifiedFiles", 0);
            semanticDiff.put("deletedFiles", 0);
            result.setSemanticDiff(semanticDiff);

            return result;

        } catch (Exception e) {
            return new DiffResult(false, "Diff generation failed: " + e.getMessage());
        }
    }

    /**
     * Creates migration steps based on analysis results
     */
    private List<MigrationStep> createMigrationSteps(AnalyzeResult analyzeResult, NqlQuery query) {
        List<MigrationStep> steps = new ArrayList<>();

        // Step 1: Parse COBOL programs
        steps.add(new MigrationStep(
                StepType.PARSE_COBOL,
                "Parse COBOL programs and extract AST",
                "Analyze COBOL source code structure"
        ));

        // Step 2: Generate Java DTOs
        steps.add(new MigrationStep(
                StepType.GENERATE_JAVA_DTOS,
                "Generate Java DTO classes from COBOL data structures",
                "Create Java classes representing COBOL data items"
        ));

        // Step 3: Generate Java service interfaces
        steps.add(new MigrationStep(
                StepType.GENERATE_JAVA_STUBS,
                "Generate Java service interfaces and implementation templates",
                "Create Java service layer for COBOL business logic"
        ));

        // Step 4: Create migration mapping
        steps.add(new MigrationStep(
                StepType.CREATE_MAPPINGS,
                "Create MapStruct mappings between COBOL and Java structures",
                "Generate mapping classes for data transformation"
        ));

        // Step 5: Generate tests
        steps.add(new MigrationStep(
                StepType.GENERATE_TESTS,
                "Generate unit tests for migrated Java code",
                "Create test classes to validate migration"
        ));

        return steps;
    }

    /**
     * Generates a human-readable plan summary
     */
    private String generatePlanSummary(MigrationPlan plan) {
        StringBuilder summary = new StringBuilder();
        summary.append("COBOL to Java Migration Plan\n");
        summary.append("Plan ID: ").append(plan.getId()).append("\n");
        summary.append("Created: ").append(plan.getCreatedAt()).append("\n");
        summary.append("Steps: ").append(plan.getSteps().size()).append("\n\n");

        for (int i = 0; i < plan.getSteps().size(); i++) {
            MigrationStep step = plan.getSteps().get(i);
            summary.append(i + 1).append(". ").append(step.getDescription()).append("\n");
            summary.append("   ").append(step.getDetails()).append("\n\n");
        }

        return summary.toString();
    }

    /**
     * Migration step types
     */
    private enum StepType {
        PARSE_COBOL,
        GENERATE_JAVA_DTOS,
        GENERATE_JAVA_STUBS,
        CREATE_MAPPINGS,
        GENERATE_TESTS
    }

    /**
     * Migration plan data structure
     */
    private static class MigrationPlan {
        private String id;
        private LocalDateTime createdAt;
        private NqlQuery query;
        private Scope scope;
        private Workspace workspace;
        private List<MigrationStep> steps;

        // Getters and setters
        public String getId() {
            return id;
        }

        public void setId(String id) {
            this.id = id;
        }

        public LocalDateTime getCreatedAt() {
            return createdAt;
        }

        public void setCreatedAt(LocalDateTime createdAt) {
            this.createdAt = createdAt;
        }

        public NqlQuery getQuery() {
            return query;
        }

        public void setQuery(NqlQuery query) {
            this.query = query;
        }

        public Scope getScope() {
            return scope;
        }

        public void setScope(Scope scope) {
            this.scope = scope;
        }

        public Workspace getWorkspace() {
            return workspace;
        }

        public void setWorkspace(Workspace workspace) {
            this.workspace = workspace;
        }

        public List<MigrationStep> getSteps() {
            return steps;
        }

        public void setSteps(List<MigrationStep> steps) {
            this.steps = steps;
        }
    }

    /**
     * Migration step data structure
     */
    private static class MigrationStep {
        private StepType type;
        private String description;
        private String details;

        public MigrationStep(StepType type, String description, String details) {
            this.type = type;
            this.description = description;
            this.details = details;
        }

        public StepType getType() {
            return type;
        }

        public String getDescription() {
            return description;
        }

        public String getDetails() {
            return details;
        }
    }

    /**
     * Migration run data structure
     */
    private static class MigrationRun {
        private String id;
        private String planId;
        private LocalDateTime startedAt;
        private LocalDateTime completedAt;
        private boolean dryRun;
        private List<String> executedSteps;
        private Map<String, String> generatedFiles;
        private String error;

        // Getters and setters
        public String getId() {
            return id;
        }

        public void setId(String id) {
            this.id = id;
        }

        public String getPlanId() {
            return planId;
        }

        public void setPlanId(String planId) {
            this.planId = planId;
        }

        public LocalDateTime getStartedAt() {
            return startedAt;
        }

        public void setStartedAt(LocalDateTime startedAt) {
            this.startedAt = startedAt;
        }

        public LocalDateTime getCompletedAt() {
            return completedAt;
        }

        public void setCompletedAt(LocalDateTime completedAt) {
            this.completedAt = completedAt;
        }

        public boolean isDryRun() {
            return dryRun;
        }

        public void setDryRun(boolean dryRun) {
            this.dryRun = dryRun;
        }

        public List<String> getExecutedSteps() {
            return executedSteps;
        }

        public void setExecutedSteps(List<String> executedSteps) {
            this.executedSteps = executedSteps;
        }

        public Map<String, String> getGeneratedFiles() {
            return generatedFiles;
        }

        public void setGeneratedFiles(Map<String, String> generatedFiles) {
            this.generatedFiles = generatedFiles;
        }

        public String getError() {
            return error;
        }

        public void setError(String error) {
            this.error = error;
        }
    }
}