package org.shark.renovatio.provider.cobol.service;

import io.github.resilience4j.circuitbreaker.annotation.CircuitBreaker;
import io.github.resilience4j.retry.annotation.Retry;
import io.github.resilience4j.timelimiter.annotation.TimeLimiter;
import org.shark.renovatio.shared.domain.*;
import org.shark.renovatio.shared.nql.NqlQuery;
import org.springframework.stereotype.Service;

import java.util.concurrent.CompletableFuture;

/**
 * Resilient COBOL migration service with circuit breakers, retries, and timeouts
 */
@Service
public class ResilientMigrationService {
    
    private final CobolParsingService parsingService;
    private final JavaGenerationService javaGenerationService;
    private final RecipeBasedMigrationPlanService migrationPlanService;
    
    public ResilientMigrationService(
            CobolParsingService parsingService,
            JavaGenerationService javaGenerationService,
            RecipeBasedMigrationPlanService migrationPlanService) {
        this.parsingService = parsingService;
        this.javaGenerationService = javaGenerationService;
        this.migrationPlanService = migrationPlanService;
    }
    
    /**
     * Resilient COBOL analysis with circuit breaker and retry
     */
    @CircuitBreaker(name = "cobol-analysis", fallbackMethod = "fallbackAnalyze")
    @Retry(name = "cobol-analysis")
    @TimeLimiter(name = "cobol-analysis")
    public CompletableFuture<AnalyzeResult> analyzeAsync(NqlQuery query, Workspace workspace) {
        return CompletableFuture.supplyAsync(() -> parsingService.analyzeCOBOL(query, workspace));
    }
    
    /**
     * Resilient Java code generation with circuit breaker
     */
    @CircuitBreaker(name = "java-generation", fallbackMethod = "fallbackGenerateStubs")
    @Retry(name = "java-generation")
    @TimeLimiter(name = "java-generation")
    public CompletableFuture<StubResult> generateStubsAsync(NqlQuery query, Workspace workspace) {
        return CompletableFuture.supplyAsync(() -> javaGenerationService.generateInterfaceStubs(query, workspace));
    }
    
    /**
     * Resilient migration planning with circuit breaker
     */
    @CircuitBreaker(name = "migration-planning", fallbackMethod = "fallbackCreatePlan")
    @Retry(name = "migration-planning")
    @TimeLimiter(name = "migration-planning")
    public CompletableFuture<PlanResult> createPlanAsync(NqlQuery query, Scope scope, Workspace workspace) {
        return CompletableFuture.supplyAsync(() -> migrationPlanService.createMigrationPlan(query, scope, workspace));
    }
    
    /**
     * Resilient plan application with circuit breaker
     */
    @CircuitBreaker(name = "plan-application", fallbackMethod = "fallbackApplyPlan")
    @Retry(name = "plan-application")
    @TimeLimiter(name = "plan-application")
    public CompletableFuture<ApplyResult> applyPlanAsync(String planId, boolean dryRun, Workspace workspace) {
        return CompletableFuture.supplyAsync(() -> migrationPlanService.applyMigrationPlan(planId, dryRun, workspace));
    }
    
    // Fallback methods
    
    /**
     * Fallback method for COBOL analysis failures
     */
    public CompletableFuture<AnalyzeResult> fallbackAnalyze(NqlQuery query, Workspace workspace, Exception ex) {
        AnalyzeResult result = new AnalyzeResult(false, "Analysis service unavailable: " + ex.getMessage());
        result.setRunId("fallback-" + System.currentTimeMillis());
        return CompletableFuture.completedFuture(result);
    }
    
    /**
     * Fallback method for Java generation failures
     */
    public CompletableFuture<StubResult> fallbackGenerateStubs(NqlQuery query, Workspace workspace, Exception ex) {
        StubResult result = new StubResult(false, "Code generation service unavailable: " + ex.getMessage());
        result.setRunId("fallback-" + System.currentTimeMillis());
        return CompletableFuture.completedFuture(result);
    }
    
    /**
     * Fallback method for migration planning failures
     */
    public CompletableFuture<PlanResult> fallbackCreatePlan(NqlQuery query, Scope scope, Workspace workspace, Exception ex) {
        PlanResult result = new PlanResult(false, "Migration planning service unavailable: " + ex.getMessage());
        result.setRunId("fallback-" + System.currentTimeMillis());
        return CompletableFuture.completedFuture(result);
    }
    
    /**
     * Fallback method for plan application failures
     */
    public CompletableFuture<ApplyResult> fallbackApplyPlan(String planId, boolean dryRun, Workspace workspace, Exception ex) {
        ApplyResult result = new ApplyResult(false, "Plan application service unavailable: " + ex.getMessage());
        result.setRunId("fallback-" + System.currentTimeMillis());
        return CompletableFuture.completedFuture(result);
    }
}