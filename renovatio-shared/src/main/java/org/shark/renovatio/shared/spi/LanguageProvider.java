package org.shark.renovatio.shared.spi;

import org.shark.renovatio.shared.domain.*;
import org.shark.renovatio.shared.nql.NqlQuery;

import java.util.Optional;
import java.util.Set;

/**
 * Service Provider Interface for language-specific implementations.
 * Each language provider implements the capabilities it supports.
 */
public interface LanguageProvider {

    /**
     * @return the language this provider supports ("java", "cobol", etc.)
     */
    String language();

    /**
     * @return the capabilities this provider supports
     */
    Set<Capabilities> capabilities();

    /**
     * Analyze code and extract information (AST, dependencies, etc.)
     */
    AnalyzeResult analyze(NqlQuery query, Workspace workspace);

    /**
     * Create an execution plan for the given query
     */
    PlanResult plan(NqlQuery query, Scope scope, Workspace workspace);

    /**
     * Apply a previously created plan
     */
    ApplyResult apply(String planId, boolean dryRun, Workspace workspace);

    /**
     * Generate diff showing changes made by a run
     */
    DiffResult diff(String runId, Workspace workspace);

    /**
     * Generate stubs/adapters for interfacing with other languages
     */
    Optional<StubResult> generateStubs(NqlQuery query, Workspace workspace);

    /**
     * Calculate metrics for the given scope
     */
    MetricsResult metrics(Scope scope, Workspace workspace);

    /**
     * Returns the list of MCP-compliant tools exposed by this provider.
     */
    java.util.List<org.shark.renovatio.shared.domain.Tool> getTools();

    /**
     * Capabilities supported by a language provider
     */
    enum Capabilities {
        ANALYZE,      // Can analyze code structure
        PLAN,         // Can create execution plans  
        APPLY,        // Can apply transformations directly
        DIFF,         // Can generate semantic diffs
        STUBS,        // Can generate interface stubs
        METRICS       // Can calculate code metrics
    }
}