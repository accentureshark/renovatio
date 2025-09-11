package org.shark.renovatio.provider.java;

import org.shark.renovatio.shared.spi.BaseLanguageProvider;
import org.shark.renovatio.shared.domain.*;
import org.shark.renovatio.shared.nql.NqlQuery;
import java.util.EnumSet;
import java.util.Optional;
import java.util.Set;

public class JavaLanguageProvider extends BaseLanguageProvider {
    public JavaLanguageProvider() {}

    @Override
    public String language() {
        return "java";
    }

    @Override
    public Set<Capabilities> capabilities() {
        return EnumSet.of(Capabilities.ANALYZE, Capabilities.PLAN, Capabilities.APPLY, Capabilities.DIFF, Capabilities.METRICS);
    }

    @Override
    public AnalyzeResult analyze(NqlQuery query, Workspace workspace) {
        return new AnalyzeResult();
    }

    @Override
    public PlanResult plan(NqlQuery query, Scope scope, Workspace workspace) {
        return new PlanResult();
    }

    @Override
    public ApplyResult apply(String planId, boolean dryRun, Workspace workspace) {
        return new ApplyResult();
    }

    @Override
    public DiffResult diff(String runId, Workspace workspace) {
        return new DiffResult();
    }

    @Override
    public Optional<StubResult> generateStubs(NqlQuery query, Workspace workspace) {
        return Optional.empty();
    }

    @Override
    public MetricsResult metrics(Scope scope, Workspace workspace) {
        return new MetricsResult();
    }
}
