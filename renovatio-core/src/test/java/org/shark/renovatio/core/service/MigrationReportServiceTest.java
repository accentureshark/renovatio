package org.shark.renovatio.core.service;

import org.junit.jupiter.api.Test;
import org.shark.renovatio.shared.domain.*;
import org.shark.renovatio.shared.nql.NqlQuery;
import org.shark.renovatio.shared.spi.LanguageProvider;
import org.shark.renovatio.shared.nql.NqlQuery;

import java.util.EnumSet;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.*;

class MigrationReportServiceTest {

    @Test
    void aggregatesMetricsAndRenders() {
        LanguageProviderRegistry registry = new LanguageProviderRegistry();
        registry.registerProvider(new StubProvider());

        MigrationReportService service = new MigrationReportService(registry);
        MigrationReport report = service.aggregateReport();

        assertEquals("SUCCESS", report.getStatuses().get("stub"));
        assertEquals(10.0, report.getMetrics().get("linesOfCode"));

        String html = service.renderHtml(report);
        assertTrue(html.contains("linesOfCode"));

        byte[] pdf = service.renderPdf(report);
        assertTrue(pdf.length > 0);
    }

    static class StubProvider implements LanguageProvider {
        @Override
        public String language() { return "stub"; }

        @Override
        public Set<Capabilities> capabilities() {
            return EnumSet.of(Capabilities.METRICS);
        }

        @Override
        public AnalyzeResult analyze(NqlQuery query, Workspace workspace) { return new AnalyzeResult(true, ""); }

        @Override
        public PlanResult plan(NqlQuery query, Scope scope, Workspace workspace) { return new PlanResult(true, ""); }

        @Override
        public ApplyResult apply(String planId, boolean dryRun, Workspace workspace) { return new ApplyResult(true, ""); }

        @Override
        public DiffResult diff(String runId, Workspace workspace) { return new DiffResult(true, ""); }

        @Override
        public Optional<StubResult> generateStubs(NqlQuery query, Workspace workspace) { return Optional.empty(); }

        @Override
        public MetricsResult metrics(Scope scope, Workspace workspace) {
            MetricsResult r = new MetricsResult(true, "ok");
            r.setMetrics(Map.of("linesOfCode", 10));
            return r;
        }

        @Override
        public java.util.List<org.shark.renovatio.shared.domain.Tool> getTools() {
            // Return a mock MCP-compliant tool for metrics
            Map<String, Object> inputSchema = new java.util.HashMap<>();
            inputSchema.put("type", "object");
            inputSchema.put("properties", java.util.Map.of("workspacePath", java.util.Map.of("type", "string")));
            inputSchema.put("required", java.util.List.of("workspacePath"));
            return java.util.List.of(
                new org.shark.renovatio.shared.domain.BasicTool("stub.metrics", "Stub metrics tool", inputSchema)
            );
        }
    }
}
