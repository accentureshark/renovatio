package org.shark.renovatio.core.service;

import org.apache.pdfbox.pdmodel.PDDocument;
import org.apache.pdfbox.pdmodel.PDPage;
import org.apache.pdfbox.pdmodel.PDPageContentStream;
import org.apache.pdfbox.pdmodel.font.PDType1Font;
import org.shark.renovatio.shared.domain.MetricsResult;
import org.shark.renovatio.shared.domain.MigrationReport;
import org.shark.renovatio.shared.domain.Scope;
import org.shark.renovatio.shared.domain.Workspace;
import org.shark.renovatio.shared.spi.LanguageProvider;
import org.springframework.stereotype.Service;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

/**
 * Service that aggregates metrics and renders reports in different formats.
 */
@Service
public class MigrationReportService {
    private final LanguageProviderRegistry registry;

    public MigrationReportService(LanguageProviderRegistry registry) {
        this.registry = registry;
    }

    /**
     * Aggregate metrics and statuses from all registered providers.
     */
    public MigrationReport aggregateReport() {
        MigrationReport report = new MigrationReport();
        Scope scope = new Scope();
        Workspace workspace = new Workspace("report", ".", "main");
        List<LanguageProvider> providers = new ArrayList<>(registry.getAllProviders());
        for (LanguageProvider provider : providers) {
            MetricsResult metrics = provider.metrics(scope, workspace);
            report.addStatus(provider.language(), metrics.isSuccess());
            report.addMetrics(metrics.getMetrics());
        }
        return report;
    }

    /**
     * Render report as HTML string.
     */
    public String renderHtml(MigrationReport report) {
        StringBuilder sb = new StringBuilder();
        sb.append("<html><body><h1>Migration Report</h1>");
        sb.append("<h2>Statuses</h2><ul>");
        report.getStatuses().forEach((k, v) ->
                sb.append("<li>").append(k).append(": ").append(v).append("</li>"));
        sb.append("</ul><h2>Metrics</h2><ul>");
        report.getMetrics().forEach((k, v) ->
                sb.append("<li>").append(k).append(": ").append(String.format("%.2f", v)).append("</li>"));
        sb.append("</ul></body></html>");
        return sb.toString();
    }

    /**
     * Render report as PDF bytes.
     */
    public byte[] renderPdf(MigrationReport report) {
        try (PDDocument doc = new PDDocument()) {
            PDPage page = new PDPage();
            doc.addPage(page);
            try (PDPageContentStream content = new PDPageContentStream(doc, page)) {
                content.beginText();
                content.setFont(PDType1Font.HELVETICA, 12);
                content.newLineAtOffset(50, 750);
                content.showText("Migration Report");
                content.newLineAtOffset(0, -20);
                content.showText("Statuses:");
                for (var entry : report.getStatuses().entrySet()) {
                    content.newLineAtOffset(0, -15);
                    content.showText(entry.getKey() + ": " + entry.getValue());
                }
                content.newLineAtOffset(0, -20);
                content.showText("Metrics:");
                for (var entry : report.getMetrics().entrySet()) {
                    content.newLineAtOffset(0, -15);
                    content.showText(entry.getKey() + ": " + String.format("%.2f", entry.getValue()));
                }
                content.endText();
            }
            ByteArrayOutputStream out = new ByteArrayOutputStream();
            doc.save(out);
            return out.toByteArray();
        } catch (IOException e) {
            throw new RuntimeException("Failed to render PDF", e);
        }
    }
}
