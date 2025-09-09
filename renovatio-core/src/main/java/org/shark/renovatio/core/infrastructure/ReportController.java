package org.shark.renovatio.core.infrastructure;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.shark.renovatio.core.service.MigrationReportService;
import org.shark.renovatio.core.service.ReportAccessService;
import org.shark.renovatio.shared.domain.AccessRole;
import org.shark.renovatio.shared.domain.MigrationReport;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * REST controller exposing report endpoints.
 */
@RestController
@RequestMapping("/reports")
@Tag(name = "Reports")
public class ReportController {
    private final MigrationReportService reportService;
    private final ReportAccessService accessService;

    public ReportController(MigrationReportService reportService, ReportAccessService accessService) {
        this.reportService = reportService;
        this.accessService = accessService;
    }

    @GetMapping(value = "/html", produces = MediaType.TEXT_HTML_VALUE)
    @Operation(summary = "Get migration report in HTML format")
    public ResponseEntity<String> getHtmlReport(@RequestHeader(value = "X-Role", required = false) String roleHeader) {
        AccessRole role = AccessRole.fromString(roleHeader);
        if (!accessService.canView(role)) {
            return ResponseEntity.status(HttpStatus.FORBIDDEN).build();
        }
        MigrationReport report = reportService.aggregateReport();
        return ResponseEntity.ok(reportService.renderHtml(report));
    }

    @GetMapping(value = "/pdf", produces = MediaType.APPLICATION_PDF_VALUE)
    @Operation(summary = "Get migration report in PDF format")
    public ResponseEntity<byte[]> getPdfReport(@RequestHeader(value = "X-Role", required = false) String roleHeader) {
        AccessRole role = AccessRole.fromString(roleHeader);
        if (!accessService.canView(role)) {
            return ResponseEntity.status(HttpStatus.FORBIDDEN).build();
        }
        MigrationReport report = reportService.aggregateReport();
        byte[] pdf = reportService.renderPdf(report);
        return ResponseEntity.ok()
                .header(HttpHeaders.CONTENT_DISPOSITION, "attachment; filename=report.pdf")
                .body(pdf);
    }
}
