package org.shark.renovatio.web.controller;

import org.shark.renovatio.web.model.MigrationStatus;
import org.shark.renovatio.web.service.MigrationStatusService;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/migration")
public class MigrationController {

    private final MigrationStatusService statusService;

    public MigrationController(MigrationStatusService statusService) {
        this.statusService = statusService;
    }

    @GetMapping("/status")
    public MigrationStatus getStatus() {
        return statusService.getStatus();
    }
}
