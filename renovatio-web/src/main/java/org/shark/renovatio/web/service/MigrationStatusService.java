package org.shark.renovatio.web.service;

import org.shark.renovatio.web.model.MigrationStatus;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Service;

@Service
public class MigrationStatusService {
    private final MigrationStatus status = new MigrationStatus(100);

    @Scheduled(fixedRate = 1000)
    public void simulateMigration() {
        if (status.getMigratedFiles() < status.getTotalFiles()) {
            status.setMigratedFiles(status.getMigratedFiles() + 1);
        }
    }

    public MigrationStatus getStatus() {
        return status;
    }
}
