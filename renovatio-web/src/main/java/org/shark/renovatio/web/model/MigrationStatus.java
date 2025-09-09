package org.shark.renovatio.web.model;

public class MigrationStatus {
    private int migratedFiles;
    private int totalFiles;
    private int errorCount;

    public MigrationStatus(int totalFiles) {
        this.totalFiles = totalFiles;
    }

    public int getMigratedFiles() {
        return migratedFiles;
    }

    public void setMigratedFiles(int migratedFiles) {
        this.migratedFiles = migratedFiles;
    }

    public int getTotalFiles() {
        return totalFiles;
    }

    public int getErrorCount() {
        return errorCount;
    }

    public void setErrorCount(int errorCount) {
        this.errorCount = errorCount;
    }

    public double getProgress() {
        return totalFiles == 0 ? 0 : (double) migratedFiles / totalFiles;
    }
}
