package org.shark.renovatio.shared.domain;

import java.util.Map;

/**
 * Workspace context for operations
 */
public class Workspace {
    private String id;
    private String path;
    private String branch;
    private Map<String, Object> metadata;

    public Workspace() {
    }

    public Workspace(String id, String path, String branch) {
        this.id = id;
        this.path = path;
        this.branch = branch;
    }

    // Getters and setters
    public String getId() {
        return id;
    }

    public void setId(String id) {
        this.id = id;
    }

    public String getPath() {
        return path;
    }

    public void setPath(String path) {
        this.path = path;
    }

    public String getBranch() {
        return branch;
    }

    public void setBranch(String branch) {
        this.branch = branch;
    }

    public Map<String, Object> getMetadata() {
        return metadata;
    }

    public void setMetadata(Map<String, Object> metadata) {
        this.metadata = metadata;
    }
}