package org.shark.renovatio.shared.domain;

/**
 * Roles used for accessing generated reports.
 */
public enum AccessRole {
    ADMIN,
    MANAGER,
    VIEWER;

    /**
     * Parse a string to an AccessRole. Returns null if the role is invalid.
     */
    public static AccessRole fromString(String role) {
        if (role == null) {
            return null;
        }
        try {
            return AccessRole.valueOf(role.toUpperCase());
        } catch (IllegalArgumentException ex) {
            return null;
        }
    }
}
