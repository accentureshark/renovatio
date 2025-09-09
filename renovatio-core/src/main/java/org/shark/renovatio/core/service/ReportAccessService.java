package org.shark.renovatio.core.service;

import org.shark.renovatio.shared.domain.AccessRole;
import org.springframework.stereotype.Service;

import java.util.EnumSet;
import java.util.Set;

/**
 * Simple access control service for report viewing.
 */
@Service
public class ReportAccessService {
    private final Set<AccessRole> allowedRoles = EnumSet.of(AccessRole.ADMIN, AccessRole.MANAGER);

    /**
     * Check if the given role can view reports.
     */
    public boolean canView(AccessRole role) {
        return role != null && allowedRoles.contains(role);
    }
}
