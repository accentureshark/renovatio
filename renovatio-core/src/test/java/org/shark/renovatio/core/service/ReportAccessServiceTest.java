package org.shark.renovatio.core.service;

import org.junit.jupiter.api.Test;
import org.shark.renovatio.shared.domain.AccessRole;
import static org.junit.jupiter.api.Assertions.*;

class ReportAccessServiceTest {

    @Test
    void testRoleAccess() {
        ReportAccessService service = new ReportAccessService();
        assertTrue(service.canView(AccessRole.ADMIN));
        assertTrue(service.canView(AccessRole.MANAGER));
        assertFalse(service.canView(AccessRole.VIEWER));
        assertFalse(service.canView(null));
    }
}
