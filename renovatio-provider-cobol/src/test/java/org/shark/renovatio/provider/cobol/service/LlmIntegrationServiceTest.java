package org.shark.renovatio.provider.cobol.service;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.shark.renovatio.shared.nql.NqlParserService;
import org.shark.renovatio.shared.nql.NqlQuery;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import static org.junit.jupiter.api.Assertions.*;

class LlmIntegrationServiceTest {

    private LlmIntegrationService service;
    private Method parseMethod;

    @BeforeEach
    void setUp() throws Exception {
        service = new LlmIntegrationService(new NqlParserService());
        parseMethod = LlmIntegrationService.class.getDeclaredMethod("parseNqlQuery", String.class);
        parseMethod.setAccessible(true);
    }

    @Test
    void parsesValidNql() throws Exception {
        NqlQuery query = (NqlQuery) parseMethod.invoke(service, "FIND programs WHERE name = 'A'");
        assertEquals(NqlQuery.QueryType.FIND, query.getType());
        assertEquals("programs", query.getTarget());
    }

    @Test
    void rejectsInvalidNql() {
        InvocationTargetException ex = assertThrows(InvocationTargetException.class,
            () -> parseMethod.invoke(service, "INVALID"));
        assertTrue(ex.getCause() instanceof IllegalArgumentException);
    }
}
