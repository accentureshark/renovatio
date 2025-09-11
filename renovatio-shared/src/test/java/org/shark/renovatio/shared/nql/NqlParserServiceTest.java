package org.shark.renovatio.shared.nql;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class NqlParserServiceTest {

    private final NqlParserService service = new NqlParserService();

    @Test
    void parsesValidQuery() {
        String q = "FIND programs WHERE name = 'TEST' IN workspace RETURN name";
        NqlQuery query = service.parse(q);
        assertEquals(NqlQuery.QueryType.FIND, query.getType());
        assertEquals("programs", query.getTarget());
        assertEquals("name = 'TEST'", query.getPredicate());
        assertEquals("workspace", query.getScope());
        assertEquals("name", query.getReturnClause());
    }

    @Test
    void rejectsInvalidQuery() {
        assertThrows(IllegalArgumentException.class, () -> service.parse("INVALID"));
    }
}
