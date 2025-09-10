package org.shark.renovatio.provider.cobol.domain;

import org.junit.jupiter.api.Test;

import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class SimpleDatasetAdapterTest {

    @Test
    void parsesDelimitedKeyValuePairs() {
        SimpleDatasetAdapter adapter = new SimpleDatasetAdapter();
        Map<String, Object> result = adapter.toPersistable("name=John, age=30");
        assertEquals("John", result.get("name"));
        assertEquals("30", result.get("age"));
        assertTrue(result.size() >= 2);
    }
}
