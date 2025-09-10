package org.shark.renovatio.provider.cobol.domain;

import java.util.LinkedHashMap;
import java.util.Map;

/**
 * Simple adapter that converts delimited text records into {@link Map}
 * structures. Each record is expected to contain comma separated
 * {@code key=value} pairs. This lightweight implementation replaces the
 * previous dependency on the jRecord library which required external JARs.
 */
public class SimpleDatasetAdapter implements DatasetAdapter<String> {

    @Override
    public Map<String, Object> toPersistable(String line) {
        Map<String, Object> record = new LinkedHashMap<>();
        if (line == null || line.isEmpty()) {
            return record;
        }

        String[] tokens = line.split(",");
        for (String token : tokens) {
            String[] pair = token.split("=", 2);
            if (pair.length == 2) {
                record.put(pair[0].trim(), pair[1].trim());
            }
        }
        return record;
    }
}
