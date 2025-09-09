package org.shark.renovatio.provider.cobol.domain;

import java.util.Map;

/**
 * Generic adapter that transforms dataset representations into
 * persistable structures such as {@link java.util.Map}.
 *
 * @param <T> source dataset type
 */
public interface DatasetAdapter<T> {

    /**
     * Transform the provided dataset into a persistable map of field names to values.
     *
     * @param dataset source dataset instance
     * @return map representation suitable for persistence
     */
    Map<String, Object> toPersistable(T dataset);
}
