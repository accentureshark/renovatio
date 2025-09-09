package org.shark.renovatio.provider.cobol.service;

import java.util.Map;

/**
 * Simple abstraction for invoking CICS transactions.
 */
public interface CicsService {

    /**
     * Invoke the given CICS transaction with the provided payload.
     *
     * @param transaction the CICS transaction or program name
     * @param payload     input data
     * @return raw response from the CICS layer
     */
    String invokeTransaction(String transaction, Map<String, Object> payload);
}

