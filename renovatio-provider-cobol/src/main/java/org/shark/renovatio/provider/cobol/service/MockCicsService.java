package org.shark.renovatio.provider.cobol.service;

import java.util.Map;

/**
 * Mock implementation of {@link CicsService} that returns static responses.
 */
public class MockCicsService implements CicsService {

    @Override
    public String invokeTransaction(String transaction, Map<String, Object> payload) {
        return "Mocked response for " + transaction;
    }
}

