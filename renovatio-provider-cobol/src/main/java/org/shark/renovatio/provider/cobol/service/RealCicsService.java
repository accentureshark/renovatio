package org.shark.renovatio.provider.cobol.service;

import java.util.HashMap;
import java.util.Map;

/**
 * Implementation of {@link CicsService} that delegates calls to a
 * Zowe CICS/JCICS REST endpoint. Transactions are mapped to concrete
 * REST paths which are invoked using {@link ZoweCicsClient}.
 */
public class RealCicsService implements CicsService {

    private final String baseUrl;
    private final Map<String, String> transactionEndpoints = new HashMap<>();
    private final ZoweCicsClient client = new ZoweCicsClient();

    public RealCicsService(String baseUrl) {
        this.baseUrl = baseUrl.endsWith("/") ? baseUrl.substring(0, baseUrl.length() - 1) : baseUrl;
    }

    public RealCicsService(String baseUrl, Map<String, String> mappings) {
        this(baseUrl);
        if (mappings != null) {
            transactionEndpoints.putAll(mappings);
        }
    }

    /**
     * Register a mapping between a CICS transaction and a REST endpoint path.
     */
    public void registerTransaction(String transaction, String endpoint) {
        transactionEndpoints.put(transaction, endpoint);
    }

    @Override
    public String invokeTransaction(String transaction, Map<String, Object> payload) {
        String endpoint = transactionEndpoints.getOrDefault(transaction, "/api/cics/" + transaction.toLowerCase());
        String url = baseUrl + endpoint;
        return client.postJson(url, payload);
    }
}
