package org.shark.renovatio.provider.cobol.service;

import java.io.IOException;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * Minimal HTTP client for invoking CICS transactions exposed via
 * Zowe/JCICS REST APIs. This implementation keeps dependencies light
 * by relying solely on {@link java.net.http.HttpClient}.
 */
public class ZoweCicsClient {

    private final HttpClient httpClient = HttpClient.newHttpClient();

    /**
     * POST the given payload as JSON to the specified URL.
     *
     * @param url     target endpoint
     * @param payload map of values to be serialised as JSON
     * @return response body as plain text
     */
    public String postJson(String url, Map<String, Object> payload) {
        String body = toJson(payload);
        HttpRequest request = HttpRequest.newBuilder()
                .uri(URI.create(url))
                .header("Content-Type", "application/json")
                .POST(HttpRequest.BodyPublishers.ofString(body))
                .build();
        try {
            HttpResponse<String> response = httpClient.send(request, HttpResponse.BodyHandlers.ofString());
            return response.body();
        } catch (IOException | InterruptedException e) {
            Thread.currentThread().interrupt();
            throw new RuntimeException("Failed to invoke CICS endpoint", e);
        }
    }

    private String toJson(Map<String, Object> payload) {
        if (payload == null || payload.isEmpty()) {
            return "{}";
        }
        return payload.entrySet().stream()
                .map(e -> "\"" + e.getKey() + "\":\"" + String.valueOf(e.getValue()) + "\"")
                .collect(Collectors.joining(",", "{", "}"));
    }
}
