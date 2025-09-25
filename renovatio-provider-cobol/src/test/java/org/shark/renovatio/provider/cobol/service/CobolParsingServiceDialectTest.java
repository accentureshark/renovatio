package org.shark.renovatio.provider.cobol.service;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Simple tests verifying that the dialect selection logic in
 * {@link CobolParsingService} works as expected. The parser is not dialected
 * differently in this lightweight test but the returned metadata should expose
 * the chosen dialect.
 */
class CobolParsingServiceDialectTest {

    @TempDir
    Path tempDir;

    @Test
    void dialectCanBeOverriddenPerCall() throws Exception {
        CobolParsingService service = new CobolParsingService();
        Path cob = tempDir.resolve("sample.cob");
        Files.writeString(cob, "IDENTIFICATION DIVISION. PROGRAM-ID. TEST.");

        Map<String, Object> result = service.parseCobolFile(cob, CobolParsingService.Dialect.GNU);
        assertEquals("GNU", result.get("dialect"));
    }
}

