package org.shark.renovatio.provider.cobol.service;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Verifies that CICS commands are detected during COBOL analysis.
 */
class CobolParsingServiceCicsTest {

    @TempDir
    Path tempDir;

    @Test
    void detectsCicsCommands() throws Exception {
        CobolParsingService service = new CobolParsingService();
        Path cob = tempDir.resolve("sample.cob");
        String source = String.join("\n",
                "IDENTIFICATION DIVISION.",
                "PROGRAM-ID. TEST.",
                "PROCEDURE DIVISION.",
                "    EXEC CICS SEND TEXT('HI') END-EXEC.",
                "    STOP RUN.");
        Files.writeString(cob, source);

        Map<String, Object> result = service.parseCobolFile(cob);
        @SuppressWarnings("unchecked")
        var cmds = (java.util.Set<String>) result.get("cicsCommands");
        assertTrue(cmds.contains("SEND"));
    }
}

