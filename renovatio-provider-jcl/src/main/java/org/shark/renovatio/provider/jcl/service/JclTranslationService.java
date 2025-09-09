package org.shark.renovatio.provider.jcl.service;

import org.shark.renovatio.provider.jcl.domain.JclStep;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Basic parser and translator for JCL scripts.
 * Extracts EXEC steps and can convert them into shell scripts or
 * simple CI workflow steps (GitHub Actions style).
 */
@Service
public class JclTranslationService {
    private static final Pattern STEP_PATTERN =
            Pattern.compile("^//([^\s]+)\\s+EXEC\\s+PGM=([^\\s,]+)", Pattern.CASE_INSENSITIVE);

    /**
     * Parse the provided JCL content and extract executable steps.
     */
    public List<JclStep> parseSteps(String jclContent) {
        List<JclStep> steps = new ArrayList<>();
        if (jclContent == null) {
            return steps;
        }
        for (String line : jclContent.split("\\R")) {
            Matcher m = STEP_PATTERN.matcher(line.trim());
            if (m.find()) {
                String name = m.group(1);
                String program = m.group(2);
                steps.add(new JclStep(name, program));
            }
        }
        return steps;
    }

    /**
     * Convert JCL steps into a simple shell script.
     */
    public String toShellScript(String jclContent) {
        List<JclStep> steps = parseSteps(jclContent);
        StringBuilder sb = new StringBuilder();
        sb.append("#!/bin/sh\n");
        sb.append("# Generated from JCL\n");
        for (JclStep step : steps) {
            sb.append("# ").append(step.getName()).append('\n');
            sb.append(step.getProgram()).append("\n");
        }
        return sb.toString();
    }

    /**
     * Convert JCL steps into a minimal GitHub Actions workflow.
     */
    public String toGithubActions(String jclContent) {
        List<JclStep> steps = parseSteps(jclContent);
        StringBuilder sb = new StringBuilder();
        sb.append("jobs:\n");
        sb.append("  jcl:\n");
        sb.append("    runs-on: ubuntu-latest\n");
        sb.append("    steps:\n");
        for (JclStep step : steps) {
            sb.append("      - name: ").append(step.getName()).append("\n");
            sb.append("        run: ").append(step.getProgram()).append("\n");
        }
        return sb.toString();
    }
}
