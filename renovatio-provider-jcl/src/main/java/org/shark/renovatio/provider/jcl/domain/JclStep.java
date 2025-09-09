package org.shark.renovatio.provider.jcl.domain;

/**
 * Representation of a single JCL execution step.
 */
public class JclStep {
    private final String name;
    private final String program;

    public JclStep(String name, String program) {
        this.name = name;
        this.program = program;
    }

    public String getName() {
        return name;
    }

    public String getProgram() {
        return program;
    }
}
