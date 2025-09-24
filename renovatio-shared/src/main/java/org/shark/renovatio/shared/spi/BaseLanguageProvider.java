package org.shark.renovatio.shared.spi;

/**
 * Base class for language providers offering common utility methods.
 */
public abstract class BaseLanguageProvider implements LanguageProvider, ExtendedLanguageProvider {

    /**
     * Generate a run identifier using the provider's language prefix.
     *
     * @return run identifier unique to this execution
     */
    protected String generateRunId() {
        return language() + "-run-" + System.currentTimeMillis();
    }

    /**
     * Generate a plan identifier using the provider's language prefix.
     *
     * @return plan identifier unique to this plan creation
     */
    protected String generatePlanId() {
        return language() + "-plan-" + System.currentTimeMillis();
    }

    /**
     * Create a sample unified diff. Concrete implementations may override
     * this method to provide language specific examples.
     *
     * @return a simple unified diff string
     */
    protected String createSampleDiff() {
        return """
            --- a/example.txt
            +++ b/example.txt
            @@ -1 +1 @@
            -old line
            +new line
            """;
    }
}
