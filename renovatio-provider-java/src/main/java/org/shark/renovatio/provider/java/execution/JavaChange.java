package org.shark.renovatio.provider.java.execution;

/**
 * Represents a single change detected or produced by OpenRewrite.
 */
public record JavaChange(String file, String diff) {
}
