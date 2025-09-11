package org.shark.renovatio.application.recipes;

import org.springframework.stereotype.Component;

@Component("org.openrewrite.java.cleanup.UnnecessaryParentheses")
public class UnnecessaryParenthesesHandler implements RecipeHandler {
    @Override
    public String apply(String source) {
        return source.replaceAll("\\(([a-zA-Z_][a-zA-Z0-9_]*)\\)", "$1");
    }
}
