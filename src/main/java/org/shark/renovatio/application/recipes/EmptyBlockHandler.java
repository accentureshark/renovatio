package org.shark.renovatio.application.recipes;

import org.springframework.stereotype.Component;

@Component("org.openrewrite.java.cleanup.EmptyBlock")
public class EmptyBlockHandler implements RecipeHandler {
    @Override
    public String apply(String source) {
        return source.replaceAll("\\{\\s*\\}", "");
    }
}
