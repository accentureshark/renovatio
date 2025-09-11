package org.shark.renovatio.application.recipes;

import org.springframework.stereotype.Component;

@Component("org.openrewrite.java.format.AutoFormat")
public class AutoFormatHandler implements RecipeHandler {
    @Override
    public String apply(String source) {
        return source.replaceAll("\\{\\s*\\}", "{ }")
                     .replaceAll("\\s+", " ")
                     .replaceAll(";\\s*", ";\n    ")
                     .replaceAll("\\{\\s*", " {\n    ")
                     .replaceAll("\\}\\s*", "\n}");
    }
}
