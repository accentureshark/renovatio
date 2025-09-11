package org.shark.renovatio.application.recipes;

import org.springframework.stereotype.Component;

/**
 * Simple handler that normalizes whitespace and line breaks to mimic the
 * behaviour of OpenRewrite's {@code AutoFormat} recipe. The component name is
 * set to the fully qualified recipe identifier so that it can be looked up in
 * the handler map.
 */
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
