package org.shark.renovatio.application.recipes;

import org.springframework.stereotype.Component;

@Component("org.openrewrite.java.cleanup.ExplicitInitialization")
public class ExplicitInitializationHandler implements RecipeHandler {
    @Override
    public String apply(String source) {
        return source.replaceAll("(\\w+\\s+\\w+)\\s*=\\s*null;", "$1;")
                     .replaceAll("(int\\s+\\w+)\\s*=\\s*0;", "$1;")
                     .replaceAll("(boolean\\s+\\w+)\\s*=\\s*false;", "$1;");
    }
}
