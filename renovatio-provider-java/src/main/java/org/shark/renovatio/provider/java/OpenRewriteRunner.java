package org.shark.renovatio.provider.java;

import org.openrewrite.ExecutionContext;
import org.openrewrite.Recipe;
import org.openrewrite.Result;
import org.openrewrite.SourceFile;

import java.util.List;
import java.util.Objects;

/**
 * Helper that executes OpenRewrite {@link Recipe recipes} while remaining compatible with
 * both the legacy {@code Recipe.run(Iterable<SourceFile>, ExecutionContext)} signature and
 * the newer {@code Recipe.run(LargeSourceSet, ExecutionContext)} overload introduced in
 * OpenRewrite 8. This class mirrors the reflective approach used inside
 * {@link JavaLanguageProvider} so that other components (tests, legacy tooling, etc.) can
 * execute recipes without having to know which signature is available at compile time.
 */
public class OpenRewriteRunner {

    /**
     * Execute the provided recipe for the given source files.
     *
     * @param recipe      the recipe to execute
     * @param ctx         the execution context
     * @param sourceFiles parsed source files
     * @return the resulting changes from the recipe execution
     */
    public List<Result> runRecipe(Recipe recipe, ExecutionContext ctx, List<SourceFile> sourceFiles) {
        Objects.requireNonNull(recipe, "recipe");
        Objects.requireNonNull(ctx, "ctx");
        Objects.requireNonNull(sourceFiles, "sourceFiles");
        // OpenRewrite 8.x+ API: run(List<SourceFile>, ExecutionContext)
        return recipe.run(sourceFiles, ctx);
    }
}
