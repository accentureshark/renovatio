package org.renovatio.provider.java;

import org.openrewrite.ExecutionContext;
import org.openrewrite.Recipe;
import org.openrewrite.Result;
import org.openrewrite.SourceFile;

import java.util.List;

/**
 * @deprecated Kept for backwards compatibility with earlier package names.
 *             Use {@link org.shark.renovatio.provider.java.OpenRewriteRunner} instead.
 */
@Deprecated(forRemoval = false)
public class OpenRewriteRunner {

    private final org.shark.renovatio.provider.java.OpenRewriteRunner delegate =
        new org.shark.renovatio.provider.java.OpenRewriteRunner();

    public List<Result> runRecipe(Recipe recipe, ExecutionContext ctx, List<SourceFile> sourceFiles) {
        return delegate.runRecipe(recipe, ctx, sourceFiles);
    }
}
