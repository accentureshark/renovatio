package org.renovatio.provider.java;

import org.openrewrite.ExecutionContext;
import org.openrewrite.Recipe;
import org.openrewrite.Result;
import org.openrewrite.SourceFile;

import java.util.List;
import java.util.Objects;

/**
 * @deprecated Preserved for backwards compatibility with legacy package names.
 *             Prefer {@link org.shark.renovatio.provider.java.OpenRewriteRunner} and
 *             associated services from the {@code org.shark.renovatio} namespace.
 */
@Deprecated(forRemoval = false)
public class JavaRefactoringService {

    private final org.shark.renovatio.provider.java.OpenRewriteRunner delegate;

    public JavaRefactoringService() {
        this(new org.shark.renovatio.provider.java.OpenRewriteRunner());
    }

    JavaRefactoringService(org.shark.renovatio.provider.java.OpenRewriteRunner delegate) {
        this.delegate = Objects.requireNonNull(delegate, "delegate");
    }

    public List<Result> runRecipe(Recipe recipe, List<SourceFile> sourceFiles, ExecutionContext ctx) {
        return delegate.runRecipe(recipe, ctx, sourceFiles);
    }
}
