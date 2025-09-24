package org.shark.renovatio.provider.java;

import org.junit.jupiter.api.Test;
import org.openrewrite.ExecutionContext;
import org.openrewrite.Recipe;
import org.openrewrite.TreeVisitor;
import org.openrewrite.tree.Tree;

import java.lang.reflect.Method;

import static org.junit.jupiter.api.Assertions.assertTrue;

class OpenRewriteRunnerSafetyTest {

    @Test
    void detectsMissingParametersForScopedCreateEmptyJavaClass() throws Exception {
        OpenRewriteRunner runner = new OpenRewriteRunner();
        Method method = OpenRewriteRunner.class.getDeclaredMethod("isRecipeMissingRequiredParameters", Recipe.class);
        method.setAccessible(true);

        Recipe scopedRecipe = new ScopedCreateEmptyJavaClassStub();
        boolean missingParameters = (boolean) method.invoke(runner, scopedRecipe);

        assertTrue(missingParameters, "Scoped CreateEmptyJavaClass instances should be flagged as missing required parameters");
    }

    private static final class ScopedCreateEmptyJavaClassStub extends Recipe {

        @Override
        public String getDisplayName() {
            return "Scoped CreateEmptyJavaClass Stub";
        }

        @Override
        public String getDescription() {
            return "Stub used to verify parameter safety checks";
        }

        @Override
        public String getName() {
            return "org.openrewrite.java.CreateEmptyJavaClass$Scoped";
        }

        @Override
        protected TreeVisitor<?, ExecutionContext> getVisitor() {
            return new TreeVisitor<Tree, ExecutionContext>() { };
        }
    }
}
