package org.shark.renovatio.provider.java;

import org.junit.jupiter.api.Test;
import org.openrewrite.ExecutionContext;
import org.openrewrite.Recipe;
import org.openrewrite.TreeVisitor;
import org.openrewrite.tree.Tree;

import java.lang.reflect.Method;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertFalse;

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

    @Test
    void detectsOptionalParametersWhenEmpty() throws Exception {
        OpenRewriteRunner runner = new OpenRewriteRunner();
        Method method = OpenRewriteRunner.class.getDeclaredMethod("isRecipeMissingRequiredParameters", Recipe.class);
        method.setAccessible(true);

        Recipe optionalRecipe = new OptionalBackedCreateEmptyJavaClassStub(Optional.empty(), Optional.of("Demo"));
        boolean missingParameters = (boolean) method.invoke(runner, optionalRecipe);

        assertTrue(missingParameters, "Recipes exposing Optional.empty() should be treated as missing required parameters");
    }

    @Test
    void acceptsConfiguredOptionalParameters() throws Exception {
        OpenRewriteRunner runner = new OpenRewriteRunner();
        Method method = OpenRewriteRunner.class.getDeclaredMethod("isRecipeMissingRequiredParameters", Recipe.class);
        method.setAccessible(true);

        Recipe optionalRecipe = new OptionalBackedCreateEmptyJavaClassStub(Optional.of("com.example"), Optional.of("Demo"));
        boolean missingParameters = (boolean) method.invoke(runner, optionalRecipe);

        assertFalse(missingParameters, "Configured Optional parameters should not be flagged as missing");
    }

    @Test
    void detectsMissingPathForAppendToTextFile() throws Exception {
        OpenRewriteRunner runner = new OpenRewriteRunner();
        Method method = OpenRewriteRunner.class.getDeclaredMethod("isRecipeMissingRequiredParameters", Recipe.class);
        method.setAccessible(true);

        Recipe appendRecipe = new AppendToTextFileStub();
        boolean missingParameters = (boolean) method.invoke(runner, appendRecipe);

        assertTrue(missingParameters, "AppendToTextFile without a path should be treated as unsafe");
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

    private static final class OptionalBackedCreateEmptyJavaClassStub extends Recipe {
        private final Optional<String> packageName;
        private final Optional<String> className;

        private OptionalBackedCreateEmptyJavaClassStub(Optional<String> packageName, Optional<String> className) {
            this.packageName = packageName;
            this.className = className;
        }

        @Override
        public String getDisplayName() {
            return "Optional CreateEmptyJavaClass Stub";
        }

        @Override
        public String getDescription() {
            return "Stub representing CreateEmptyJavaClass with Optional parameters";
        }

        @Override
        public String getName() {
            return "org.openrewrite.java.CreateEmptyJavaClass";
        }

        @SuppressWarnings("unused")
        public Optional<String> getPackageName() {
            return packageName;
        }

        @SuppressWarnings("unused")
        public Optional<String> getClassName() {
            return className;
        }

        @Override
        protected TreeVisitor<?, ExecutionContext> getVisitor() {
            return new TreeVisitor<Tree, ExecutionContext>() { };
        }
    }

    private static final class AppendToTextFileStub extends Recipe {

        @Override
        public String getName() {
            return "org.openrewrite.text.AppendToTextFile";
        }

        @Override
        public String getDisplayName() {
            return "AppendToTextFile Stub";
        }

        @Override
        public String getDescription() {
            return "Stub used to verify path safety checks";
        }

        @Override
        public String toString() {
            return "AppendToTextFile{path=null}";
        }

        @Override
        protected TreeVisitor<?, ExecutionContext> getVisitor() {
            return new TreeVisitor<Tree, ExecutionContext>() { };
        }
    }
}
