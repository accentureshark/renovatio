package org.shark.renovatio.provider.java;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.openrewrite.ExecutionContext;
import org.openrewrite.InMemoryExecutionContext;
import org.openrewrite.Recipe;
import org.openrewrite.Result;
import org.openrewrite.SourceFile;
import org.openrewrite.TreeVisitor;
import org.openrewrite.java.JavaParser;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.*;

class OpenRewriteRunnerSafetyTest {

    @TempDir
    Path tempDir;

    private List<SourceFile> parseHelloWorld(ExecutionContext ctx) throws IOException {
        Path src = tempDir.resolve("Hello.java");
        Files.writeString(src, "public class Hello { void hi() {} }", StandardCharsets.UTF_8);
        JavaParser parser = JavaParser.fromJavaVersion().build();
        return parser.parse(ctx, Files.readString(src)).toList();
    }

    @Test
    void blocksCreateEmptyJavaClassWithMissingParams() throws IOException {
        ExecutionContext ctx = new InMemoryExecutionContext(Throwable::printStackTrace);
        List<SourceFile> sources = parseHelloWorld(ctx);
        OpenRewriteRunner runner = new OpenRewriteRunner();

        // Intentionally misconfigured stub mimicking CreateEmptyJavaClass (missing packageName)
        Recipe unsafe = new OptionalBackedCreateEmptyJavaClassStub(Optional.empty(), Optional.of("Demo"));

        IllegalArgumentException ex = assertThrows(IllegalArgumentException.class, () -> {
            runner.runRecipe(unsafe, ctx, sources);
        });
        String msg = ex.getMessage();
        assertNotNull(msg);
        assertTrue(msg.toLowerCase().contains("requires parameters"));
    }

    @Test
    void blocksCompositeRecipeContainingUnsafeChild() throws IOException {
        ExecutionContext ctx = new InMemoryExecutionContext(Throwable::printStackTrace);
        List<SourceFile> sources = parseHelloWorld(ctx);
        OpenRewriteRunner runner = new OpenRewriteRunner();

        Recipe unsafe = new OptionalBackedCreateEmptyJavaClassStub(Optional.empty(), Optional.of("Demo"));
        Recipe composite = new Recipe() {
            @Override
            public String getDisplayName() {
                return "TestComposite";
            }

            @Override
            public String getDescription() {
                return "Composite for testing";
            }

            @Override
            public String getName() {
                return "TestComposite";
            }

            @Override
            public List<Recipe> getRecipeList() {
                return List.of(unsafe);
            }
        };

        RuntimeException ex = assertThrows(RuntimeException.class, () -> {
            runner.runRecipe(composite, ctx, sources);
        });
        String msg = ex.getMessage();
        assertNotNull(msg);
        assertTrue(msg.toLowerCase().contains("requires parameters"));
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
            // Return the canonical name so safety checks match
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
        public TreeVisitor<?, ExecutionContext> getVisitor() {
            return new TreeVisitor<SourceFile, ExecutionContext>() { };
        }
    }
}
