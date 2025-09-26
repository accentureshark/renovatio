package org.shark.renovatio.provider.java;

import org.openrewrite.ExecutionContext;
import org.openrewrite.Recipe;
import org.openrewrite.Result;
import org.openrewrite.SourceFile;

import java.lang.reflect.Method;
import java.util.*;

/**
 * OpenRewrite runner with basic safety validations.
 * <p>
 * Note: This class replaces the previous self-delegating compatibility wrapper
 * which caused recursive construction and StackOverflowError.
 */
public class OpenRewriteRunner {

    /**
     * Execute a recipe over the provided source files after performing safety checks.
     * <p>
     * Contract:
     * - Throws IllegalArgumentException if a known-unsafe/misconfigured recipe is detected.
     * - Supports composite recipes by validating children recursively.
     */
    public List<Result> runRecipe(Recipe recipe, ExecutionContext ctx, List<SourceFile> sourceFiles) {
        Objects.requireNonNull(recipe, "recipe");
        Objects.requireNonNull(ctx, "ctx");
        Objects.requireNonNull(sourceFiles, "sourceFiles");

        validateSafeRecipe(recipe);
        // Execute using OpenRewrite 8.21.0 API via LargeSourceSet
        org.openrewrite.LargeSourceSet lss = new org.openrewrite.internal.InMemoryLargeSourceSet(sourceFiles);
        org.openrewrite.RecipeRun run = recipe.run(lss, ctx);
        return run.getChangeset().getAllResults();
    }

    private void validateSafeRecipe(Recipe root) {
        Deque<Recipe> stack = new ArrayDeque<>();
        Deque<Boolean> isRootStack = new ArrayDeque<>();
        stack.push(root);
        isRootStack.push(Boolean.TRUE);
        while (!stack.isEmpty()) {
            Recipe r = stack.pop();
            boolean isRoot = isRootStack.pop();
            String name = safeName(r);
            // Guard for CreateEmptyJavaClass requiring parameters
            if (name.endsWith("org.openrewrite.java.CreateEmptyJavaClass") ||
                    name.equals("org.openrewrite.java.CreateEmptyJavaClass") ||
                    name.endsWith(".CreateEmptyJavaClass")) {
                ensureCreateEmptyJavaClassParams(r, isRoot);
            }
            // Guard for HasMinimumJavaVersion requiring a non-null version
            if (name.endsWith("org.openrewrite.java.search.HasMinimumJavaVersion") ||
                    name.equals("org.openrewrite.java.search.HasMinimumJavaVersion") ||
                    name.endsWith(".HasMinimumJavaVersion")) {
                ensureHasMinimumJavaVersionParams(r, isRoot);
            }
            // Recurse into child recipes if present
            List<Recipe> children = r.getRecipeList();
            if (children != null && !children.isEmpty()) {
                for (Recipe c : children) {
                    if (c != null) {
                        stack.push(c);
                        isRootStack.push(Boolean.FALSE);
                    }
                }
            }
        }
    }

    private String safeName(Recipe r) {
        try {
            String n = r.getName();
            if (n != null && !n.isBlank()) return n;
        } catch (Throwable ignored) {
        }
        try {
            String dn = r.getDisplayName();
            if (dn != null && !dn.isBlank()) return dn;
        } catch (Throwable ignored) {
        }
        return r.getClass().getName();
    }

    private void ensureCreateEmptyJavaClassParams(Recipe r, boolean isRoot) {
        // Try Optional<String> getters first
        Optional<String> pkg = readOptionalString(r, "getPackageName");
        Optional<String> cls = readOptionalString(r, "getClassName");

        boolean pkgPresent = pkg.map(s -> !s.isBlank()).orElse(false);
        boolean clsPresent = cls.map(s -> !s.isBlank()).orElse(false);

        if (!pkgPresent || !clsPresent) {
            // Fallback to plain String getters only if the return type is actually String
            String pkgStr = pkgPresent ? pkg.orElse(null) : readStringIfReturnTypeIsString(r, "getPackageName");
            String clsStr = clsPresent ? cls.orElse(null) : readStringIfReturnTypeIsString(r, "getClassName");
            pkgPresent = pkgPresent || (pkgStr != null && !pkgStr.isBlank());
            clsPresent = clsPresent || (clsStr != null && !clsStr.isBlank());
        }

        if (!pkgPresent || !clsPresent) {
            String msg = "org.openrewrite.java.CreateEmptyJavaClass requires parameters: packageName and className";
            if (isRoot) {
                throw new IllegalArgumentException(msg);
            } else {
                throw new RuntimeException(msg);
            }
        }
    }

    private void ensureHasMinimumJavaVersionParams(Recipe r, boolean isRoot) {
        // Try a few common getter names for the version constraint
        List<String> getters = Arrays.asList("getVersion", "getSinceVersion", "getMinimumVersion", "getRelease");
        String version = null;
        for (String g : getters) {
            Optional<String> vOpt = readOptionalString(r, g);
            if (vOpt.isPresent() && !vOpt.get().isBlank()) {
                version = vOpt.get();
                break;
            }
            String vs = readStringIfReturnTypeIsString(r, g);
            if (vs != null && !vs.isBlank()) {
                version = vs;
                break;
            }
        }
        if (version == null || version.isBlank()) {
            String msg = "org.openrewrite.java.search.HasMinimumJavaVersion requires a non-empty version constraint";
            if (isRoot) {
                throw new IllegalArgumentException(msg);
            } else {
                throw new RuntimeException(msg);
            }
        }
    }

    private Optional<String> readOptionalString(Object target, String getter) {
        try {
            Method m = target.getClass().getMethod(getter);
            if (Optional.class.isAssignableFrom(m.getReturnType())) {
                Object val = m.invoke(target);
                if (val instanceof Optional<?>) {
                    Object inner = ((Optional<?>) val).orElse(null);
                    return Optional.ofNullable(inner == null ? null : inner.toString());
                }
            }
        } catch (ReflectiveOperationException ignored) {
        }
        return Optional.empty();
    }

    private String readStringIfReturnTypeIsString(Object target, String getter) {
        try {
            Method m = target.getClass().getMethod(getter);
            if (m.getReturnType() == String.class) {
                Object val = m.invoke(target);
                return (String) val;
            }
            return null;
        } catch (ReflectiveOperationException ignored) {
            return null;
        }
    }
}
