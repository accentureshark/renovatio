package org.shark.renovatio.provider.java;

import org.openrewrite.ExecutionContext;
import org.openrewrite.Recipe;
import org.openrewrite.Result;
import org.openrewrite.SourceFile;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Locale;
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

        try {
            return runRecipeWithLargeSourceSet(recipe, ctx, sourceFiles);
        } catch (ClassNotFoundException | NoSuchMethodException e) {
            try {
                return runRecipeWithLegacyIterable(recipe, ctx, sourceFiles);
            } catch (InvocationTargetException legacyInvocation) {
                throw propagateInvocationException(legacyInvocation);
            } catch (IllegalAccessException | NoSuchMethodException reflectionFailure) {
                throw new RuntimeException(reflectionFailure);
            }
        } catch (InvocationTargetException ex) {
            throw propagateInvocationException(ex);
        } catch (IllegalAccessException ex) {
            throw new RuntimeException(ex);
        }
    }

    @SuppressWarnings("unchecked")
    private List<Result> runRecipeWithLargeSourceSet(Recipe recipe,
                                                     ExecutionContext ctx,
                                                     List<SourceFile> sourceFiles)
        throws ClassNotFoundException, NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        Class<?> largeSourceSetClass = Class.forName("org.openrewrite.LargeSourceSet");
        Object largeSourceSet = createLargeSourceSet(largeSourceSetClass, sourceFiles);
        Method runMethod = Recipe.class.getMethod("run", largeSourceSetClass, ExecutionContext.class);
        Object recipeRun = runMethod.invoke(recipe, largeSourceSet, ctx);
        return extractResultsFromRecipeRun(recipeRun);
    }

    @SuppressWarnings("unchecked")
    private List<Result> runRecipeWithLegacyIterable(Recipe recipe,
                                                     ExecutionContext ctx,
                                                     List<SourceFile> sourceFiles)
        throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        Method runMethod = findLegacyRunMethod(sourceFiles);
        Object argument = adaptLegacyArgument(runMethod.getParameterTypes()[0], sourceFiles);
        Object recipeRun = runMethod.invoke(recipe, argument, ctx);
        if (recipeRun instanceof List<?>) {
            return (List<Result>) recipeRun;
        }
        return extractResultsFromRecipeRun(recipeRun);
    }

    private Method findLegacyRunMethod(List<SourceFile> sourceFiles) throws NoSuchMethodException {
        for (Method method : Recipe.class.getMethods()) {
            if (!"run".equals(method.getName()) || method.getParameterCount() != 2) {
                continue;
            }
            Class<?>[] parameterTypes = method.getParameterTypes();
            if (!ExecutionContext.class.isAssignableFrom(parameterTypes[1])) {
                continue;
            }
            Class<?> firstParam = parameterTypes[0];
            if (isLegacyIterableCompatible(firstParam, sourceFiles)) {
                return method;
            }
        }
        throw new NoSuchMethodException("No compatible Recipe.run signature for Iterable SourceFile inputs");
    }

    private boolean isLegacyIterableCompatible(Class<?> parameterType, List<SourceFile> sourceFiles) {
        return parameterType.isAssignableFrom(sourceFiles.getClass())
            || parameterType.isAssignableFrom(List.class)
            || parameterType.isAssignableFrom(Collection.class)
            || parameterType.isAssignableFrom(Iterable.class)
            || (parameterType.isArray()
                && parameterType.getComponentType() != null
                && parameterType.getComponentType().isAssignableFrom(SourceFile.class));
    }

    private Object adaptLegacyArgument(Class<?> parameterType, List<SourceFile> sourceFiles) {
        if (parameterType.isAssignableFrom(sourceFiles.getClass())
            || parameterType.isAssignableFrom(List.class)
            || parameterType.isAssignableFrom(Collection.class)
            || parameterType.isAssignableFrom(Iterable.class)) {
            return sourceFiles;
        }
        if (parameterType.isArray()
            && parameterType.getComponentType() != null
            && parameterType.getComponentType().isAssignableFrom(SourceFile.class)) {
            return sourceFiles.toArray(new SourceFile[0]);
        }
        throw new IllegalArgumentException("Unsupported Recipe.run parameter type: " + parameterType.getName());
    }

    private RuntimeException propagateInvocationException(InvocationTargetException ex) {
        Throwable cause = ex.getCause();
        if (cause instanceof RuntimeException runtimeException) {
            return runtimeException;
        }
        return new RuntimeException(cause != null ? cause : ex);
    }

    private Object createLargeSourceSet(Class<?> largeSourceSetClass,
                                        List<SourceFile> sourceFiles)
        throws InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        for (Method method : largeSourceSetClass.getMethods()) {
            if (Modifier.isStatic(method.getModifiers())
                && largeSourceSetClass.isAssignableFrom(method.getReturnType())
                && method.getParameterCount() == 1
                && method.getParameterTypes()[0].isAssignableFrom(sourceFiles.getClass())) {
                return method.invoke(null, sourceFiles);
            }
        }

        Method builderMethod = null;
        for (Method candidate : largeSourceSetClass.getMethods()) {
            if (Modifier.isStatic(candidate.getModifiers())
                && candidate.getParameterCount() == 0
                && candidate.getReturnType() != null
                && candidate.getReturnType().getSimpleName().toLowerCase(Locale.ROOT).contains("builder")) {
                builderMethod = candidate;
                break;
            }
        }
        if (builderMethod == null) {
            throw new NoSuchMethodException("No factory method found to create LargeSourceSet");
        }

        Object builder = builderMethod.invoke(null);
        boolean added = tryInvokeBuilderAddAll(builder, sourceFiles);
        if (!added) {
            Method addMethod = builder.getClass().getMethod("add", SourceFile.class);
            for (SourceFile sourceFile : sourceFiles) {
                addMethod.invoke(builder, sourceFile);
            }
        }
        Method buildMethod = builder.getClass().getMethod("build");
        return buildMethod.invoke(builder);
    }

    private boolean tryInvokeBuilderAddAll(Object builder, List<SourceFile> sourceFiles)
        throws InvocationTargetException, IllegalAccessException {
        for (Method method : builder.getClass().getMethods()) {
            if ("addAll".equals(method.getName()) && method.getParameterCount() == 1) {
                Class<?> paramType = method.getParameterTypes()[0];
                if (paramType.isAssignableFrom(sourceFiles.getClass())
                    || paramType.isAssignableFrom(List.class)
                    || paramType.isAssignableFrom(Collection.class)
                    || paramType.isAssignableFrom(Iterable.class)) {
                    method.invoke(builder, sourceFiles);
                    return true;
                }
            }
        }
        return false;
    }

    @SuppressWarnings("unchecked")
    private List<Result> extractResultsFromRecipeRun(Object recipeRun)
        throws InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        if (recipeRun == null) {
            return Collections.emptyList();
        }

        Class<?> runClass = recipeRun.getClass();
        try {
            Method getResults = runClass.getMethod("getResults");
            Object value = getResults.invoke(recipeRun);
            if (value instanceof List<?>) {
                return (List<Result>) value;
            }
        } catch (NoSuchMethodException ignored) {
            // Continue to alternate access pattern
        }

        try {
            Method getChangeset = runClass.getMethod("getChangeset");
            Object changeSet = getChangeset.invoke(recipeRun);
            if (changeSet != null) {
                Method getResults = changeSet.getClass().getMethod("getResults");
                Object value = getResults.invoke(changeSet);
                if (value instanceof List<?>) {
                    return (List<Result>) value;
                }
            }
        } catch (NoSuchMethodException ignored) {
            // No alternate representation available
        }

        throw new NoSuchMethodException("Unable to extract results from RecipeRun type: " + runClass.getName());
    }
}
