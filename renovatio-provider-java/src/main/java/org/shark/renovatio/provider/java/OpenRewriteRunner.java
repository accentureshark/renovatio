package org.shark.renovatio.provider.java;

import org.openrewrite.ExecutionContext;
import org.openrewrite.Recipe;
import org.openrewrite.Result;
import org.openrewrite.SourceFile;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
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

        List<Method> candidates = new ArrayList<>();
        for (Method method : Recipe.class.getMethods()) {
            if (!"run".equals(method.getName()) || method.getParameterCount() != 2) {
                continue;
            }
            Class<?>[] parameterTypes = method.getParameterTypes();
            boolean firstIsContext = ExecutionContext.class.isAssignableFrom(parameterTypes[0]);
            boolean secondIsContext = ExecutionContext.class.isAssignableFrom(parameterTypes[1]);
            if (firstIsContext == secondIsContext) {
                continue;
            }
            candidates.add(method);
        }

        candidates.sort(Comparator.comparingInt(method -> {
            Class<?>[] parameterTypes = method.getParameterTypes();
            Class<?> sourceParam = ExecutionContext.class.isAssignableFrom(parameterTypes[0])
                ? parameterTypes[1]
                : parameterTypes[0];
            return requiresSpecializedSourceSet(sourceParam) ? 0 : 1;
        }));

        ReflectiveOperationException lastReflectionFailure = null;
        for (Method method : candidates) {
            Class<?>[] parameterTypes = method.getParameterTypes();
            boolean contextFirst = ExecutionContext.class.isAssignableFrom(parameterTypes[0]);
            Class<?> parameterType = contextFirst ? parameterTypes[1] : parameterTypes[0];
            try {
                Object argument = adaptRunArgument(parameterType, sourceFiles);
                Object recipeRun = contextFirst
                    ? method.invoke(recipe, ctx, argument)
                    : method.invoke(recipe, argument, ctx);
                if (recipeRun instanceof List<?>) {
                    return castResults(recipeRun);
                }
                return extractResultsFromRecipeRun(recipeRun);
            } catch (InvocationTargetException invocationTargetException) {
                throw propagateInvocationException(invocationTargetException);
            } catch (IllegalAccessException illegalAccessException) {
                throw new RuntimeException(illegalAccessException);
            } catch (ReflectiveOperationException reflectionFailure) {
                lastReflectionFailure = reflectionFailure;
            }
        }

        if (lastReflectionFailure != null) {
            throw new RuntimeException(lastReflectionFailure);
        }
        throw new RuntimeException(new NoSuchMethodException("No compatible Recipe.run signature found"));
    }

    @SuppressWarnings("unchecked")
    private List<Result> castResults(Object recipeRun) {
        return (List<Result>) recipeRun;
    }

    private boolean requiresSpecializedSourceSet(Class<?> parameterType) {
        if (parameterType.isArray()
            && parameterType.getComponentType() != null
            && parameterType.getComponentType().isAssignableFrom(SourceFile.class)) {
            return false;
        }
        if (parameterType.isAssignableFrom(List.class)
            || parameterType.isAssignableFrom(Collection.class)
            || parameterType.isAssignableFrom(Iterable.class)) {
            return false;
        }
        return !(List.class.isAssignableFrom(parameterType)
            || Collection.class.isAssignableFrom(parameterType)
            || Iterable.class.isAssignableFrom(parameterType));
    }

    private Object adaptRunArgument(Class<?> parameterType, List<SourceFile> sourceFiles)
        throws ReflectiveOperationException {
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
        return createSpecializedSourceSet(parameterType, sourceFiles);
    }

    private RuntimeException propagateInvocationException(InvocationTargetException ex) {
        Throwable cause = ex.getCause();
        if (cause instanceof RuntimeException runtimeException) {
            return runtimeException;
        }
        return new RuntimeException(cause != null ? cause : ex);
    }

    private Object createSpecializedSourceSet(Class<?> parameterType,
                                              List<SourceFile> sourceFiles)
        throws InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        for (Method method : parameterType.getMethods()) {
            if (Modifier.isStatic(method.getModifiers())
                && parameterType.isAssignableFrom(method.getReturnType())
                && method.getParameterCount() == 1
                && isCollectionCompatible(method.getParameterTypes()[0], sourceFiles)) {
                return method.invoke(null, sourceFiles);
            }
        }

        Method builderMethod = null;
        for (Method candidate : parameterType.getMethods()) {
            if (Modifier.isStatic(candidate.getModifiers())
                && candidate.getParameterCount() == 0
                && candidate.getReturnType() != null
                && candidate.getReturnType().getSimpleName().toLowerCase(Locale.ROOT).contains("builder")) {
                builderMethod = candidate;
                break;
            }
        }
        if (builderMethod == null) {
            throw new NoSuchMethodException("No factory method found to create " + parameterType.getName());
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
                if (isCollectionCompatible(paramType, sourceFiles)) {
                    method.invoke(builder, sourceFiles);
                    return true;
                }
            }
        }
        return false;
    }

    private boolean isCollectionCompatible(Class<?> parameterType, List<SourceFile> sourceFiles) {
        return parameterType.isAssignableFrom(sourceFiles.getClass())
            || parameterType.isAssignableFrom(List.class)
            || parameterType.isAssignableFrom(Collection.class)
            || parameterType.isAssignableFrom(Iterable.class);
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
