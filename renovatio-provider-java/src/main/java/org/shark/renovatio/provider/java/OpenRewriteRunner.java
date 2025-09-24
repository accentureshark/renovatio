package org.shark.renovatio.provider.java;

import org.openrewrite.ExecutionContext;
import org.openrewrite.Recipe;
import org.openrewrite.Result;
import org.openrewrite.SourceFile;

import java.lang.invoke.MethodHandle;
import java.lang.invoke.MethodHandles;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.lang.reflect.Proxy;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.ListIterator;
import java.util.Objects;
import java.util.Set;
import java.util.function.UnaryOperator;

/**
 * Helper that executes OpenRewrite {@link Recipe recipes} while remaining compatible with
 * both the legacy {@code Recipe.run(Iterable<SourceFile>, ExecutionContext)} signature and
 * the newer {@code Recipe.run(LargeSourceSet, ExecutionContext)} overload introduced in
 * OpenRewrite 8. This class mirrors the reflective approach used inside
 * {@link JavaLanguageProvider} so that other components (tests, legacy tooling, etc.) can
 * execute recipes without having to know which signature is available at compile time.
 */
public class OpenRewriteRunner {

    private static final Class<?> LARGE_SOURCE_SET_CLASS;
    private static final Method RUN_WITH_LARGE_SOURCE_SET;
    private static final Method RUN_WITH_ITERABLE;
    private static final Constructor<?> LARGE_SOURCE_SET_ITERABLE_CTOR;
    private static final Method LARGE_SOURCE_SET_FACTORY_FROM_ITERABLE;

    static {
        Method runIterable;
        try {
            runIterable = Recipe.class.getMethod("run", Iterable.class, ExecutionContext.class);
        } catch (NoSuchMethodException ex) {
            runIterable = null;
        }
        RUN_WITH_ITERABLE = runIterable;

        Class<?> largeSourceSetClass = null;
        Method runWithLargeSourceSet = null;
        Constructor<?> iterableCtor = null;
        Method factoryFromIterable = null;

        try {
            largeSourceSetClass = Class.forName("org.openrewrite.LargeSourceSet");
            runWithLargeSourceSet = Recipe.class.getMethod("run", largeSourceSetClass, ExecutionContext.class);

            try {
                Class<?> inMemory = Class.forName("org.openrewrite.InMemoryLargeSourceSet");
                for (Constructor<?> ctor : inMemory.getConstructors()) {
                    Class<?>[] parameterTypes = ctor.getParameterTypes();
                    if (parameterTypes.length == 1 && Iterable.class.isAssignableFrom(parameterTypes[0])) {
                        iterableCtor = ctor;
                        break;
                    }
                }

                if (iterableCtor == null) {
                    for (Method method : inMemory.getMethods()) {
                        if (Modifier.isStatic(method.getModifiers())
                            && method.getParameterCount() == 1
                            && Iterable.class.isAssignableFrom(method.getParameterTypes()[0])
                            && largeSourceSetClass.isAssignableFrom(method.getReturnType())) {
                            factoryFromIterable = method;
                            break;
                        }
                    }
                }
            } catch (ClassNotFoundException ignored) {
                // Older OpenRewrite versions did not expose the helper implementation.
            }
        } catch (ClassNotFoundException | NoSuchMethodException ex) {
            largeSourceSetClass = null;
            runWithLargeSourceSet = null;
            iterableCtor = null;
            factoryFromIterable = null;
        }

        LARGE_SOURCE_SET_CLASS = largeSourceSetClass;
        RUN_WITH_LARGE_SOURCE_SET = runWithLargeSourceSet;
        LARGE_SOURCE_SET_ITERABLE_CTOR = iterableCtor;
        LARGE_SOURCE_SET_FACTORY_FROM_ITERABLE = factoryFromIterable;
    }

    /**
     * Checks if the recipe is known to require parameters and is not configured.
     * Returns true if the recipe should be filtered out to avoid runtime errors.
     */
    private boolean isRecipeMissingRequiredParameters(Recipe recipe) {
        if (recipe == null) {
            return false;
        }
        Set<Recipe> visited = Collections.newSetFromMap(new IdentityHashMap<>());
        return isRecipeMissingRequiredParameters(recipe, visited);
    }

    private boolean isRecipeMissingRequiredParameters(Recipe recipe, Set<Recipe> visited) {
        if (recipe == null || !visited.add(recipe)) {
            return false;
        }

        if (isSingleRecipeMissingRequiredParameters(recipe)) {
            return true;
        }

        try {
            Collection<Recipe> nested = recipe.getRecipeList();
            if (nested != null) {
                for (Recipe nestedRecipe : nested) {
                    if (isRecipeMissingRequiredParameters(nestedRecipe, visited)) {
                        return true;
                    }
                }
            }
        } catch (UnsupportedOperationException ignored) {
            // Some Recipe implementations may not support getRecipeList; ignore and continue.
        } catch (Exception ex) {
            // If we cannot inspect nested recipes, be conservative and block execution.
            return true;
        }

        return false;
    }

    private boolean isSingleRecipeMissingRequiredParameters(Recipe recipe) {
        String recipeName = safeRecipeName(recipe);

        if (matchesRecipe(recipe, recipeName, "org.openrewrite.java.CreateEmptyJavaClass")) {
            try {
                Object packageName = recipe.getClass().getMethod("getPackageName").invoke(recipe);
                Object className = recipe.getClass().getMethod("getClassName").invoke(recipe);
                return packageName == null || String.valueOf(packageName).isEmpty() ||
                    className == null || String.valueOf(className).isEmpty();
            } catch (Exception e) {
                return true; // If we can't check, assume it's missing
            }
        }

        if (matchesRecipe(recipe, recipeName, "org.openrewrite.yaml.CreateYamlFile")) {
            try {
                Method getPath = recipe.getClass().getMethod("getPath");
                Object path = getPath.invoke(recipe);
                if (path == null || String.valueOf(path).isEmpty()) {
                    return true;
                }
            } catch (Exception e) {
                String asString = recipe.toString();
                if (asString.contains("path=null") || asString.contains("path=''")) {
                    return true;
                }
                return true;
            }
        }

        if (matchesRecipe(recipe, recipeName, "org.openrewrite.text.CreateTextFile") ||
            matchesRecipe(recipe, recipeName, "org.openrewrite.xml.CreateXmlFile")) {
            try {
                Method getPath = recipe.getClass().getMethod("getPath");
                Object path = getPath.invoke(recipe);
                return path == null || String.valueOf(path).isEmpty();
            } catch (Exception e) {
                return true;
            }
        }

        return false;
    }

    private String safeRecipeName(Recipe recipe) {
        try {
            String name = recipe.getName();
            if (name != null && !name.isBlank()) {
                return name;
            }
        } catch (Exception ignored) {
            // Older versions may not expose getName; fall back to class name.
        }
        return recipe.getClass().getName();
    }

    private boolean matchesRecipe(Recipe recipe, String recipeName, String targetName) {
        if (targetName.equals(recipe.getClass().getName())) {
            return true;
        }
        return recipeName != null && targetName.equals(recipeName);
    }

    /**
     * Execute the provided recipe for the given source files.
     *
     * @param recipe      the recipe to execute
     * @param ctx         the execution context
     * @param sourceFiles parsed source files
     * @return the resulting changes from the recipe execution
     */
    @SuppressWarnings("unchecked")
    public List<Result> runRecipe(Recipe recipe, ExecutionContext ctx, List<SourceFile> sourceFiles) {
        Objects.requireNonNull(recipe, "recipe");
        Objects.requireNonNull(ctx, "ctx");
        Objects.requireNonNull(sourceFiles, "sourceFiles");

        // === FILTER recipes with missing required parameters ===
        if (isRecipeMissingRequiredParameters(recipe)) {
            throw new IllegalStateException("OpenRewrite recipe '" + recipe.getClass().getName() + "' requires parameters that are not set. " +
                "Please configure all required parameters before execution, or use a different recipe.");
        }

        try {
            if (RUN_WITH_LARGE_SOURCE_SET != null && LARGE_SOURCE_SET_CLASS != null) {
                Object largeSourceSet = createLargeSourceSet(sourceFiles);
                Object result = RUN_WITH_LARGE_SOURCE_SET.invoke(recipe, largeSourceSet, ctx);
                
                // Handle different return types from different OpenRewrite versions
                if (result instanceof List) {
                    return (List<Result>) result;
                } else {
                    // Try to extract results from RecipeRun or similar objects
                    try {
                        java.lang.reflect.Method getResultsMethod = result.getClass().getMethod("getResults");
                        Object results = getResultsMethod.invoke(result);
                        if (results instanceof List) {
                            return (List<Result>) results;
                        }
                    } catch (Exception e) {
                        // Try other common method names
                        try {
                            java.lang.reflect.Method getChangesMethod = result.getClass().getMethod("getChangelist");
                            Object changes = getChangesMethod.invoke(result);
                            if (changes instanceof List) {
                                return (List<Result>) changes;
                            }
                        } catch (Exception e2) {
                            // If we can't extract results, return empty list
                            return new ArrayList<>();
                        }
                    }
                }
            }

            if (RUN_WITH_ITERABLE != null) {
                Object result = RUN_WITH_ITERABLE.invoke(recipe, sourceFiles, ctx);
                if (result instanceof List) {
                    return (List<Result>) result;
                } else {
                    // Handle RecipeRun for legacy API as well
                    try {
                        java.lang.reflect.Method getResultsMethod = result.getClass().getMethod("getResults");
                        Object results = getResultsMethod.invoke(result);
                        if (results instanceof List) {
                            return (List<Result>) results;
                        }
                    } catch (Exception e) {
                        return new ArrayList<>();
                    }
                }
            }
        } catch (ReflectiveOperationException ex) {
            // Enhanced error handling - check for known problematic recipes
            String errorMessage = ex.getMessage();
            Throwable cause = ex.getCause();
            
            // Check the full exception chain for NPE related to recipe parameters
            Throwable current = ex;
            while (current != null) {
                String currentMessage = current.getMessage();
                if (currentMessage != null) {
                    if (currentMessage.contains("packageName") || 
                        currentMessage.contains("Cannot invoke \"String.replace")) {
                        throw new IllegalStateException("Failed to execute OpenRewrite recipe - Recipe requires specific configuration parameters that are not set. " +
                            "Recipes like CreateEmptyJavaClass need parameters such as 'packageName'. " +
                            "These recipes have been filtered out in newer versions to prevent this error. " +
                            "Consider using safe alternatives like: 'org.openrewrite.java.format.AutoFormat', " +
                            "'org.openrewrite.java.cleanup.UnnecessaryParentheses', or browse recipes with the 'java_recipe_list' tool. " +
                            "Original error: " + currentMessage, ex);
                    }
                    
                    if (currentMessage.contains("className") || 
                        currentMessage.contains("methodName") ||
                        currentMessage.contains("fieldName") ||
                        currentMessage.contains("type") && current instanceof NullPointerException) {
                        throw new IllegalStateException("Failed to execute OpenRewrite recipe - Recipe requires specific configuration parameters that are not set. " +
                            "These types of recipes have been filtered out in newer versions to prevent this error. " +
                            "Please use the 'java_recipe_list' tool to find safe recipes, or browse by profile using 'quality', 'modernize', 'security', etc. " +
                            "Original error: " + currentMessage, ex);
                    }
                }
                current = current.getCause();
            }
            
            if (errorMessage != null && (errorMessage.contains("packageName") || errorMessage.contains("Cannot invoke \"String.replace"))) {
                throw new IllegalStateException("Failed to execute OpenRewrite recipe - likely due to misconfigured recipe parameters. " +
                    "Some recipes require specific configuration (e.g., packageName for CreateEmptyJavaClass). " +
                    "Original error: " + errorMessage, ex);
            }
            throw new IllegalStateException("Failed to execute OpenRewrite recipe", ex);
        } catch (Exception ex) {
            // Catch any other exceptions that might occur during recipe execution
            String errorMessage = ex.getMessage();
            Throwable cause = ex.getCause();
            
            // Check for NullPointerException that might be related to missing parameters
            if (ex instanceof NullPointerException || 
                (cause instanceof NullPointerException)) {
                throw new IllegalStateException("Recipe execution failed due to missing required parameters. " +
                    "This typically happens with recipes that require specific configuration like packageName, className, etc. " +
                    "Consider using recipes that don't require additional parameters or provide the necessary configuration. " +
                    "Original error: " + (errorMessage != null ? errorMessage : ex.getClass().getSimpleName()), ex);
            }
            
            if (errorMessage != null && errorMessage.contains("packageName")) {
                throw new IllegalStateException("Recipe execution failed due to missing required parameters. " +
                    "This typically happens with recipes like CreateEmptyJavaClass that require packageName configuration. " +
                    "Original error: " + errorMessage, ex);
            }
            throw new IllegalStateException("Unexpected error during recipe execution: " + errorMessage, ex);
        }

        throw new IllegalStateException("No compatible OpenRewrite Recipe#run overload found");
    }

    private Object createLargeSourceSet(List<SourceFile> sourceFiles) throws ReflectiveOperationException {
        if (LARGE_SOURCE_SET_CLASS == null) {
            throw new IllegalStateException("LargeSourceSet class is not available on the classpath");
        }

        if (LARGE_SOURCE_SET_ITERABLE_CTOR != null) {
            return LARGE_SOURCE_SET_ITERABLE_CTOR.newInstance(sourceFiles);
        }

        if (LARGE_SOURCE_SET_FACTORY_FROM_ITERABLE != null) {
            return LARGE_SOURCE_SET_FACTORY_FROM_ITERABLE.invoke(null, sourceFiles);
        }

        return Proxy.newProxyInstance(
            LARGE_SOURCE_SET_CLASS.getClassLoader(),
            new Class<?>[]{LARGE_SOURCE_SET_CLASS},
            new LargeSourceSetInvocationHandler(sourceFiles)
        );
    }

    private static final class LargeSourceSetInvocationHandler implements InvocationHandler {
        private final List<SourceFile> delegate;

        private LargeSourceSetInvocationHandler(List<SourceFile> delegate) {
            this.delegate = delegate;
        }

        @Override
        public Object invoke(Object proxy, Method method, Object[] args) throws Throwable {
            String name = method.getName();
            Object[] actualArgs = args == null ? new Object[0] : args;

            if (method.isDefault()) {
                return invokeDefault(proxy, method, actualArgs);
            }

            // Handle the edit method early to avoid conflicts
            if ("edit".equals(name)) {
                // Handle all variations of edit method
                if (actualArgs.length >= 1) {
                    return handleEdit(proxy, actualArgs[0]);
                } else {
                    return proxy; // Return the proxy itself for parameterless edit calls
                }
            }

            switch (name) {
                case "iterator":
                    return delegate.iterator();
                case "spliterator":
                    return delegate.spliterator();
                case "stream":
                    return delegate.stream();
                case "isEmpty":
                    return delegate.isEmpty();
                case "size":
                    return delegate.size();
                default:
                    break;
            }

            Object invocationResult = tryInvokeOn(Collection.class, name, method.getParameterTypes(), actualArgs);
            if (invocationResult != InvocationResult.NOT_HANDLED) {
                return invocationResult;
            }

            invocationResult = tryInvokeOn(List.class, name, method.getParameterTypes(), actualArgs);
            if (invocationResult != InvocationResult.NOT_HANDLED) {
                return invocationResult;
            }

            invocationResult = tryInvokeOn(Iterable.class, name, method.getParameterTypes(), actualArgs);
            if (invocationResult != InvocationResult.NOT_HANDLED) {
                return invocationResult;
            }

            if ("getSourceFiles".equals(name) && method.getParameterCount() == 0) {
                return delegate;
            }

            if ("getChangeset".equals(name) && method.getParameterCount() == 0) {
                // Return an empty changeset - we'll create a simple implementation
                try {
                    // Try to create a Changeset object
                    Class<?> changesetClass = Class.forName("org.openrewrite.Changeset");
                    // Look for a static empty or create method
                    try {
                        java.lang.reflect.Method emptyMethod = changesetClass.getMethod("empty");
                        return emptyMethod.invoke(null);
                    } catch (NoSuchMethodException e) {
                        // Try constructor or factory methods
                        return changesetClass.getDeclaredConstructor().newInstance();
                    }
                } catch (Exception e) {
                    // If we can't create a proper Changeset, return null (might be acceptable)
                    return null;
                }
            }

            if ("generate".equals(name)) {
                // Handle generate method - typically adds new source files
                if (actualArgs.length >= 1 && actualArgs[0] instanceof java.util.Collection) {
                    @SuppressWarnings("unchecked")
                    java.util.Collection<SourceFile> newSources = (java.util.Collection<SourceFile>) actualArgs[0];
                    List<SourceFile> combinedList = new java.util.ArrayList<>(delegate);
                    combinedList.addAll(newSources);
                    return Proxy.newProxyInstance(
                        LARGE_SOURCE_SET_CLASS.getClassLoader(),
                        new Class<?>[]{LARGE_SOURCE_SET_CLASS},
                        new LargeSourceSetInvocationHandler(combinedList)
                    );
                } else {
                    // Return the proxy itself if no valid collection is provided
                    return proxy;
                }
            }

            if ("setRecipe".equals(name)) {
                // Handle setRecipe method - this is called during recipe execution
                // We don't need to actually store the recipe list, just return successfully
                return null; // setRecipe returns void
            }

            if ("toString".equals(name) && method.getParameterCount() == 0) {
                return "LargeSourceSet" + delegate;
            }

            if ("hashCode".equals(name) && method.getParameterCount() == 0) {
                return System.identityHashCode(proxy);
            }

            if ("equals".equals(name) && method.getParameterCount() == 1) {
                return proxy == actualArgs[0];
            }

            // Handle other common methods that might be called
            if ("clone".equals(name) && method.getParameterCount() == 0) {
                return Proxy.newProxyInstance(
                    LARGE_SOURCE_SET_CLASS.getClassLoader(),
                    new Class<?>[]{LARGE_SOURCE_SET_CLASS},
                    new LargeSourceSetInvocationHandler(new java.util.ArrayList<>(delegate))
                );
            }

            // Try to handle methods that return the same type (fluent methods)
            if (method.getReturnType().equals(LARGE_SOURCE_SET_CLASS)) {
                // For methods that return LargeSourceSet, return the proxy unchanged
                // This handles fluent-style methods that aren't critical for recipe execution
                return proxy;
            }

            // Log the unsupported method for debugging
            System.err.println("Unsupported LargeSourceSet method called: " + method + " with args: " + java.util.Arrays.toString(actualArgs));
            throw new UnsupportedOperationException("Unsupported LargeSourceSet method: " + method);
        }

        private Object handleEdit(Object proxy, Object unaryOperator) {
            // Handle null case
            if (unaryOperator == null) {
                return proxy;
            }
            
            try {
                List<SourceFile> newList = new java.util.ArrayList<>();
                if (unaryOperator instanceof UnaryOperator) {
                    @SuppressWarnings("unchecked")
                    UnaryOperator<SourceFile> operator = (UnaryOperator<SourceFile>) unaryOperator;
                    for (SourceFile current : delegate) {
                        try {
                            SourceFile updated = operator.apply(current);
                            if (updated != null) {
                                newList.add(updated);
                            }
                        } catch (Exception e) {
                            // If transformation fails, keep the original
                            newList.add(current);
                        }
                    }
                } else {
                    // Try reflection-based approach for other types of operators
                    try {
                        Method applyMethod = unaryOperator.getClass().getMethod("apply", Object.class);
                        for (SourceFile current : delegate) {
                            try {
                                @SuppressWarnings("unchecked")
                                SourceFile updated = (SourceFile) applyMethod.invoke(unaryOperator, current);
                                if (updated != null) {
                                    newList.add(updated);
                                } else {
                                    newList.add(current);
                                }
                            } catch (Exception e) {
                                // If transformation fails, keep the original
                                newList.add(current);
                            }
                        }
                    } catch (NoSuchMethodException e) {
                        // If we can't find apply method, return the original proxy unchanged
                        return proxy;
                    }
                }
                
                // Return a new proxy with the updated list
                return Proxy.newProxyInstance(
                    LARGE_SOURCE_SET_CLASS.getClassLoader(),
                    new Class<?>[]{LARGE_SOURCE_SET_CLASS},
                    new LargeSourceSetInvocationHandler(newList)
                );
            } catch (Exception e) {
                // If anything goes wrong, return the original proxy
                return proxy;
            }
        }

        private Object tryInvokeOn(Class<?> targetClass, String methodName, Class<?>[] parameterTypes, Object[] args) throws Throwable {
            try {
                Method target = targetClass.getMethod(methodName, parameterTypes);
                return target.invoke(delegate, args);
            } catch (NoSuchMethodException ignored) {
                return InvocationResult.NOT_HANDLED;
            }
        }

        private Object invokeDefault(Object proxy, Method method, Object[] args) throws Throwable {
            MethodHandles.Lookup lookup = MethodHandles.lookup();
            MethodHandle handle = MethodHandles.privateLookupIn(method.getDeclaringClass(), lookup)
                .unreflectSpecial(method, method.getDeclaringClass())
                .bindTo(proxy);
            return handle.invokeWithArguments(args);
        }
    }

    private enum InvocationResult {
        NOT_HANDLED
    }
}
