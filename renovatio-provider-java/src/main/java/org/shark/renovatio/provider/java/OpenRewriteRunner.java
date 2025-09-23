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
import java.util.Collection;
import java.util.List;
import java.util.ListIterator;
import java.util.Objects;
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

        try {
            if (RUN_WITH_LARGE_SOURCE_SET != null && LARGE_SOURCE_SET_CLASS != null) {
                Object largeSourceSet = createLargeSourceSet(sourceFiles);
                return (List<Result>) RUN_WITH_LARGE_SOURCE_SET.invoke(recipe, largeSourceSet, ctx);
            }

            if (RUN_WITH_ITERABLE != null) {
                return (List<Result>) RUN_WITH_ITERABLE.invoke(recipe, sourceFiles, ctx);
            }
        } catch (ReflectiveOperationException ex) {
            throw new IllegalStateException("Failed to execute OpenRewrite recipe", ex);
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

            if ("edit".equals(name)) {
                // Handle all variations of edit method
                if (actualArgs.length >= 1) {
                    return handleEdit(proxy, actualArgs[0]);
                } else {
                    return proxy; // Return the proxy itself
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

            throw new UnsupportedOperationException("Unsupported LargeSourceSet method: " + method);
        }

        private Object handleEdit(Object proxy, Object unaryOperator) {
            // Handle null case
            if (unaryOperator == null) {
                return proxy;
            }
            
            List<SourceFile> newList = new java.util.ArrayList<>();
            if (unaryOperator instanceof UnaryOperator) {
                @SuppressWarnings("unchecked")
                UnaryOperator<SourceFile> operator = (UnaryOperator<SourceFile>) unaryOperator;
                for (SourceFile current : delegate) {
                    SourceFile updated = operator.apply(current);
                    if (updated != null) {
                        newList.add(updated);
                    }
                }
            } else {
                try {
                    Method applyMethod = unaryOperator.getClass().getMethod("apply", Object.class);
                    for (SourceFile current : delegate) {
                        @SuppressWarnings("unchecked")
                        SourceFile updated = (SourceFile) applyMethod.invoke(unaryOperator, current);
                        if (updated != null) {
                            newList.add(updated);
                        }
                    }
                } catch (ReflectiveOperationException ignored) {
                    throw new UnsupportedOperationException("Unsupported LargeSourceSet#edit argument type: " + unaryOperator);
                }
            }
            // Return a new proxy with the updated list
            return Proxy.newProxyInstance(
                LARGE_SOURCE_SET_CLASS.getClassLoader(),
                new Class<?>[]{LARGE_SOURCE_SET_CLASS},
                new LargeSourceSetInvocationHandler(newList)
            );
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
