package org.shark.renovatio.provider.java.util;

import org.openrewrite.Recipe;
import java.lang.reflect.Method;
import java.util.Collection;
import java.util.Optional;

public class RecipeSafetyUtils {
    private RecipeSafetyUtils() {}

    /**
     * Returns true if the Recipe instance has missing required parameters (for safety checks).
     */
    public static boolean hasMissingRequiredParameters(Recipe recipe) {
        if (recipe == null) return true;
        String recipeName = recipe.getName();
        // Special logic for CreateEmptyJavaClass and subclasses: both methods must exist and return valid values
        if (recipeName != null && recipeName.startsWith("org.openrewrite.java.CreateEmptyJavaClass")) {
            // Both methods must exist and return valid, non-blank values
            try {
                Method getPackage = recipe.getClass().getMethod("getPackageName");
                getPackage.setAccessible(true); // Fix IllegalAccessException
                Object packageValue = getPackage.invoke(recipe);
                if (packageValue == null) {
                    return true;
                }
                if (packageValue instanceof Optional<?> opt) {
                    if (opt.isEmpty()) {
                        return true;
                    }
                    Object inner = opt.get();
                    if (inner instanceof String s && s.isBlank()) {
                        return true;
                    }
                } else if (packageValue instanceof String s && s.isBlank()) {
                    return true;
                }
            } catch (Exception e) {
                e.printStackTrace();
                return true;
            }
            try {
                Method getClassName = recipe.getClass().getMethod("getClassName");
                getClassName.setAccessible(true); // Fix IllegalAccessException
                Object classValue = getClassName.invoke(recipe);
                if (classValue == null) {
                    return true;
                }
                if (classValue instanceof Optional<?> opt) {
                    if (opt.isEmpty()) {
                        return true;
                    }
                    Object inner = opt.get();
                    if (inner instanceof String s && s.isBlank()) {
                        return true;
                    }
                } else if (classValue instanceof String s && s.isBlank()) {
                    return true;
                }
            } catch (Exception e) {
                e.printStackTrace();
                return true;
            }
            // If both parameters are present and non-blank, it's safe
            return false;
        }
        // Lista de getters relevantes para recetas de creación y refactorización
        String[] requiredGetters = new String[] {
            "getPath", "getFile", "getFilename", "getTarget", "getSource", "getOldName", "getNewName"
        };
        for (String getter : requiredGetters) {
            try {
                Method m = recipe.getClass().getMethod(getter);
                Object value = m.invoke(recipe);
                if (isValueMissing(value)) {
                    return true;
                }
            } catch (NoSuchMethodException ignored) {
                // Not all recipes have all getters
            } catch (Exception e) {
                return true;
            }
        }
        // Fallback: check toString for common null/empty patterns
        String asString = recipe.toString();
        if (asString.contains("=null") || asString.contains("=''")) {
            return true;
        }
        // Add more explicit recipe names if needed
        if ("org.openrewrite.yaml.CreateYamlFile".equals(recipeName) ||
            "org.openrewrite.text.CreateTextFile".equals(recipeName) ||
            "org.openrewrite.xml.CreateXmlFile".equals(recipeName) ||
            "org.openrewrite.text.AppendToTextFile".equals(recipeName)) {
            return true;
        }
        return false;
    }

    private static boolean isValueMissing(Object value) {
        // Null is always missing
        if (value == null) return true;
        // If value is Optional, only empty is missing; if present, check if it's a blank String, otherwise accept
        if (value instanceof Optional<?> opt) {
            if (opt.isEmpty()) return true;
            Object inner = opt.get();
            // If present and String blank, mark as missing
            if (inner instanceof String s) return s.isBlank();
            // If present and null or any other type, consider valid (do NOT mark as missing)
            return false;
        }
        // If value is a String, blank is missing
        if (value instanceof String s) return s.isBlank();
        // If value is a Collection, empty is missing
        if (value instanceof Collection<?> col) return col.isEmpty();
        // If value is an array, empty is missing
        if (value.getClass().isArray()) return java.lang.reflect.Array.getLength(value) == 0;
        // Any other type: not missing
        return false;
    }
}
