package org.shark.renovatio.provider.java.util;

import org.openrewrite.Recipe;

import java.lang.reflect.Method;
import java.util.Collection;
import java.util.Optional;

public class RecipeSafetyUtils {
    private RecipeSafetyUtils() {
    }

    /**
     * Returns true if the Recipe instance has missing required parameters (for safety checks).
     */
    public static boolean hasMissingRequiredParameters(Recipe recipe) {
        if (recipe == null) return true;
        String recipeName = recipe.getName();
        // Special logic for CreateEmptyJavaClass and subclasses: both methods must exist and return valid values
        if (recipeName != null && recipeName.startsWith("org.openrewrite.java.CreateEmptyJavaClass")) {
            // Both methods must exist and return valid, non-blank values
            if (isBlankish(safeInvoke(recipe, "getPackageName"))) {
                return true;
            }
            if (isBlankish(safeInvoke(recipe, "getClassName"))) {
                return true;
            }
            // If both parameters are present and non-blank, it's safe
            return false;
        }
        // Lista de getters relevantes para recetas de creación y refactorización
        String[] requiredGetters = new String[]{
                "getPath", "getFile", "getFilename", "getTarget", "getSource", "getOldName", "getNewName"
        };
        for (String getter : requiredGetters) {
            try {
                Method m = recipe.getClass().getMethod(getter);
                if (!m.canAccess(recipe)) {
                    // If we cannot access, treat as missing
                    return true;
                }
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

    private static Object safeInvoke(Recipe recipe, String method) {
        try {
            Method m = recipe.getClass().getMethod(method);
            if (!m.canAccess(recipe)) {
                return null; // Don't force accessible; treat as missing
            }
            return m.invoke(recipe);
        } catch (Exception e) {
            return null;
        }
    }

    private static boolean isBlankish(Object value) {
        if (value == null) return true;
        if (value instanceof Optional<?> opt) {
            if (opt.isEmpty()) return true;
            Object inner = opt.get();
            if (inner instanceof String s) return s.isBlank();
            return inner == null; // treat null optional content as blankish
        }
        if (value instanceof String s) return s.isBlank();
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
