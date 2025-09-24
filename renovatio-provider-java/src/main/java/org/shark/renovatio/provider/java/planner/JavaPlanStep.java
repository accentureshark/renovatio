package org.shark.renovatio.provider.java.planner;

import java.util.LinkedHashMap;
import java.util.Map;

/**
 * Describes a single step in a refactoring plan.
 */
public record JavaPlanStep(String id, String recipe, String description) {

    public Map<String, Object> toMap() {
        Map<String, Object> map = new LinkedHashMap<>();
        map.put("id", id);
        map.put("recipe", recipe);
        map.put("description", description);
        return map;
    }
}
