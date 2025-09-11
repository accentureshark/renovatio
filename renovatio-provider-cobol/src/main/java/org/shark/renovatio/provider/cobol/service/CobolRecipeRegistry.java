package org.shark.renovatio.provider.cobol.service;

import org.springframework.stereotype.Component;

import java.io.BufferedReader;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * Registry for COBOL migration recipes defined in cobol-rewrite.yml.
 * Loads the recipe names and exposes them in declaration order.
 */
@Component
public class CobolRecipeRegistry {

    private final List<String> recipes;

    public CobolRecipeRegistry() {
        this.recipes = loadRecipes();
    }

    /**
     * @return immutable list of recipe names in declaration order
     */
    public List<String> getRecipes() {
        return recipes;
    }

    private List<String> loadRecipes() {
        Path path = Paths.get("cobol-rewrite.yml");
        if (!Files.exists(path)) {
            return Collections.emptyList();
        }
        List<String> list = new ArrayList<>();
        try (BufferedReader reader = Files.newBufferedReader(path)) {
            String line;
            boolean inList = false;
            while ((line = reader.readLine()) != null) {
                line = line.trim();
                if (line.startsWith("recipeList")) {
                    inList = true;
                    continue;
                }
                if (inList) {
                    if (line.startsWith("-")) {
                        list.add(line.substring(1).trim());
                    } else if (!line.isEmpty() && !line.startsWith("#")) {
                        // any non-list line ends the section
                        inList = false;
                    }
                }
            }
        } catch (IOException ignored) {
        }
        return List.copyOf(list);
    }
}
