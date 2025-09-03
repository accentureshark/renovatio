package org.shark.renovatio.application;

import org.openrewrite.Recipe;
import org.openrewrite.java.format.AutoFormat;
import org.shark.renovatio.domain.RefactorRequest;
import org.shark.renovatio.domain.RefactorResponse;
import org.shark.renovatio.domain.Tool;
import org.springframework.stereotype.Service;

import java.util.*;
import java.util.function.Supplier;
import java.util.stream.Collectors;

@Service
public class McpToolingService {
    private final RefactorService refactorService;
    private final Map<String, Supplier<Recipe>> recipeSuppliers;
    private final List<Tool> tools;

    public McpToolingService(RefactorService refactorService) {
        this.refactorService = refactorService;
        // Definir recetas soportadas manualmente
        this.recipeSuppliers = new LinkedHashMap<>();
        this.recipeSuppliers.put("AutoFormat", AutoFormat::new);
        // Puedes agregar más recetas aquí
        this.tools = recipeSuppliers.keySet().stream()
            .map(name -> {
                Tool tool = new Tool();
                tool.setName(name);
                tool.setDescription("Receta OpenRewrite: " + name);
                tool.setCommand(name);
                return tool;
            })
            .collect(Collectors.toList());
    }

    public List<Tool> getTools() {
        return tools;
    }

    public RefactorResponse runTool(String recipeName, RefactorRequest request) {
        Supplier<Recipe> supplier = recipeSuppliers.get(recipeName);
        if (supplier == null) {
            return new RefactorResponse(request.getSourceCode(), "Receta no soportada: " + recipeName);
        }
        return refactorService.refactorWithRecipe(supplier.get(), request);
    }
}
