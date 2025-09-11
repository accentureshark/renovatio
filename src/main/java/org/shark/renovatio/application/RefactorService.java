package org.shark.renovatio.application;

import java.util.Map;

import org.springframework.stereotype.Service;
import org.shark.renovatio.application.recipes.RecipeHandler;
import org.shark.renovatio.domain.RefactorRequest;
import org.shark.renovatio.domain.RefactorResponse;

@Service
public class RefactorService {

    private final Map<String, RecipeHandler> recipeHandlers;

    public RefactorService(Map<String, RecipeHandler> recipeHandlers) {
        this.recipeHandlers = recipeHandlers;
    }

    public RefactorResponse refactor(RefactorRequest request) {
        try {
            String refactoredCode = applyRecipe(request.getSourceCode(), request.getRecipe());
            String message = "Receta '" + request.getRecipe() + "' aplicada exitosamente";

            return new RefactorResponse(refactoredCode, message);
        } catch (Exception e) {
            return new RefactorResponse(request.getSourceCode(), "Error: " + e.getMessage());
        }
    }

    private String applyRecipe(String sourceCode, String recipe) {
        RecipeHandler handler = recipeHandlers.get(recipe);
        if (handler == null) {
            return sourceCode + "\n// Applied recipe: " + recipe;
        }
        return handler.apply(sourceCode);
    }
}
