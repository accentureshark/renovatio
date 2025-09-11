package org.shark.renovatio.application;

import java.util.Map;

import org.springframework.stereotype.Service;
import org.shark.renovatio.application.recipes.RecipeHandler;
import org.shark.renovatio.domain.RefactorRequest;
import org.shark.renovatio.domain.RefactorResponse;

@Service
public class RefactorService {

    private final Map<String, RecipeHandler> handlers;

    public RefactorService(Map<String, RecipeHandler> handlers) {
        this.handlers = handlers;
    }

    public RefactorResponse refactor(RefactorRequest request) {
        try {
            // For now, implement basic transformations to demonstrate functionality
            // This will be enhanced with proper OpenRewrite integration later
            String refactoredCode = applyBasicRefactoring(request.getSourceCode(), request.getRecipe());
            String message = "Receta '" + request.getRecipe() + "' aplicada exitosamente";
            
            return new RefactorResponse(refactoredCode, message);
        } catch (Exception e) {
            return new RefactorResponse(request.getSourceCode(), "Error: " + e.getMessage());
        }
    }
    
    private String applyBasicRefactoring(String sourceCode, String recipe) {
        RecipeHandler handler = handlers.get(recipe);
        if (handler != null) {
            return handler.apply(sourceCode);
        }
        return sourceCode + "\n// Applied recipe: " + recipe;
    }
}
