package org.shark.renovatio.application;

import org.shark.renovatio.domain.RefactorRequest;
import org.shark.renovatio.domain.RefactorResponse;
import org.openrewrite.InMemoryExecutionContext;
import org.openrewrite.Recipe;
import org.openrewrite.config.Environment;
import org.openrewrite.java.JavaParser;
import org.springframework.stereotype.Service;

@Service
public class RefactorService {
    public RefactorResponse refactorCode(RefactorRequest request) {
        try {
            JavaParser parser = JavaParser.fromJavaVersion().build();
            Environment env = Environment.builder().scanRuntimeClasspath().build();
            Recipe recipe = env.activateRecipes(request.getRecipe());
            if (recipe.getRecipeList().isEmpty()) {
                return new RefactorResponse(request.getSourceCode(), "Receta no soportada: " + request.getRecipe());
            }
            var cu = parser.parse(request.getSourceCode());
            var runResult = recipe.run(cu, new InMemoryExecutionContext(Throwable::printStackTrace));
            // OpenRewrite puede devolver los resultados en runResult.getResults()
            String refactoredCode = request.getSourceCode();
            if (!runResult.getResults().isEmpty()) {
                refactoredCode = runResult.getResults().get(0).getAfter().printAll();
            }
            return new RefactorResponse(refactoredCode, "Refactorización exitosa");
        } catch (Exception e) {
            return new RefactorResponse(request.getSourceCode(), "Error: " + e.getMessage());
        }
    }
}
