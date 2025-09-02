package org.shark.renovatio.service;

import org.shark.renovatio.model.RefactorRequest;
import org.shark.renovatio.model.RefactorResponse;
import org.openrewrite.InMemoryExecutionContext;
import org.openrewrite.Recipe;
import org.openrewrite.java.JavaParser;
import org.openrewrite.java.cleanup.RemoveUnusedImports;
import org.springframework.stereotype.Service;

@Service
public class RefactorService {
    public RefactorResponse refactorCode(RefactorRequest request) {
        try {
            JavaParser parser = JavaParser.fromJavaVersion().build();
            Recipe recipe;
            if ("RemoveUnusedImports".equals(request.getRecipe())) {
                recipe = new RemoveUnusedImports();
            } else {
                return new RefactorResponse(request.getSourceCode(), "Receta no soportada");
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
