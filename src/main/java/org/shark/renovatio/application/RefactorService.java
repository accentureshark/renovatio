package org.shark.renovatio.application;

import org.openrewrite.InMemoryExecutionContext;
import org.openrewrite.Recipe;
import org.openrewrite.java.JavaParser;
import org.openrewrite.SourceFile;
import org.openrewrite.RecipeRun;
import org.openrewrite.Result;
import org.shark.renovatio.domain.RefactorRequest;
import org.shark.renovatio.domain.RefactorResponse;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.stream.Collectors;

@Service
public class RefactorService {
    public RefactorResponse refactorCode(RefactorRequest request) {
        // Por compatibilidad, solo soporta AutoFormat
        return refactorWithRecipe(new org.openrewrite.java.format.AutoFormat(), request);
    }

    public RefactorResponse refactorWithRecipe(Recipe recipe, RefactorRequest request) {
        try {
            JavaParser parser = JavaParser.fromJavaVersion().build();
            List<SourceFile> cus = parser.parse(request.getSourceCode()).collect(Collectors.toList());
            RecipeRun run = recipe.run(cus, new InMemoryExecutionContext(Throwable::printStackTrace));
            String refactoredCode = request.getSourceCode();
            List<Result> results = run.getResults();
            if (!results.isEmpty() && results.get(0).getAfter() != null) {
                refactoredCode = results.get(0).getAfter().printAll();
            }
            return new RefactorResponse(refactoredCode, "Refactorización exitosa");
        } catch (Exception e) {
            return new RefactorResponse(request.getSourceCode(), "Error: " + e.getMessage());
        }
    }
}
