package org.shark.renovatio.domain;

import io.swagger.v3.oas.annotations.media.Schema;

@Schema(description = "Petición para refactorizar código")
public class RefactorRequest {
    @Schema(description = "Código fuente a refactorizar")
    private String sourceCode;
    @Schema(description = "Nombre de la receta de OpenRewrite a ejecutar")
    private String recipe;

    public String getSourceCode() {
        return sourceCode;
    }
    public void setSourceCode(String sourceCode) {
        this.sourceCode = sourceCode;
    }
    public String getRecipe() {
        return recipe;
    }
    public void setRecipe(String recipe) {
        this.recipe = recipe;
    }
}
