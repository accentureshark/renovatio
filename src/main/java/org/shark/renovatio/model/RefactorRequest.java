package org.shark.renovatio.model;

public class RefactorRequest {
    private String sourceCode;
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
