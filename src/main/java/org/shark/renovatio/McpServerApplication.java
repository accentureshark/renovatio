package org.shark.renovatio;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

@SpringBootApplication
public class McpServerApplication {
    public static void main(String[] args) {
        SpringApplication.run(McpServerApplication.class, args);
    }
}
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

