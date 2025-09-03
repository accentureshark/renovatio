package org.shark.renovatio.domain;

import io.swagger.v3.oas.annotations.media.Schema;

@Schema(description = "Respuesta tras la refactorización")
public class RefactorResponse {
    @Schema(description = "Código resultante luego de aplicar la receta")
    private String refactoredCode;
    @Schema(description = "Mensaje con el resultado de la operación")
    private String message;

    public RefactorResponse() {}
    public RefactorResponse(String refactoredCode, String message) {
        this.refactoredCode = refactoredCode;
        this.message = message;
    }
    public String getRefactoredCode() {
        return refactoredCode;
    }
    public void setRefactoredCode(String refactoredCode) {
        this.refactoredCode = refactoredCode;
    }
    public String getMessage() {
        return message;
    }
    public void setMessage(String message) {
        this.message = message;
    }
}

