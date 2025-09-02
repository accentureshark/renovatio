package org.shark.renovatio.domain;

public class RefactorResponse {
    private String refactoredCode;
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

