package org.shark.renovatio.shared.domain;

import java.util.Map;

/**
 * Result of stub generation
 */
public class StubResult extends ProviderResult {
    private String targetLanguage;
    private Map<String, String> generatedFiles;
    private String stubTemplate;
    private Map<String, String> generatedCode;
    
    public StubResult() {}
    
    public StubResult(boolean success, String message) {
        super(success, message);
    }
    
    public String getTargetLanguage() { return targetLanguage; }
    public void setTargetLanguage(String targetLanguage) { this.targetLanguage = targetLanguage; }
    
    public Map<String, String> getGeneratedFiles() { return generatedFiles; }
    public void setGeneratedFiles(Map<String, String> generatedFiles) { this.generatedFiles = generatedFiles; }
    
    public String getStubTemplate() { return stubTemplate; }
    public void setStubTemplate(String stubTemplate) { this.stubTemplate = stubTemplate; }
    
    public Map<String, String> getGeneratedCode() { return generatedCode; }
    public void setGeneratedCode(Map<String, String> generatedCode) { this.generatedCode = generatedCode; }
}