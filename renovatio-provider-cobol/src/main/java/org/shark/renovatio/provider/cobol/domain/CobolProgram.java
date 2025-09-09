package org.shark.renovatio.provider.cobol.domain;

import java.util.List;
import java.util.Map;

/**
 * Represents a parsed COBOL program structure
 */
public class CobolProgram {
    private String programId;
    private String programName;
    private CobolEnvironmentDivision environmentDivision;
    private CobolDataDivision dataDivision;
    private CobolProcedureDivision procedureDivision;
    private Map<String, Object> metadata;
    
    public CobolProgram() {}
    
    public CobolProgram(String programId, String programName) {
        this.programId = programId;
        this.programName = programName;
    }
    
    // Getters and setters
    public String getProgramId() { return programId; }
    public void setProgramId(String programId) { this.programId = programId; }
    
    public String getProgramName() { return programName; }
    public void setProgramName(String programName) { this.programName = programName; }
    
    public CobolEnvironmentDivision getEnvironmentDivision() { return environmentDivision; }
    public void setEnvironmentDivision(CobolEnvironmentDivision environmentDivision) { this.environmentDivision = environmentDivision; }
    
    public CobolDataDivision getDataDivision() { return dataDivision; }
    public void setDataDivision(CobolDataDivision dataDivision) { this.dataDivision = dataDivision; }
    
    public CobolProcedureDivision getProcedureDivision() { return procedureDivision; }
    public void setProcedureDivision(CobolProcedureDivision procedureDivision) { this.procedureDivision = procedureDivision; }
    
    public Map<String, Object> getMetadata() { return metadata; }
    public void setMetadata(Map<String, Object> metadata) { this.metadata = metadata; }
}

/**
 * COBOL Environment Division representation
 */
class CobolEnvironmentDivision {
    private Map<String, String> configurationSection;
    private Map<String, String> inputOutputSection;
    
    public Map<String, String> getConfigurationSection() { return configurationSection; }
    public void setConfigurationSection(Map<String, String> configurationSection) { this.configurationSection = configurationSection; }
    
    public Map<String, String> getInputOutputSection() { return inputOutputSection; }
    public void setInputOutputSection(Map<String, String> inputOutputSection) { this.inputOutputSection = inputOutputSection; }
}

/**
 * COBOL Data Division representation
 */
class CobolDataDivision {
    private List<CobolDataItem> workingStorageSection;
    private List<CobolDataItem> fileSection;
    private List<CobolDataItem> linkageSection;
    
    public List<CobolDataItem> getWorkingStorageSection() { return workingStorageSection; }
    public void setWorkingStorageSection(List<CobolDataItem> workingStorageSection) { this.workingStorageSection = workingStorageSection; }
    
    public List<CobolDataItem> getFileSection() { return fileSection; }
    public void setFileSection(List<CobolDataItem> fileSection) { this.fileSection = fileSection; }
    
    public List<CobolDataItem> getLinkageSection() { return linkageSection; }
    public void setLinkageSection(List<CobolDataItem> linkageSection) { this.linkageSection = linkageSection; }
}

/**
 * COBOL Procedure Division representation
 */
class CobolProcedureDivision {
    private List<CobolParagraph> paragraphs;
    private List<CobolSection> sections;
    
    public List<CobolParagraph> getParagraphs() { return paragraphs; }
    public void setParagraphs(List<CobolParagraph> paragraphs) { this.paragraphs = paragraphs; }
    
    public List<CobolSection> getSections() { return sections; }
    public void setSections(List<CobolSection> sections) { this.sections = sections; }
}