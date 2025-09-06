package org.shark.renovatio.provider.cobol.domain;

import java.util.List;
import java.util.Map;

/**
 * Represents a COBOL data item (field/variable)
 */
public class CobolDataItem {
    private String name;
    private int level;
    private String picture;
    private String usage;
    private String value;
    private boolean isGroup;
    private List<CobolDataItem> children;
    private Map<String, Object> attributes;
    
    public CobolDataItem() {}
    
    public CobolDataItem(String name, int level, String picture) {
        this.name = name;
        this.level = level;
        this.picture = picture;
    }
    
    // Getters and setters
    public String getName() { return name; }
    public void setName(String name) { this.name = name; }
    
    public int getLevel() { return level; }
    public void setLevel(int level) { this.level = level; }
    
    public String getPicture() { return picture; }
    public void setPicture(String picture) { this.picture = picture; }
    
    public String getUsage() { return usage; }
    public void setUsage(String usage) { this.usage = usage; }
    
    public String getValue() { return value; }
    public void setValue(String value) { this.value = value; }
    
    public boolean isGroup() { return isGroup; }
    public void setGroup(boolean group) { isGroup = group; }
    
    public List<CobolDataItem> getChildren() { return children; }
    public void setChildren(List<CobolDataItem> children) { this.children = children; }
    
    public Map<String, Object> getAttributes() { return attributes; }
    public void setAttributes(Map<String, Object> attributes) { this.attributes = attributes; }
    
    /**
     * Converts COBOL picture clause to Java type
     */
    public String getJavaType() {
        if (picture == null) return "Object";
        
        if (picture.startsWith("9")) {
            if (picture.contains("V")) {
                return "BigDecimal";
            } else if (picture.length() <= 9) {
                return "Integer";
            } else {
                return "Long";
            }
        } else if (picture.startsWith("X")) {
            return "String";
        } else if (picture.startsWith("A")) {
            return "String";
        }
        
        return "Object";
    }
}

/**
 * Represents a COBOL paragraph
 */
class CobolParagraph {
    private String name;
    private List<CobolStatement> statements;
    
    public String getName() { return name; }
    public void setName(String name) { this.name = name; }
    
    public List<CobolStatement> getStatements() { return statements; }
    public void setStatements(List<CobolStatement> statements) { this.statements = statements; }
}

/**
 * Represents a COBOL section
 */
class CobolSection {
    private String name;
    private List<CobolParagraph> paragraphs;
    
    public String getName() { return name; }
    public void setName(String name) { this.name = name; }
    
    public List<CobolParagraph> getParagraphs() { return paragraphs; }
    public void setParagraphs(List<CobolParagraph> paragraphs) { this.paragraphs = paragraphs; }
}

/**
 * Represents a COBOL statement
 */
class CobolStatement {
    private StatementType type;
    private String sourceCode;
    private Map<String, Object> attributes;
    
    public StatementType getType() { return type; }
    public void setType(StatementType type) { this.type = type; }
    
    public String getSourceCode() { return sourceCode; }
    public void setSourceCode(String sourceCode) { this.sourceCode = sourceCode; }
    
    public Map<String, Object> getAttributes() { return attributes; }
    public void setAttributes(Map<String, Object> attributes) { this.attributes = attributes; }
    
    public enum StatementType {
        MOVE, COMPUTE, IF, PERFORM, CALL, READ, WRITE, OPEN, CLOSE, 
        DISPLAY, ACCEPT, EXIT, STOP, OTHER
    }
}