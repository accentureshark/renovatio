package org.shark.renovatio.shared.nql;

import java.util.List;
import java.util.Map;

/**
 * Natural Query Language (NQL) query representation
 * Grammar: find/plan/apply with target, predicate, scope and return
 */
public class NqlQuery {
    private QueryType type;          // find, plan, apply
    private String target;           // methods, classes, files, records
    private String predicate;        // conditions to match
    private String scope;           // where to look
    private String returnClause;    // what to return
    private Map<String, Object> parameters;
    private String originalQuery;   // original natural language
    private String language;        // target language (java, cobol, etc.)
    
    public NqlQuery() {}
    
    public NqlQuery(QueryType type, String target, String predicate) {
        this.type = type;
        this.target = target;
        this.predicate = predicate;
    }
    
    // Getters and setters
    public QueryType getType() { return type; }
    public void setType(QueryType type) { this.type = type; }
    
    public String getTarget() { return target; }
    public void setTarget(String target) { this.target = target; }
    
    public String getPredicate() { return predicate; }
    public void setPredicate(String predicate) { this.predicate = predicate; }
    
    public String getScope() { return scope; }
    public void setScope(String scope) { this.scope = scope; }
    
    public String getReturnClause() { return returnClause; }
    public void setReturnClause(String returnClause) { this.returnClause = returnClause; }
    
    public Map<String, Object> getParameters() { return parameters; }
    public void setParameters(Map<String, Object> parameters) { this.parameters = parameters; }
    
    public String getOriginalQuery() { return originalQuery; }
    public void setOriginalQuery(String originalQuery) { this.originalQuery = originalQuery; }
    
    public String getLanguage() { return language; }
    public void setLanguage(String language) { this.language = language; }
    
    public enum QueryType {
        FIND,    // find code patterns
        PLAN,    // create execution plan
        APPLY    // apply transformations
    }
}