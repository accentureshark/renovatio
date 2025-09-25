package org.shark.renovatio.shared.domain;

import java.util.Map;


/**
 * Result of analyze operation
 */
public class AnalyzeResult extends ProviderResult {
    private Map<String, Object> ast;
    private Map<String, Object> dependencies;
    private Map<String, Object> symbols;
    private Map<String, Object> data;
    private PerformanceMetrics performance;

    public AnalyzeResult() {
    }

    public AnalyzeResult(boolean success, String message) {
        super(success, message);
    }

    public Map<String, Object> getAst() {
        return ast;
    }

    public void setAst(Map<String, Object> ast) {
        this.ast = ast;
    }

    public Map<String, Object> getDependencies() {
        return dependencies;
    }

    public void setDependencies(Map<String, Object> dependencies) {
        this.dependencies = dependencies;
    }

    public Map<String, Object> getSymbols() {
        return symbols;
    }

    public void setSymbols(Map<String, Object> symbols) {
        this.symbols = symbols;
    }

    public Map<String, Object> getData() {
        return data;
    }

    public void setData(Map<String, Object> data) {
        this.data = data;
    }

    public PerformanceMetrics getPerformance() {
        return performance;
    }

    public void setPerformance(PerformanceMetrics performance) {
        this.performance = performance;
    }
}