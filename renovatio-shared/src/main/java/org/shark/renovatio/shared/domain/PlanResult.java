package org.shark.renovatio.shared.domain;

import java.util.Map;

/**
 * Result of plan operation
 */
public class PlanResult extends ProviderResult {
    private String planId;
    private String planContent;
    private Map<String, Object> steps;
    
    public PlanResult() {}
    
    public PlanResult(boolean success, String message) {
        super(success, message);
    }
    
    public String getPlanId() { return planId; }
    public void setPlanId(String planId) { this.planId = planId; }
    
    public String getPlanContent() { return planContent; }
    public void setPlanContent(String planContent) { this.planContent = planContent; }
    
    public Map<String, Object> getSteps() { return steps; }
    public void setSteps(Map<String, Object> steps) { this.steps = steps; }
}