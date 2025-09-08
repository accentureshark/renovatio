package org.shark.renovatio.provider.cobol.infrastructure;

import org.shark.renovatio.provider.cobol.domain.CobolMcpTool;
import org.shark.renovatio.provider.cobol.CobolLanguageProvider;
import org.shark.renovatio.shared.domain.*;
import org.shark.renovatio.shared.nql.NqlQuery;
import org.springframework.stereotype.Component;

import java.util.*;

/**
 * MCP tools integration for COBOL migration capabilities
 * Exposes COBOL provider functionality as MCP tools
 */
@Component
public class CobolMcpToolsProvider {
    
    private final CobolLanguageProvider cobolProvider;
    
    public CobolMcpToolsProvider(CobolLanguageProvider cobolProvider) {
        this.cobolProvider = cobolProvider;
    }
    
    /**
     * Gets all available COBOL migration tools
     */
    public List<CobolMcpTool> getCobolMigrationTools() {
        List<CobolMcpTool> tools = new ArrayList<>();
        
        tools.add(createAnalyzeCobolTool());
        tools.add(createGenerateJavaStubsTool());
        tools.add(createCreateMigrationPlanTool());
        tools.add(createApplyMigrationPlanTool());
        tools.add(createCalculateMetricsTool());
        tools.add(createGenerateDiffTool());
        
        return tools;
    }
    
    /**
     * Executes COBOL migration tool
     */
    public Object executeCobolTool(String toolName, Map<String, Object> arguments) {
        return switch (toolName) {
            case "cobol.analyze" -> executeAnalyzeTool(arguments);
            case "cobol.generate.stubs" -> executeGenerateStubsTool(arguments);
            case "cobol.migration.plan" -> executeCreatePlanTool(arguments);
            case "cobol.migration.apply" -> executeApplyPlanTool(arguments);
            case "cobol.metrics" -> executeMetricsTool(arguments);
            case "cobol.diff" -> executeDiffTool(arguments);
            default -> Map.of("error", "Unknown COBOL tool: " + toolName);
        };
    }
    
    private CobolMcpTool createAnalyzeCobolTool() {
        CobolMcpTool tool = new CobolMcpTool();
        tool.setName("cobol.analyze");
        tool.setDescription("Analyze COBOL programs and extract structure information");
        
        Map<String, Object> schema = new HashMap<>();
        schema.put("type", "object");
        schema.put("properties", Map.of(
            "workspacePath", Map.of("type", "string", "description", "Path to COBOL workspace"),
            "query", Map.of("type", "string", "description", "Analysis query"),
            "includeMetrics", Map.of("type", "boolean", "description", "Include code metrics in analysis")
        ));
        schema.put("required", List.of("workspacePath"));
        
        tool.setInputSchema(schema);
        return tool;
    }
    
    private CobolMcpTool createGenerateJavaStubsTool() {
        CobolMcpTool tool = new CobolMcpTool();
        tool.setName("cobol.generate.stubs");
        tool.setDescription("Generate Java interface stubs from COBOL programs");
        
        Map<String, Object> schema = new HashMap<>();
        schema.put("type", "object");
        schema.put("properties", Map.of(
            "workspacePath", Map.of("type", "string", "description", "Path to COBOL workspace"),
            "targetPackage", Map.of("type", "string", "description", "Java package for generated code"),
            "generateTests", Map.of("type", "boolean", "description", "Generate test classes")
        ));
        schema.put("required", List.of("workspacePath"));
        
        tool.setInputSchema(schema);
        return tool;
    }
    
    private CobolMcpTool createCreateMigrationPlanTool() {
        CobolMcpTool tool = new CobolMcpTool();
        tool.setName("cobol.migration.plan");
        tool.setDescription("Create a migration plan for COBOL to Java transformation");
        
        Map<String, Object> schema = new HashMap<>();
        schema.put("type", "object");
        schema.put("properties", Map.of(
            "workspacePath", Map.of("type", "string", "description", "Path to COBOL workspace"),
            "migrationStrategy", Map.of("type", "string", "description", "Migration strategy (full, incremental, hybrid)"),
            "targetFramework", Map.of("type", "string", "description", "Target Java framework (spring-boot, quarkus, etc.)")
        ));
        schema.put("required", List.of("workspacePath"));
        
        tool.setInputSchema(schema);
        return tool;
    }
    
    private CobolMcpTool createApplyMigrationPlanTool() {
        CobolMcpTool tool = new CobolMcpTool();
        tool.setName("cobol.migration.apply");
        tool.setDescription("Apply a migration plan to transform COBOL to Java");
        
        Map<String, Object> schema = new HashMap<>();
        schema.put("type", "object");
        schema.put("properties", Map.of(
            "planId", Map.of("type", "string", "description", "Migration plan ID"),
            "dryRun", Map.of("type", "boolean", "description", "Execute as dry run"),
            "outputPath", Map.of("type", "string", "description", "Output path for generated Java code")
        ));
        schema.put("required", List.of("planId"));
        
        tool.setInputSchema(schema);
        return tool;
    }
    
    private CobolMcpTool createCalculateMetricsTool() {
        CobolMcpTool tool = new CobolMcpTool();
        tool.setName("cobol.metrics");
        tool.setDescription("Calculate code metrics for COBOL programs");
        
        Map<String, Object> schema = new HashMap<>();
        schema.put("type", "object");
        schema.put("properties", Map.of(
            "workspacePath", Map.of("type", "string", "description", "Path to COBOL workspace"),
            "includeComplexity", Map.of("type", "boolean", "description", "Include cyclomatic complexity"),
            "includeDependencies", Map.of("type", "boolean", "description", "Include dependency analysis")
        ));
        schema.put("required", List.of("workspacePath"));
        
        tool.setInputSchema(schema);
        return tool;
    }
    
    private CobolMcpTool createGenerateDiffTool() {
        CobolMcpTool tool = new CobolMcpTool();
        tool.setName("cobol.diff");
        tool.setDescription("Generate diff for migration changes");
        
        Map<String, Object> schema = new HashMap<>();
        schema.put("type", "object");
        schema.put("properties", Map.of(
            "runId", Map.of("type", "string", "description", "Migration run ID"),
            "format", Map.of("type", "string", "description", "Diff format (unified, semantic, both)")
        ));
        schema.put("required", List.of("runId"));
        
        tool.setInputSchema(schema);
        return tool;
    }
    
    // Tool execution methods
    
    private Object executeAnalyzeTool(Map<String, Object> arguments) {
        try {
            String workspacePath = (String) arguments.get("workspacePath");
            
            Workspace workspace = new Workspace();
            workspace.setId("mcp-" + System.currentTimeMillis());
            workspace.setPath(workspacePath);
            workspace.setBranch("main");
            
            NqlQuery query = new NqlQuery();
            query.setType(NqlQuery.QueryType.FIND);
            query.setTarget("programs");
            query.setLanguage("cobol");
            
            AnalyzeResult result = cobolProvider.analyze(query, workspace);

            // Crear respuesta completa con todos los datos del análisis
            Map<String, Object> response = new HashMap<>();
            response.put("success", result.isSuccess());
            response.put("message", result.getMessage());

            // Incluir todos los datos del análisis real
            if (result.getData() != null) {
                response.put("data", result.getData());
            }

            // Incluir AST si está disponible
            if (result.getAst() != null) {
                response.put("ast", result.getAst());
            }

            // Incluir símbolos si están disponibles
            if (result.getSymbols() != null) {
                response.put("symbols", result.getSymbols());
            }

            // Incluir dependencias si están disponibles
            if (result.getDependencies() != null) {
                response.put("dependencies", result.getDependencies());
            }

            // Incluir runId si está disponible
            if (result.getRunId() != null) {
                response.put("runId", result.getRunId());
            }

            return response;

        } catch (Exception e) {
            return Map.of("success", false, "error", e.getMessage());
        }
    }
    
    private Object executeGenerateStubsTool(Map<String, Object> arguments) {
        try {
            String workspacePath = (String) arguments.get("workspacePath");
            
            Workspace workspace = new Workspace();
            workspace.setId("mcp-" + System.currentTimeMillis());
            workspace.setPath(workspacePath);
            workspace.setBranch("main");
            
            NqlQuery query = new NqlQuery();
            query.setType(NqlQuery.QueryType.FIND);
            query.setTarget("stubs");
            query.setLanguage("cobol");
            
            Optional<StubResult> result = cobolProvider.generateStubs(query, workspace);
            if (result.isPresent()) {
                StubResult stubResult = result.get();
                return Map.of(
                    "success", stubResult.isSuccess(),
                    "message", stubResult.getMessage(),
                    "generatedFiles", stubResult.getGeneratedCode() != null ? stubResult.getGeneratedCode().size() : 0,
                    "files", stubResult.getGeneratedCode()
                );
            } else {
                return Map.of("success", false, "error", "No stubs generated");
            }
            
        } catch (Exception e) {
            return Map.of("success", false, "error", e.getMessage());
        }
    }
    
    private Object executeCreatePlanTool(Map<String, Object> arguments) {
        try {
            String workspacePath = (String) arguments.get("workspacePath");
            
            Workspace workspace = new Workspace();
            workspace.setId("mcp-" + System.currentTimeMillis());
            workspace.setPath(workspacePath);
            workspace.setBranch("main");
            
            NqlQuery query = new NqlQuery();
            query.setType(NqlQuery.QueryType.PLAN);
            query.setTarget("migration");
            query.setLanguage("cobol");
            
            Scope scope = new Scope();
            
            PlanResult result = cobolProvider.plan(query, scope, workspace);
            return Map.of(
                "success", result.isSuccess(),
                "message", result.getMessage(),
                "planId", result.getPlanId(),
                "steps", result.getSteps()
            );
            
        } catch (Exception e) {
            return Map.of("success", false, "error", e.getMessage());
        }
    }
    
    private Object executeApplyPlanTool(Map<String, Object> arguments) {
        try {
            String planId = (String) arguments.get("planId");
            Boolean dryRun = (Boolean) arguments.getOrDefault("dryRun", true);
            
            Workspace workspace = new Workspace();
            workspace.setId("mcp-" + System.currentTimeMillis());
            workspace.setPath("/tmp");
            workspace.setBranch("main");
            
            ApplyResult result = cobolProvider.apply(planId, dryRun, workspace);
            return Map.of(
                "success", result.isSuccess(),
                "message", result.getMessage(),
                "runId", result.getRunId(),
                "modifiedFiles", result.getModifiedFiles(),
                "changes", result.getChanges()
            );
            
        } catch (Exception e) {
            return Map.of("success", false, "error", e.getMessage());
        }
    }
    
    private Object executeMetricsTool(Map<String, Object> arguments) {
        try {
            String workspacePath = (String) arguments.get("workspacePath");
            
            Workspace workspace = new Workspace();
            workspace.setId("mcp-" + System.currentTimeMillis());
            workspace.setPath(workspacePath);
            workspace.setBranch("main");
            
            Scope scope = new Scope();
            
            MetricsResult result = cobolProvider.metrics(scope, workspace);
            return Map.of(
                "success", result.isSuccess(),
                "message", result.getMessage(),
                "metrics", result.getMetrics(),
                "details", result.getDetails()
            );
            
        } catch (Exception e) {
            return Map.of("success", false, "error", e.getMessage());
        }
    }
    
    private Object executeDiffTool(Map<String, Object> arguments) {
        try {
            String runId = (String) arguments.get("runId");
            
            Workspace workspace = new Workspace();
            workspace.setId("mcp-" + System.currentTimeMillis());
            workspace.setPath("/tmp");
            workspace.setBranch("main");
            
            DiffResult result = cobolProvider.diff(runId, workspace);
            return Map.of(
                "success", result.isSuccess(),
                "message", result.getMessage(),
                "diff", result.getUnifiedDiff(),
                "semantic", result.getSemanticDiff()
            );
            
        } catch (Exception e) {
            return Map.of("success", false, "error", e.getMessage());
        }
    }
}

