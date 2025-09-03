package org.shark.renovatio.application;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.shark.renovatio.domain.RefactorRequest;
import org.shark.renovatio.domain.RefactorResponse;
import org.shark.renovatio.domain.Tool;
import org.shark.renovatio.domain.mcp.McpTool;
import org.springframework.core.io.ClassPathResource;
import org.springframework.stereotype.Service;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@Service
public class McpToolingService {
    private final String spec;
    private final List<Tool> tools;
    private final List<McpTool> mcpTools;
    private final RefactorService refactorService;

    public McpToolingService(RefactorService refactorService) {
        this.refactorService = refactorService;
        ObjectMapper mapper = new ObjectMapper();
        try {
            var resource = new ClassPathResource("mcp-tooling.json");
            JsonNode root = mapper.readTree(resource.getInputStream());
            this.spec = root.path("spec").asText();
            
            // Create comprehensive set of OpenRewrite tools
            this.tools = createBasicTools();
            this.mcpTools = createMcpTools();
        } catch (IOException e) {
            throw new RuntimeException("No se pudo leer mcp-tooling.json", e);
        }
    }

    private List<Tool> createBasicTools() {
        List<Tool> basicTools = new ArrayList<>();
        
        // Java formatting and cleanup recipes
        addTool(basicTools, "org.openrewrite.java.format.AutoFormat", "Automatically format Java code");
        addTool(basicTools, "org.openrewrite.java.cleanup.UnnecessaryParentheses", "Remove unnecessary parentheses");
        addTool(basicTools, "org.openrewrite.java.cleanup.EmptyBlock", "Remove empty blocks");
        addTool(basicTools, "org.openrewrite.java.cleanup.ExplicitInitialization", "Remove explicit initialization of variables to default values");
        addTool(basicTools, "org.openrewrite.java.cleanup.FinalizePrivateFields", "Finalize private fields that are not reassigned");
        
        // Code improvement recipes
        addTool(basicTools, "org.openrewrite.java.cleanup.BigDecimalRoundingConstantsToEnums", "Replace BigDecimal rounding constants with enums");
        addTool(basicTools, "org.openrewrite.java.cleanup.BooleanChecksNotInverted", "Replace inverted boolean checks");
        addTool(basicTools, "org.openrewrite.java.cleanup.CaseInsensitiveComparisonsDoNotChangeCase", "Use case-insensitive comparison methods");
        addTool(basicTools, "org.openrewrite.java.cleanup.ChainStringBuilderAppendCalls", "Chain StringBuilder append calls");
        addTool(basicTools, "org.openrewrite.java.cleanup.CovariantEquals", "Use covariant equals");
        
        // Migration recipes
        addTool(basicTools, "org.openrewrite.java.migrate.Java8toJava11", "Migrate from Java 8 to Java 11");
        addTool(basicTools, "org.openrewrite.java.migrate.JavaVersion11", "Upgrade to Java 11");
        addTool(basicTools, "org.openrewrite.java.migrate.JavaVersion17", "Upgrade to Java 17");
        addTool(basicTools, "org.openrewrite.java.migrate.JavaVersion21", "Upgrade to Java 21");
        
        // Security recipes
        addTool(basicTools, "org.openrewrite.java.security.FindJdbcUrl", "Find JDBC URLs");
        addTool(basicTools, "org.openrewrite.java.security.FindSqlInjection", "Find potential SQL injection vulnerabilities");
        addTool(basicTools, "org.openrewrite.java.security.SecureRandomPrefersDefaultSeed", "Use SecureRandom with default seed");
        
        return basicTools;
    }
    
    private List<McpTool> createMcpTools() {
        List<McpTool> mcpTools = new ArrayList<>();
        
        for (Tool tool : tools) {
            Map<String, Object> inputSchema = createInputSchema();
            McpTool mcpTool = new McpTool(tool.getName(), tool.getDescription(), inputSchema);
            mcpTools.add(mcpTool);
        }
        
        return mcpTools;
    }
    
    private Map<String, Object> createInputSchema() {
        Map<String, Object> schema = new HashMap<>();
        schema.put("type", "object");
        
        Map<String, Object> properties = new HashMap<>();
        
        Map<String, Object> sourceCodeProperty = new HashMap<>();
        sourceCodeProperty.put("type", "string");
        sourceCodeProperty.put("description", "Java source code to refactor");
        properties.put("sourceCode", sourceCodeProperty);
        
        Map<String, Object> projectPathProperty = new HashMap<>();
        projectPathProperty.put("type", "string");
        projectPathProperty.put("description", "Path to the project (optional)");
        properties.put("projectPath", projectPathProperty);
        
        schema.put("properties", properties);
        schema.put("required", List.of("sourceCode"));
        
        return schema;
    }
    
    private void addTool(List<Tool> tools, String name, String description) {
        Tool tool = new Tool();
        tool.setName(name);
        tool.setDescription(description);
        tool.setCommand("openrewrite");
        tools.add(tool);
    }

    public String getSpec() {
        return spec;
    }

    public List<Tool> getTools() {
        return tools;
    }
    
    public List<McpTool> getMcpTools() {
        return mcpTools;
    }

    public RefactorResponse runTool(String name, RefactorRequest request) {
        request.setRecipe(name);
        return refactorService.refactorCode(request);
    }
    
    public Map<String, Object> executeTool(String toolName, Map<String, Object> arguments) {
        String sourceCode = (String) arguments.get("sourceCode");
        String projectPath = (String) arguments.getOrDefault("projectPath", "");
        
        RefactorRequest request = new RefactorRequest();
        request.setSourceCode(sourceCode);
        request.setRecipe(toolName);
        
        RefactorResponse response = refactorService.refactorCode(request);
        
        Map<String, Object> result = new HashMap<>();
        result.put("type", "text");
        result.put("text", "Refactored Code:\n" + response.getRefactoredCode() + "\n\nMessage: " + response.getMessage());
        
        return result;
    }
}
