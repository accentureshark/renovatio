package org.shark.renovatio.application;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.shark.renovatio.domain.RefactorRequest;
import org.shark.renovatio.domain.RefactorResponse;
import org.shark.renovatio.domain.ToolDefinition;

import org.shark.renovatio.domain.mcp.McpPrompt;
import org.shark.renovatio.domain.mcp.McpResource;
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
    private final List<ToolDefinition> tools;
    private final List<McpTool> mcpTools;
    private final RefactorService refactorService;
    private final List<McpPrompt> prompts;
    private final List<McpResource> resources;

    public McpToolingService(RefactorService refactorService) {
        this.refactorService = refactorService;
        ObjectMapper mapper = new ObjectMapper();
        try {
            var resource = new ClassPathResource("mcp-tooling.json");
            JsonNode root = mapper.readTree(resource.getInputStream());
            this.spec = root.path("spec").asText();
            
            var toolsResource = new ClassPathResource("tools.json");
            ToolDefinition[] definitions = mapper.readValue(toolsResource.getInputStream(), ToolDefinition[].class);
            this.tools = java.util.Arrays.asList(definitions);

            this.mcpTools = createMcpTools();
            this.prompts = createPrompts();
            this.resources = createResources();
        } catch (IOException e) {
            throw new RuntimeException("No se pudo leer mcp-tooling.json", e);
        }
    }

    private List<McpTool> createMcpTools() {
        List<McpTool> mcpTools = new ArrayList<>();

        for (ToolDefinition tool : tools) {
            Map<String, Object> inputSchema = createInputSchema();
            McpTool mcpTool = new McpTool(tool.getName(), tool.getDescription(), inputSchema);
            mcpTools.add(mcpTool);
        }

        return mcpTools;
    }
    private List<McpPrompt> createPrompts() {
        List<McpPrompt> prompts = new ArrayList<>();

        List<McpPrompt.Message> formatMessages = new ArrayList<>();
        formatMessages.add(new McpPrompt.Message("user", "Provide Java code to format"));
        formatMessages.add(new McpPrompt.Message("assistant", "Here is the formatted code"));
        prompts.add(new McpPrompt("format-java", "Sample prompt for formatting Java code", formatMessages));

        List<McpPrompt.Message> migrateMessages = new ArrayList<>();
        migrateMessages.add(new McpPrompt.Message("user", "Upgrade this project to Java 17"));
        migrateMessages.add(new McpPrompt.Message("assistant", "The project has been updated to Java 17"));
        prompts.add(new McpPrompt("migrate-java17", "Prompt for migrating code to Java 17", migrateMessages));

        return prompts;
    }

    private List<McpResource> createResources() {
        List<McpResource> resources = new ArrayList<>();
        resources.add(new McpResource("file://welcome.txt", "Welcome", "text/plain",
                "Welcome to the Renovatio MCP server"));
        resources.add(new McpResource("file://usage.txt", "Usage", "text/plain",
                "Use tools, prompts and resources via MCP methods"));
        return resources;
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
    
    public String getSpec() {
        return spec;

    }

    public List<ToolDefinition> getTools() {
        return tools;
    }
    
    public List<McpTool> getMcpTools() {
        return mcpTools;
    }

    public List<McpPrompt> getPrompts() {
        return prompts;
    }

    public McpPrompt getPrompt(String name) {
        return prompts.stream()
                .filter(p -> p.getName().equals(name))
                .findFirst()
                .orElse(null);
    }

    public List<McpResource> getResources() {
        return resources;
    }

    public McpResource getResource(String uri) {
        return resources.stream()
                .filter(r -> r.getUri().equals(uri))
                .findFirst()
                .orElse(null);
    }


    public RefactorResponse runTool(String name, RefactorRequest request) {
        request.setRecipe(name);
        return refactorService.refactor(request);
    }
    
    public Map<String, Object> executeTool(String toolName, Map<String, Object> arguments) {
        String sourceCode = (String) arguments.get("sourceCode");
        String projectPath = (String) arguments.getOrDefault("projectPath", "");
        
        RefactorRequest request = new RefactorRequest();
        request.setSourceCode(sourceCode);
        request.setRecipe(toolName);
        
        RefactorResponse response = refactorService.refactor(request);
        
        Map<String, Object> result = new HashMap<>();
        result.put("type", "text");
        result.put("text", "Refactored Code:\n" + response.getRefactoredCode() + "\n\nMessage: " + response.getMessage());
        
        return result;
    }
}
