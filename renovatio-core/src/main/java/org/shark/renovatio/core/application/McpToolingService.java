package org.shark.renovatio.core.application;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.shark.renovatio.core.mcp.McpPrompt;
import org.shark.renovatio.core.mcp.McpResource;
import org.shark.renovatio.core.mcp.McpTool;
import org.shark.renovatio.core.service.LanguageProviderRegistry;
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
    private final LanguageProviderRegistry providerRegistry;
    private final List<McpPrompt> prompts;
    private final List<McpResource> resources;

    public McpToolingService(LanguageProviderRegistry providerRegistry) {
        this.providerRegistry = providerRegistry;
        ObjectMapper mapper = new ObjectMapper()
            .configure(com.fasterxml.jackson.core.JsonParser.Feature.ALLOW_COMMENTS, true);
        try {
            var resource = new ClassPathResource("mcp-tooling.json");
            JsonNode root = mapper.readTree(resource.getInputStream());
            this.spec = root.path("spec").asText();
            
            this.prompts = createPrompts();
            this.resources = createResources();
        } catch (IOException e) {
            throw new RuntimeException("No se pudo leer mcp-tooling.json", e);
        }
    }

    /**
     * Get available MCP tools based on registered providers
     */
    public List<McpTool> getMcpTools() {
        return providerRegistry.generateMcpTools();
    }

    /**
     * Execute a tool using the provider registry
     */
    public Map<String, Object> executeTool(String toolName, Map<String, Object> arguments) {
        return providerRegistry.routeToolCall(toolName, arguments);
    }

    /**
     * Get server specification
     */
    public String getSpec() {
        return spec;
    }

    /**
     * Get prompts
     */
    public List<McpPrompt> getPrompts() {
        return prompts;
    }

    /**
     * Get specific prompt by name
     */
    public McpPrompt getPrompt(String name) {
        return prompts.stream()
                .filter(p -> p.getName().equals(name))
                .findFirst()
                .orElse(null);
    }

    /**
     * Get resources
     */
    public List<McpResource> getResources() {
        return resources;
    }

    /**
     * Get specific resource by URI
     */
    public McpResource getResource(String uri) {
        return resources.stream()
                .filter(r -> r.getUri().equals(uri))
                .findFirst()
                .orElse(null);
    }

    private List<McpPrompt> createPrompts() {
        List<McpPrompt> prompts = new ArrayList<>();

        List<McpPrompt.Message> formatMessages = new ArrayList<>();
        formatMessages.add(new McpPrompt.Message("user", "Provide code to format"));
        formatMessages.add(new McpPrompt.Message("assistant", "Here is the formatted code"));
        prompts.add(new McpPrompt("format-code", "Format source code", formatMessages));

        List<McpPrompt.Message> analyzeMessages = new ArrayList<>();
        analyzeMessages.add(new McpPrompt.Message("user", "Analyze this code structure"));
        analyzeMessages.add(new McpPrompt.Message("assistant", "Code analysis completed"));
        prompts.add(new McpPrompt("analyze-code", "Analyze code structure", analyzeMessages));

        List<McpPrompt.Message> generateStubMessages = new ArrayList<>();
        generateStubMessages.add(new McpPrompt.Message("user", "Generate Java stubs for this COBOL interface"));
        generateStubMessages.add(new McpPrompt.Message("assistant", "Java stubs generated"));
        prompts.add(new McpPrompt("generate-stubs", "Generate interface stubs", generateStubMessages));

        return prompts;
    }

    private List<McpResource> createResources() {
        List<McpResource> resources = new ArrayList<>();
        resources.add(new McpResource("file://welcome.txt", "Welcome", "text/plain",
                "Welcome to the Renovatio Multi-Language MCP server"));
        resources.add(new McpResource("file://architecture.txt", "Architecture", "text/plain",
                "Multi-language architecture with Provider SPI pattern"));
        resources.add(new McpResource("file://nql-syntax.txt", "NQL Syntax", "text/plain",
                "Natural Query Language syntax reference"));
        return resources;
    }
}
