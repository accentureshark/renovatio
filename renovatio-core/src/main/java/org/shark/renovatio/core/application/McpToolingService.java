package org.shark.renovatio.core.application;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import jakarta.annotation.PostConstruct;
import org.shark.renovatio.core.mcp.McpPrompt;
import org.shark.renovatio.core.mcp.McpResource;
import org.shark.renovatio.core.mcp.McpTool;
import org.shark.renovatio.core.service.LanguageProviderRegistry;
import org.springframework.core.io.ClassPathResource;
import org.springframework.stereotype.Service;
import org.springframework.context.event.EventListener;
import org.springframework.boot.context.event.ApplicationReadyEvent;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@Service
public class McpToolingService {
    private static final Logger logger = LoggerFactory.getLogger(McpToolingService.class);
    private final String spec;
    private final LanguageProviderRegistry providerRegistry;
    private final List<McpPrompt> prompts;
    private final List<McpResource> resources;

    public McpToolingService(LanguageProviderRegistry providerRegistry) {
        this.providerRegistry = providerRegistry;
        ObjectMapper mapper = new ObjectMapper();
        try {
            var resource = new ClassPathResource("mcp-tooling.json", McpToolingService.class.getClassLoader());
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
        List<McpTool> tools = providerRegistry.generateMcpTools();
        logger.info("DEBUG getMcpTools: {}", tools);
        return tools;
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

    /**
     * Read file content from workspace
     */
    public String readFileContent(String path) throws IOException {
        java.nio.file.Path filePath = java.nio.file.Paths.get(path);
        return java.nio.file.Files.readString(filePath);
    }

    /**
     * Write file content to workspace
     */
    public void writeFileContent(String path, String content) throws IOException {
        java.nio.file.Path filePath = java.nio.file.Paths.get(path);
        java.nio.file.Files.writeString(filePath, content);
    }

    /**
     * List workspace root directories/files
     */
    public List<String> listWorkspaces() {
        // Example: list directories in current working directory
        List<String> workspaces = new ArrayList<>();
        java.nio.file.Path root = java.nio.file.Paths.get("");
        try (var stream = java.nio.file.Files.list(root)) {
            stream.filter(java.nio.file.Files::isDirectory)
                  .forEach(dir -> workspaces.add(dir.toString()));
        } catch (IOException e) {
            // Log and return empty list
        }
        return workspaces;
    }

    /**
     * Describe workspace by id (directory name)
     */
    public Map<String, Object> describeWorkspace(String workspaceId) {
        Map<String, Object> info = new HashMap<>();
        java.nio.file.Path dir = java.nio.file.Paths.get(workspaceId);
        info.put("id", workspaceId);
        info.put("exists", java.nio.file.Files.exists(dir));
        info.put("isDirectory", java.nio.file.Files.isDirectory(dir));
        try {
            info.put("files", java.nio.file.Files.list(dir)
                .map(java.nio.file.Path::toString)
                .toArray(String[]::new));
        } catch (IOException e) {
            info.put("files", new String[0]);
        }
        return info;
    }

    /**
     * Get tool by name
     */
    public McpTool getTool(String toolName) {
        return getMcpTools().stream()
            .filter(tool -> toolName.equals(tool.getName()))
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

    /**
     * Log all registered language providers and generated MCP tools in a unified, descriptive format.
     */
    public void logProvidersAndTools() {
        logger.info("================ Renovatio MCP Server Started ================");
        logger.info("Registered Language Providers:");
        var providers = providerRegistry.getAllProviders();
        if (providers.isEmpty()) {
            logger.warn("No language providers registered.");
        } else {
            for (var provider : providers) {
                logger.info("- {}: {}", provider.language(), provider.capabilities());
            }
        }
        logger.info("");
        logger.info("Generated MCP Tools:");
        var tools = getMcpTools();
        if (tools.isEmpty()) {
            logger.warn("No MCP tools generated.");
        } else {
            for (var tool : tools) {
                logger.info("- {}: {}", tool.getName(), tool.getDescription());
            }
        }
        logger.info("===============================================================");
    }

    @EventListener(ApplicationReadyEvent.class)
    public void logAvailableToolsOnApplicationReady() {
        List<McpTool> tools = getMcpTools();
        if (tools == null || tools.isEmpty()) {
            logger.warn("No MCP Tools available on server startup. Check provider registration.");
        } else {
            logger.info("MCP Tools available on server startup:");
            for (McpTool tool : tools) {
                logger.info("- {}: {}", tool.getName(), tool.getDescription());
            }
        }
    }
}
