package org.shark.renovatio.application;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.openrewrite.config.Environment;
import org.openrewrite.config.RecipeDescriptor;
import org.shark.renovatio.domain.RefactorRequest;
import org.shark.renovatio.domain.RefactorResponse;
import org.shark.renovatio.domain.Tool;
import org.springframework.core.io.ClassPathResource;
import org.springframework.stereotype.Service;

import java.io.IOException;
import java.util.List;
import java.util.stream.Collectors;

@Service
public class McpToolingService {
    private final String spec;
    private final List<Tool> tools;
    private final RefactorService refactorService;

    public McpToolingService(RefactorService refactorService) {
        this.refactorService = refactorService;
        ObjectMapper mapper = new ObjectMapper();
        try {
            var resource = new ClassPathResource("mcp-tooling.json");
            JsonNode root = mapper.readTree(resource.getInputStream());
            this.spec = root.path("spec").asText();
            Environment env = Environment.builder().scanRuntimeClasspath().build();
            this.tools = env.listRecipes().stream()
                    .map(this::toTool)
                    .collect(Collectors.toList());
        } catch (IOException e) {
            throw new RuntimeException("No se pudo leer mcp-tooling.json", e);
        }
    }

    public String getSpec() {
        return spec;
    }

    public List<Tool> getTools() {
        return tools;
    }

    private Tool toTool(RecipeDescriptor descriptor) {
        Tool tool = new Tool();
        tool.setName(descriptor.getName());
        tool.setDescription(descriptor.getDisplayName());
        tool.setCommand("openrewrite");
        return tool;
    }

    public RefactorResponse runTool(String name, RefactorRequest request) {
        request.setRecipe(name);
        return refactorService.refactorCode(request);
    }
}
