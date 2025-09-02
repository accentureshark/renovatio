package org.shark.renovatio.application;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.shark.renovatio.domain.RefactorRequest;
import org.shark.renovatio.domain.RefactorResponse;
import org.shark.renovatio.domain.Tool;
import org.springframework.core.io.ClassPathResource;
import org.springframework.stereotype.Service;

import java.io.IOException;
import java.util.Arrays;
import java.util.List;

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
            Tool[] toolsArray = mapper.readValue(root.path("tools").traverse(), Tool[].class);
            this.tools = Arrays.asList(toolsArray);
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

    public RefactorResponse runTool(String name, RefactorRequest request) {
        if ("openrewrite".equalsIgnoreCase(name)) {
            return refactorService.refactorCode(request);
        }
        return new RefactorResponse(request.getSourceCode(), "Tool no soportado: " + name);
    }
}
