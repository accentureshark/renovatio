package org.shark.renovatio.provider.jcl.infrastructure;

import org.shark.renovatio.provider.jcl.domain.JclMcpTool;
import org.shark.renovatio.provider.jcl.service.JclTranslationService;
import org.springframework.stereotype.Component;

import java.util.*;

/**
 * Exposes JCL conversion utilities as MCP tools.
 */
@Component
public class JclMcpToolsProvider {

    private final JclTranslationService translationService;

    public JclMcpToolsProvider(JclTranslationService translationService) {
        this.translationService = translationService;
    }

    /**
     * List available JCL tools.
     */
    public List<JclMcpTool> getJclTools() {
        return List.of(createConvertTool());
    }

    /**
     * Execute the given JCL tool.
     */
    public Object executeJclTool(String toolName, Map<String, Object> arguments) {
        if ("jcl.convert".equals(toolName)) {
            return executeConvert(arguments);
        }
        return Map.of("error", "Unknown JCL tool: " + toolName);
    }

    private JclMcpTool createConvertTool() {
        JclMcpTool tool = new JclMcpTool();
        tool.setName("jcl.convert");
        tool.setDescription("Convert JCL steps to shell scripts or CI workflows");

        Map<String, Object> schema = new HashMap<>();
        schema.put("type", "object");
        schema.put("properties", Map.of(
                "jcl", Map.of("type", "string", "description", "JCL content"),
                "target", Map.of("type", "string", "description", "shell or github-actions")
        ));
        schema.put("required", List.of("jcl", "target"));
        tool.setInputSchema(schema);
        return tool;
    }

    private Object executeConvert(Map<String, Object> arguments) {
        String jcl = (String) arguments.get("jcl");
        String target = (String) arguments.getOrDefault("target", "shell");
        String script;
        if ("github-actions".equalsIgnoreCase(target) || "ci".equalsIgnoreCase(target)) {
            script = translationService.toGithubActions(jcl);
        } else {
            script = translationService.toShellScript(jcl);
        }
        return Map.of(
                "success", true,
                "script", script
        );
    }
}
