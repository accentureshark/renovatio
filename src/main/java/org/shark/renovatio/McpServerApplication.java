package org.shark.renovatio;

import io.swagger.v3.oas.annotations.OpenAPIDefinition;
import io.swagger.v3.oas.annotations.info.Info;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.CommandLineRunner;
import org.springframework.context.annotation.Bean;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.beans.factory.annotation.Autowired;
import org.shark.renovatio.application.McpToolingService;
import org.shark.renovatio.domain.Tool;

@SpringBootApplication
@OpenAPIDefinition(info = @Info(title = "Renovatio API", version = "1.0", description = "API para refactorización con OpenRewrite"))
public class McpServerApplication {
    public static void main(String[] args) {
        SpringApplication.run(McpServerApplication.class, args);
    }

    @Bean
    public CommandLineRunner printToolsAndUrls(
            @Autowired McpToolingService mcpToolingService,
            @Value("${server.port:8181}") int port) {
        return args -> {
            System.out.println("\n================ MCP Server iniciado ================");
            System.out.println("Tools publicadas:");
            for (Tool tool : mcpToolingService.getTools()) {
                System.out.printf("- %s: %s\n", tool.getName(), tool.getDescription());
            }
            System.out.println("\nURLs de conexión para configurar el cliente:");
            System.out.printf("- Lista de tools:        http://localhost:%d/mcp/tools\n", port);
            System.out.printf("- Ejecutar tool:         http://localhost:%d/mcp/run/{toolName}\n", port);
            System.out.printf("- Especificación MCP:    http://localhost:%d/mcp/spec\n", port);
            System.out.printf("- Swagger UI:            http://localhost:%d/swagger-ui.html\n", port);
            System.out.println("====================================================\n");
        };
    }
}
