package org.shark.melian.mcp;

import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.shark.melian.service.TMDBService;
import org.shark.melian.service.AggregatedMovieService;
import org.shark.melian.model.MovieResult;
import org.shark.melian.model.ChunkDto;
import org.springframework.stereotype.Component;

import java.time.Instant;
import java.util.*;

@Component
@RequiredArgsConstructor
@Slf4j
public class PureMcpServer {

    private final TMDBService tmdbService;
    private final AggregatedMovieService aggregatedMovieService;
    private final ObjectMapper objectMapper = new ObjectMapper();

    public McpDto.InitializeResult initialize(McpDto.InitializeRequest request) {
        log.info("[PureMcpServer] Initializing MCP with new format: {}", request);

        return McpDto.InitializeResult.builder()
                .protocolVersion("2024-11-05")
                .serverInfo(McpDto.ServerInfo.builder()
                        .name("melian-movie-server")
                        .version("1.0.0")
                        .build())
                .capabilities(McpDto.ServerCapabilities.builder()
                        .logging(McpDto.LoggingCapability.builder().build())
                        .tools(McpDto.ToolsCapability.builder().listChanged(true).build())
                        .resources(McpDto.ResourcesCapability.builder()
                                .subscribe(true)
                                .listChanged(true)
                                .build())
                        .prompts(McpDto.PromptsCapability.builder().build())
                        .build())
                .build();
    }

    public McpDto.ToolsListResult listTools() {
        log.info("[PureMcpServer] Listando herramientas disponibles");

        List<McpDto.Tool> tools = Arrays.asList(
                McpDto.Tool.builder()
                        .name("search_movies")
                        .description("Search for movies across all available sources")
                        .inputSchema(createSearchSchema())
                        .build(),
                McpDto.Tool.builder()
                        .name("get_movie_chunks")
                        .description("Get movie chunks for RAG applications")
                        .inputSchema(createChunksSchema())
                        .build(),
                McpDto.Tool.builder()
                        .name("get_server_status")
                        .description("Get server status")
                        .inputSchema(new HashMap<>())
                        .build()
        );

        return McpDto.ToolsListResult.builder().tools(tools).build();
    }

    public McpDto.CallToolResult callTool(McpDto.CallToolRequest request) {
        log.info("[PureMcpServer] Llamando a herramienta: {} con argumentos: {}",
                request.getName(), request.getArguments());

        try {
            switch (request.getName()) {
                case "search_movies": {
                    String query = (String) request.getArguments().get("query");
                    Integer limit = (Integer) request.getArguments().getOrDefault("limit", 10);
                    if (query == null || query.isBlank()) {
                        return McpDto.CallToolResult.builder()
                                .isError(true)
                                .content(List.of(McpDto.ToolContent.builder()
                                        .type("text")
                                        .text("Query parameter is required")
                                        .build()))
                                .build();
                    }
                    List<MovieResult> movies = tmdbService.search(query, limit);
                    String text = "Found " + movies.size() + " movies for query '" + query + "'";
                    return McpDto.CallToolResult.builder()
                            .isError(false)
                            .content(List.of(McpDto.ToolContent.builder()
                                    .type("text")
                                    .text(text)
                                    .data(movies)
                                    .build()))
                            .build();
                }
                case "get_movie_chunks": {
                    String source = (String) request.getArguments().get("source");
                    Integer limit = (Integer) request.getArguments().getOrDefault("limit", 10);
                    List<ChunkDto> chunks = aggregatedMovieService.getMovieChunks(limit, null, null, null, null);
                    String text = "Retrieved " + chunks.size() + " chunks from source: " + source;
                    return McpDto.CallToolResult.builder()
                            .isError(false)
                            .content(List.of(McpDto.ToolContent.builder()
                                    .type("text")
                                    .text(text)
                                    .data(chunks)
                                    .build()))
                            .build();
                }
                case "get_server_status": {
                    McpDto.HealthStatus health = getHealth();
                    String text = "Server status: " + health.getStatus();
                    return McpDto.CallToolResult.builder()
                            .isError(false)
                            .content(List.of(McpDto.ToolContent.builder()
                                    .type("text")
                                    .text(text)
                                    .data(health)
                                    .build()))
                            .build();
                }
                default:
                    return McpDto.CallToolResult.builder()
                            .isError(true)
                            .content(List.of(McpDto.ToolContent.builder()
                                    .type("text")
                                    .text("Unknown tool: " + request.getName())
                                    .build()))
                            .build();
            }
        } catch (Exception e) {
            log.error("[PureMcpServer] Error al ejecutar herramienta: {}", e.getMessage(), e);

            return McpDto.CallToolResult.builder()
                    .isError(true)
                    .content(List.of(McpDto.ToolContent.builder()
                            .type("text")
                            .text("Error: " + e.getMessage())
                            .build()))
                    .build();
        }
    }

    public McpDto.ResourcesListResult listResources() {
        log.info("[PureMcpServer] Listando recursos disponibles");

        List<McpDto.Resource> resources = Arrays.asList(
                McpDto.Resource.builder()
                        .uri("melian://movies/sql")
                        .name("Películas SQL")
                        .description("Películas de la base de datos SQL")
                        .mimeType("application/json")
                        .build(),
                McpDto.Resource.builder()
                        .uri("melian://movies/mongo")
                        .name("Películas MongoDB")
                        .description("Películas de MongoDB")
                        .mimeType("application/json")
                        .build(),
                McpDto.Resource.builder()
                        .uri("melian://movies/tmdb")
                        .name("Películas TMDB")
                        .description("Películas de TMDB")
                        .mimeType("application/json")
                        .build()
        );

        return McpDto.ResourcesListResult.builder().resources(resources).build();
    }

    public McpDto.HealthStatus getHealth() {
        log.info("[PureMcpServer] Getting server health status");

        Map<String, Object> details = new HashMap<>();
        details.put("tmdbService", "OK");
        details.put("sqlService", "OK");
        details.put("mongoService", "OK");

        return McpDto.HealthStatus.builder()
                .status("OK")
                .details(details)
                .timestamp(Instant.now().toString())
                .build();
    }

    // Métodos auxiliares para crear esquemas
    private Object createSearchSchema() {
        Map<String, Object> schema = new HashMap<>();
        schema.put("type", "object");

        Map<String, Object> properties = new HashMap<>();

        Map<String, Object> queryProp = new HashMap<>();
        queryProp.put("type", "string");
        queryProp.put("description", "Consulta de búsqueda");
        properties.put("query", queryProp);

        Map<String, Object> limitProp = new HashMap<>();
        limitProp.put("type", "integer");
        limitProp.put("description", "Límite de resultados");
        properties.put("limit", limitProp);

        schema.put("properties", properties);
        schema.put("required", List.of("query"));

        return schema;
    }

    private Object createChunksSchema() {
        Map<String, Object> schema = new HashMap<>();
        schema.put("type", "object");

        Map<String, Object> properties = new HashMap<>();

        Map<String, Object> limitProp = new HashMap<>();
        limitProp.put("type", "integer");
        limitProp.put("description", "Límite de chunks");
        properties.put("limit", limitProp);

        Map<String, Object> filterProp = new HashMap<>();
        filterProp.put("type", "string");
        filterProp.put("description", "Filtro opcional");
        properties.put("filter", filterProp);

        schema.put("properties", properties);

        return schema;
    }

    public McpDto.ReadResourceResult readResource(McpDto.ReadResourceRequest request) {
        log.info("[PureMcpServer] Leyendo recurso: {}", request.getUri());

        try {
            String uri = request.getUri();
            List<ChunkDto> chunks = new ArrayList<>();

            if (uri.startsWith("movies/")) {
                String source = uri.substring("movies/".length());
                if ("all".equals(source)) {
                    chunks = aggregatedMovieService.getMovieChunks(10, null, null, null, null);
                } else if ("sql".equals(source)) {
                    // Obtener solo de SQL
                } else if ("mongo".equals(source)) {
                    // Obtener solo de MongoDB
                }
            }

            String content = objectMapper.writeValueAsString(chunks);

            return McpDto.ReadResourceResult.builder()
                    .contents(List.of(McpDto.ResourceContent.builder()
                            .uri(request.getUri())
                            .mimeType("application/json")
                            .text(content)
                            .build()))
                    .build();

        } catch (Exception e) {
            log.error("[PureMcpServer] Error reading resource: {}", e.getMessage(), e);
            throw new RuntimeException("Error reading resource: " + e.getMessage());
        }
    }

    // MCP ping method for compliance
    public McpDto.PingResult ping(McpDto.PingRequest request) {
        log.info("[PureMcpServer] Received ping request");
        return McpDto.PingResult.builder()
                .timestamp(Instant.now().toString())
                .message("pong")
                .build();
    }
}