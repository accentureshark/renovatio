package org.shark.melian.mcp;

import com.fasterxml.jackson.databind.JsonNode;
import org.springframework.stereotype.Service;
import org.shark.melian.service.AggregatedMovieService;
import org.shark.melian.model.ChunkDto;

import java.util.*;

/**
 * Minimal in-memory MCP service implementing core JSON-RPC methods.
 */
@Service
public class McpService {

    private boolean initialized = false;

    private final AggregatedMovieService aggregatedMovieService;

    public McpService(AggregatedMovieService aggregatedMovieService) {
        this.aggregatedMovieService = aggregatedMovieService;
    }

    /**
     * Dispatch a JSON-RPC method call.
     *
     * @param method method name
     * @param params parameters as JsonNode (may be null)
     * @return result object
     * @throws NoSuchMethodException when the method is unknown
     */
    public Object dispatch(String method, JsonNode params) throws Exception {
        if (method == null) {
            throw new NoSuchMethodException("Method must be provided");
        }
        switch (method) {
            case "initialize":
                return initialize();
            case "ping":
                return ping();
            case "tools/list":
                return listTools();
            case "tools/call":
                return callTool(params);
            case "resources/list":
                return listResourcesMcp(params);
            case "resources/read":
                return readResourceMcp(params);
            default:
                throw new NoSuchMethodException("Unknown method: " + method);
        }
    }

    private Map<String, Object> initialize() {
        initialized = true;
        Map<String, Object> capabilities = new HashMap<>();
        capabilities.put("tools", Map.of("listChanged", true));
        capabilities.put("resources", Map.of("listChanged", true));
        capabilities.put("prompts", Map.of());
        capabilities.put("logging", Map.of());
        capabilities.put("progress", true);

        Map<String, Object> serverInfo = Map.of(
                "name", "melian",
                "version", "0.1.0"
        );

        Map<String, Object> result = new HashMap<>();
        result.put("protocolVersion", "2024-11-05");
        result.put("serverInfo", serverInfo);
        result.put("capabilities", capabilities);
        return result;
    }

    private Map<String, Object> ping() {
        if (!initialized) {
            throw new IllegalStateException("Server not initialized");
        }
        return Map.of("ok", true);
    }

    private Map<String, Object> listTools() {
        // Tool real: find_movie_by_title
        Map<String, Object> findMovieTool = Map.of(
            "name", "find_movie_by_title",
            "description", "Busca películas por título (coincidencia parcial, insensible a mayúsculas)",
            "inputSchema", Map.of(
                "type", "object",
                "properties", Map.of(
                    "title", Map.of("type", "string", "description", "Título o parte del título a buscar")
                ),
                "required", List.of("title")
            ),
            "outputSchema", Map.of(
                "type", "object",
                "properties", Map.of(
                    "movies", Map.of(
                        "type", "array",
                        "items", Map.of(
                            "type", "object",
                            "properties", Map.of(
                                "id", Map.of("type", "string"),
                                "title", Map.of("type", "string"),
                                "year", Map.of("type", "integer"),
                                "rating", Map.of("type", "number"),
                                "genre", Map.of("type", "string"),
                                "overview", Map.of("type", "string")
                            )
                        )
                    )
                ),
                "required", List.of("movies")
            ),
            "example", Map.of(
                "name", "find_movie_by_title",
                "arguments", Map.of("title", "star wars"),
                "result", Map.of(
                    "movies", List.of(
                        Map.of("id", "1", "title", "Star Wars: A New Hope", "year", 1977, "rating", 8.6, "genre", "Sci-Fi", "overview", "Luke Skywalker joins the Rebel Alliance..."),
                        Map.of("id", "2", "title", "Star Wars: The Empire Strikes Back", "year", 1980, "rating", 8.7, "genre", "Sci-Fi", "overview", "Darth Vader pursues the Rebels...")
                    )
                )
            ),
            "category", "peliculas",
            "tags", List.of("busqueda", "titulo", "peliculas")
        );
        // Puedes agregar más tools reales aquí si las implementas
        List<Map<String, Object>> tools = List.of(findMovieTool, askDataTool());
        return Map.of("tools", tools);
    }

    private Map<String, Object> callTool(JsonNode params) {
        if (params == null) {
            throw new IllegalArgumentException("params required");
        }
        String name = optionalText(params.get("name"));
        if (name == null) {
            throw new IllegalArgumentException("name is required");
        }
        JsonNode arguments = params.get("arguments");
        if (arguments == null) {
            throw new IllegalArgumentException("arguments are required");
        }
        if ("find_movie_by_title".equals(name)) {
            String title = optionalText(arguments.get("title"));
            if (title == null || title.isBlank()) {
                throw new IllegalArgumentException("title is required");
            }
            // Use AggregatedMovieService to search movies by title (case-insensitive, partial match)
            int limit = 20; // default limit
            List<org.shark.melian.model.MovieResult> results = aggregatedMovieService.searchMovies(title, limit);
            List<Map<String, Object>> movies = new ArrayList<>();
            for (org.shark.melian.model.MovieResult movie : results) {
                Map<String, Object> movieMap = new HashMap<>();
                movieMap.put("id", null); // MovieResult does not provide id
                movieMap.put("title", movie.title());
                // Parse year from releaseDate if available
                Integer year = null;
                if (movie.releaseDate() != null && !movie.releaseDate().isBlank()) {
                    try {
                        year = Integer.parseInt(movie.releaseDate().substring(0, 4));
                    } catch (Exception ignored) {}
                }
                movieMap.put("year", year);
                movieMap.put("rating", movie.rating());
                movieMap.put("genre", null); // MovieResult does not provide genre
                movieMap.put("overview", movie.overview());
                movies.add(movieMap);
            }
            return Map.of("movies", movies);
        }
        if ("ask_data".equals(name)) {
            String question = optionalText(arguments.get("question"));
            if (question == null) {
                throw new IllegalArgumentException("question is required");
            }
            if (arguments.has("causeError") && arguments.get("causeError").asBoolean()) {
                throw new RuntimeException("Simulated internal error");
            }
            Map<String, Object> content = new HashMap<>();
            content.put("type", "text");
            content.put("text", "answer to " + question);
            return Map.of("content", List.of(content));
        }
        throw new IllegalArgumentException("Unknown tool: " + name);
    }

    private String optionalText(JsonNode node) {
        return node != null && !node.isNull() ? node.asText() : null;
    }

    public Map<String, Object> health() {
        return Map.of("status", "ok");
    }

    public Map<String, Object> listResources() {
        return Map.of("resources", Collections.emptyList());
    }

    // MCP-compliant: List resources (POST /mcp/resources/list)
    public Map<String, Object> listResourcesMcp(JsonNode params) {
        // Simulación: lista de recursos fijos
        List<Map<String, Object>> resources = List.of(
            Map.of("name", "movies", "type", "table", "description", "Películas disponibles"),
            Map.of("name", "chunks", "type", "table", "description", "Chunks de datos"),
            Map.of("name", "metadata", "type", "table", "description", "Metadatos de recursos")
        );
        // Filtros y paginación (simulado)
        int offset = 0;
        int limit = resources.size();
        if (params != null) {
            if (params.has("offset")) offset = params.get("offset").asInt(0);
            if (params.has("limit")) limit = params.get("limit").asInt(resources.size());
        }
        List<Map<String, Object>> paged = resources.subList(Math.min(offset, resources.size()), Math.min(offset+limit, resources.size()));
        return Map.of("resources", paged, "total", resources.size());
    }

    // MCP-compliant: Read resource (POST /mcp/resources/read)
    public Map<String, Object> readResourceMcp(JsonNode params) {
        String resource = params != null && params.has("resource") ? params.get("resource").asText() : null;
        if (!"movies".equals(resource)) {
            return Map.of("data", List.of(), "total", 0);
        }
        int offset = 0;
        int limit = 10;
        String filter = null;
        if (params != null) {
            if (params.has("offset")) offset = params.get("offset").asInt(0);
            if (params.has("limit")) limit = params.get("limit").asInt(10);
            if (params.has("filter")) filter = params.get("filter").asText();
        }
        // Call the real service
        List<ChunkDto> chunks = aggregatedMovieService.getMovieChunks(offset + limit, null, filter, null, null);
        // Paginate
        List<ChunkDto> paged = chunks.subList(Math.min(offset, chunks.size()), Math.min(offset + limit, chunks.size()));
        // Map to MCP data structure
        List<Map<String, Object>> data = new ArrayList<>();
        for (ChunkDto chunk : paged) {
            Map<String, Object> meta = chunk.getMetadata();
            Map<String, Object> row = new HashMap<>();
            row.put("id", chunk.getId());
            if (meta != null) {
                if (meta.get("title") != null) row.put("title", meta.get("title"));
                if (meta.get("release_date") != null) {
                    row.put("year", parseYear(meta.get("release_date")));
                }
                if (meta.get("rating") != null) row.put("rating", meta.get("rating"));
                if (meta.get("genre") != null) row.put("genre", meta.get("genre"));
                if (meta.get("overview") != null) row.put("overview", meta.get("overview"));
            }
            data.add(row);
        }
        return Map.of("data", data, "total", chunks.size());
    }

    private Integer parseYear(Object releaseDate) {
        if (releaseDate == null) return null;
        String s = releaseDate.toString();
        if (s.length() >= 4) {
            try {
                return Integer.parseInt(s.substring(0, 4));
            } catch (Exception ignored) {}
        }
        return null;
    }

    // Helper para tool ask_data
    private Map<String, Object> askDataTool() {
        return Map.of(
            "name", "ask_data",
            "description", "Pregunta datos de ejemplo (dummy)",
            "inputSchema", Map.of(
                "type", "object",
                "properties", Map.of(
                    "question", Map.of("type", "string", "description", "Pregunta a responder")
                ),
                "required", List.of("question")
            ),
            "outputSchema", Map.of(
                "type", "object",
                "properties", Map.of(
                    "content", Map.of("type", "string")
                ),
                "required", List.of("content")
            ),
            "example", Map.of(
                "name", "ask_data",
                "arguments", Map.of("question", "¿Cuál es la capital de Francia?"),
                "result", Map.of("content", "La capital de Francia es París.")
            ),
            "category", "demo",
            "tags", List.of("dummy", "ejemplo")
        );
    }
}
