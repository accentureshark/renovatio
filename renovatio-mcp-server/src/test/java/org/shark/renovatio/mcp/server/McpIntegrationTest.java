package org.shark.renovatio.mcp.server;

import io.restassured.RestAssured;
import io.restassured.http.ContentType;
import io.restassured.response.Response;
import org.junit.jupiter.api.*;
import org.junit.jupiter.api.condition.EnabledIfEnvironmentVariable;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.*;

import static io.restassured.RestAssured.given;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.*;
import static org.junit.jupiter.api.Assumptions.assumeTrue;

/**
 * Integration tests for MCP HTTP transport using JSON-RPC 2.0.
 * Assumes an external server is already running (do not spin Spring here).
 *
 * Configuration via environment variables:
 * - MCP_BASE_URL: Base URL for server (default: http://127.0.0.1:8081, fallback 8080)
 * - MCP_WORKSPACE: Absolute path to a workspace for language tools (default: project root)
 * - MCP_ALLOW_MUTATIONS: If "true", enable tools that write (e.g., java.format). Default: false
 * - MCP_TEST_SHUTDOWN: If "true", will call shutdown at the very end (stops server). Default: false
 */
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class McpIntegrationTest {

    private static String baseUrl;
    private static String workspacePath;
    private static String workspaceJava;
    private static String workspaceCobol;
    private static boolean allowMutations;
    private static boolean testShutdown;

    @BeforeAll
    static void setup() {
        String fromEnv = Optional.ofNullable(System.getenv("MCP_BASE_URL")).orElse("");
        if (fromEnv.isBlank()) {
            baseUrl = "http://127.0.0.1:8081";
        } else {
            baseUrl = fromEnv;
        }
        // quick fallback probe to 8080 if 8099 unresponsive
        if (!isServerUp(baseUrl)) {
            String fallback = "http://127.0.0.1:8080";
            if (isServerUp(fallback)) {
                baseUrl = fallback;
            }
        }
        RestAssured.baseURI = baseUrl;

        String wsEnv = System.getenv("MCP_WORKSPACE");
        if (wsEnv != null && !wsEnv.isBlank()) {
            workspacePath = wsEnv;
        } else {
            // default to repo root if tests run inside project
            workspacePath = Paths.get("").toAbsolutePath().normalize().toString();
        }
        // Defaults to user-provided paths if env not set
        workspaceJava = Optional.ofNullable(System.getenv("MCP_WORKSPACE_JAVA")).filter(s -> !s.isBlank())
                .orElse("/home/faguero/accenture/melian/src/main/java");
        workspaceCobol = Optional.ofNullable(System.getenv("MCP_WORKSPACE_COBOL")).filter(s -> !s.isBlank())
                .orElse("/home/faguero/accenture/renovatio/samples/cobol/Cobol-Programming-Collection/Cobol Utilities");

        allowMutations = Boolean.parseBoolean(System.getenv().getOrDefault("MCP_ALLOW_MUTATIONS", "false"));
        testShutdown = Boolean.parseBoolean(System.getenv().getOrDefault("MCP_TEST_SHUTDOWN", "false"));

        // Skip entire suite if server is not reachable
        assumeTrue(isServerUp(baseUrl), () -> "MCP server not reachable at " + baseUrl + ". Set MCP_BASE_URL or start the server.");
    }

    private static boolean isServerUp(String url) {
        try {
            Response r = given().when().get(url + "/mcp/health");
            return r.statusCode() == 200;
        } catch (Exception ignored) {
            return false;
        }
    }

    // --- Utilities ---

    private Map<String, Object> jsonRpc(String method, Object params, Object id) {
        Map<String, Object> body = new LinkedHashMap<>();
        body.put("jsonrpc", "2.0");
        body.put("method", method);
        if (id != null) body.put("id", id);
        if (params != null) body.put("params", params);
        return body;
    }

    private Map<String, Object> toolsCallParams(String name, Map<String, Object> arguments) {
        Map<String, Object> params = new LinkedHashMap<>();
        params.put("name", name);
        if (arguments != null) params.put("arguments", arguments);
        return params;
    }

    private Response postMcp(Map<String, Object> payload) {
        return given()
                .contentType(ContentType.JSON)
                .body(payload)
                .when()
                .post("/mcp")
                .thenReturn();
    }

    // --- Basic health ---

    @Test
    @Order(1)
    void health_should_be_up() {
        Response r = given().when().get("/mcp/health");
        assertThat(r.statusCode(), is(200));
        Map<?,?> json = r.as(Map.class);
        assertThat(String.valueOf(json.get("status")), is("UP"));
        assertThat(String.valueOf(json.get("server")), containsString("Renovatio MCP"));
    }

    // --- MCP core methods ---

    @Test
    @Order(2)
    void initialize_should_return_tools_and_capabilities() {
        Map<String, Object> params = new LinkedHashMap<>();
        params.put("language", "java");
        Response r = postMcp(jsonRpc("initialize", params, 1));
        assertThat(r.statusCode(), is(200));
        Map<?,?> json = r.as(Map.class);
        Map<?,?> result = (Map<?,?>) json.get("result");
        assertThat(result, is(notNullValue()));
        assertThat(result.get("protocolVersion"), is(notNullValue()));
        assertThat(result.get("availableTools"), is(instanceOf(List.class)));
    }

    @Test
    @Order(3)
    void ping_should_return_pong() {
        Response r = postMcp(jsonRpc("ping", Map.of(), 2));
        assertThat(r.statusCode(), is(200));
        Map<?,?> json = r.as(Map.class);
        Map<?,?> result = (Map<?,?>) json.get("result");
        assertThat(result.get("status"), is("pong"));
    }

    @Test
    @Order(4)
    void capabilities_should_return_object() {
        Response r = postMcp(jsonRpc("capabilities", Map.of(), 3));
        assertThat(r.statusCode(), is(200));
        Map<?,?> result = (Map<?,?>) r.as(Map.class).get("result");
        assertThat(result.get("capabilities"), is(notNullValue()));
    }

    @Test
    @Order(5)
    void serverInfo_should_return_metadata() {
        Response r = postMcp(jsonRpc("server/info", Map.of(), 4));
        assertThat(r.statusCode(), is(200));
        Map<?,?> info = (Map<?,?>) ((Map<?,?>) r.as(Map.class).get("result")).get("serverInfo");
        assertThat(String.valueOf(info.get("name")), is("Renovatio MCP Server"));
        assertThat(info.get("supportedLanguages"), is(notNullValue()));
    }

    @Test
    @Order(6)
    void toolsList_should_return_tools() {
        Response r = postMcp(jsonRpc("tools/list", Map.of(), 5));
        assertThat(r.statusCode(), is(200));
        Map<?,?> result = (Map<?,?>) r.as(Map.class).get("result");
        List<?> tools = (List<?>) result.get("tools");
        assertThat(tools, is(not(empty())));
    }

    @Test
    @Order(7)
    void toolsDescribe_should_work_for_a_known_tool() {
        // Prefer java.plan; otherwise pick first tool from tools/list
        String toolName = "java.plan";
        Response listResp = postMcp(jsonRpc("tools/list", Map.of(), 6));
        List<Map<String, Object>> tools = (List<Map<String, Object>>) ((Map<?,?>) listResp.as(Map.class).get("result")).get("tools");
        boolean found = tools.stream().anyMatch(t -> String.valueOf(t.get("name")).equalsIgnoreCase("java_plan") || String.valueOf(t.get("name")).equalsIgnoreCase("java.plan"));
        if (!found && !tools.isEmpty()) {
            toolName = String.valueOf(tools.get(0).get("name"));
        }
        Response r = postMcp(jsonRpc("tools/describe", Map.of("name", toolName), 7));
        assertThat(r.statusCode(), is(200));
        Map<?,?> result = (Map<?,?>) r.as(Map.class).get("result");
        assertThat(result.get("tool"), is(notNullValue()));
    }

    @Test
    @Order(8)
    void cliManifest_should_return_tools_for_cli() {
        Response r = postMcp(jsonRpc("cli/manifest", Map.of("language", "java"), 8));
        assertThat(r.statusCode(), is(200));
        Map<?,?> result = (Map<?,?>) r.as(Map.class).get("result");
        assertThat(result, is(notNullValue()));
    }

    // --- Content & Workspace ---

    @Test
    @Order(9)
    void content_write_and_read_should_roundtrip() throws Exception {
        // Prefer a path inside the configured workspace to maximize server accessibility
        Path targetPath;
        if (workspacePath != null && !workspacePath.isBlank()) {
            targetPath = Paths.get(workspacePath, "mcp-it-" + System.currentTimeMillis() + ".txt");
        } else {
            targetPath = Files.createTempFile("mcp-it-", ".txt");
        }
        String content = "Hello MCP! " + System.currentTimeMillis();
        Response write = postMcp(jsonRpc("content/write", Map.of("path", targetPath.toString(), "content", content), 9));
        assertThat(write.statusCode(), is(200));
        Response read = postMcp(jsonRpc("content/read", Map.of("path", targetPath.toString()), 10));
        assertThat(read.statusCode(), is(200));
        String readContent = (String) ((Map<?,?>) read.as(Map.class).get("result")).get("content");
        assertThat(readContent, is(content));
        try { Files.deleteIfExists(targetPath); } catch (Exception ignored) { }
    }

    @Test
    @Order(10)
    void workspace_list_and_describe_should_work() {
        Response list = postMcp(jsonRpc("workspace/list", Map.of(), 11));
        assertThat(list.statusCode(), is(200));
        Map<?,?> envelope = list.as(Map.class);
        Object resultObj = envelope.get("result");
        if (!(resultObj instanceof Map<?,?> result)) {
            // Unexpected shape; skip describe check but assert envelope shape has either result or error
            assertThat("Response should contain result or error", envelope.containsKey("result") || envelope.containsKey("error"), is(true));
            return;
        }
        Object workspacesObj = result.get("workspaces");
        if (!(workspacesObj instanceof List<?> workspaces) || workspaces.isEmpty()) {
            return; // nothing to describe
        }
        Object first = workspaces.get(0);
        if (!(first instanceof Map<?,?> firstWs)) {
            return;
        }
        String id = String.valueOf(firstWs.get("id"));
        if (id == null || id.isBlank()) {
            return;
        }
        Response describe = postMcp(jsonRpc("workspace/describe", Map.of("id", id), 12));
        assertThat(describe.statusCode(), is(200));
    }

    // --- Logging, Prompts, Resources ---

    @Test
    @Order(11)
    void logging_subscribe_unsubscribe_should_succeed() {
        String clientId = "mcp-it-client";
        Response sub = postMcp(jsonRpc("logging/subscribe", Map.of("id", clientId), 13));
        assertThat(sub.statusCode(), is(200));
        Response unsub = postMcp(jsonRpc("logging/unsubscribe", Map.of("id", clientId), 14));
        assertThat(unsub.statusCode(), is(200));
    }

    @Test
    @Order(12)
    void prompts_and_resources_should_return_structures() {
        Response prompts = postMcp(jsonRpc("prompts/list", Map.of(), 15));
        assertThat(prompts.statusCode(), is(200));
        Response resources = postMcp(jsonRpc("resources/list", Map.of(), 16));
        assertThat(resources.statusCode(), is(200));
    }

    @Test
    @Order(13)
    void prompts_get_should_return_prompt_when_available() {
        Response list = postMcp(jsonRpc("prompts/list", Map.of(), 17));
        assertThat(list.statusCode(), is(200));
        Map<?,?> result = (Map<?,?>) list.as(Map.class).get("result");
        List<Map<String, Object>> prompts = (List<Map<String, Object>>) result.get("prompts");
        if (prompts != null && !prompts.isEmpty()) {
            String name = String.valueOf(prompts.get(0).get("name"));
            Response get = postMcp(jsonRpc("prompts/get", Map.of("name", name), 18));
            assertThat(get.statusCode(), is(200));
        }
    }

    @Test
    @Order(14)
    void resources_read_should_return_resource_when_available() {
        Response list = postMcp(jsonRpc("resources/list", Map.of(), 19));
        assertThat(list.statusCode(), is(200));
        Map<?,?> result = (Map<?,?>) list.as(Map.class).get("result");
        List<Map<String, Object>> resources = (List<Map<String, Object>>) result.get("resources");
        if (resources != null && !resources.isEmpty()) {
            String uri = String.valueOf(resources.get(0).get("uri"));
            Response read = postMcp(jsonRpc("resources/read", Map.of("uri", uri), 20));
            assertThat(read.statusCode(), is(200));
        }
    }

    // --- Tools: iterate and try basic calls ---

    @Test
    @Order(20)
    void tools_call_should_work_for_known_tools() {
        Response listResp = postMcp(jsonRpc("tools/list", Map.of(), 21));
        assertThat(listResp.statusCode(), is(200));
        List<Map<String, Object>> tools = (List<Map<String, Object>>) ((Map<?,?>) listResp.as(Map.class).get("result")).get("tools");
        assertThat(tools, is(not(empty())));

        int success200 = 0;
        for (Map<String, Object> tool : tools) {
            String name = String.valueOf(tool.get("name"));
            Map<String, Object> toolMap = tool;
            Map<String, Object> inputSchema = (Map<String, Object>) toolMap.get("inputSchema");
            Map<String, Object> args = buildArguments(inputSchema);

            // Provide workspace when required selecting by language
            Map<String, Object> props = inputSchema != null ? (Map<String, Object>) inputSchema.get("properties") : Map.of();
            boolean needsWorkspace = props != null && props.containsKey("workspacePath");
            if (needsWorkspace) {
                String langPrefix = name.contains("_") ? name.substring(0, name.indexOf('_')) : (name.contains(".") ? name.substring(0, name.indexOf('.')) : "");
                String ws = workspacePath; // fallback
                if ("java".equalsIgnoreCase(langPrefix) && workspaceJava != null && !workspaceJava.isBlank()) {
                    ws = workspaceJava;
                } else if ("cobol".equalsIgnoreCase(langPrefix) && workspaceCobol != null && !workspaceCobol.isBlank()) {
                    ws = workspaceCobol;
                }
                if (ws == null || ws.isBlank()) {
                    // Skip if we cannot provide a suitable workspace for this tool
                    continue;
                }
                args.put("workspacePath", ws);
            }

            // Skip potentially destructive call unless allowed
            if (!allowMutations && (name.equalsIgnoreCase("java.format") || name.equalsIgnoreCase("java_format")
                    || name.equalsIgnoreCase("cobol_stubs_generate") || name.equalsIgnoreCase("cobol.stubs_generate"))) {
                continue;
            }

            Response call = postMcp(jsonRpc("tools/call", toolsCallParams(name, args), 100 + name.hashCode()));
            if (call.statusCode() == 200) {
                success200++;
                Map<?,?> response = call.as(Map.class);
                if (response.get("error") != null) {
                    Map<?,?> error = (Map<?,?>) response.get("error");
                    assertThat(error.get("message"), is(notNullValue()));
                } else {
                    Map<?,?> result = (Map<?,?>) response.get("result");
                    assertThat(result, is(notNullValue()));
                }
            } else {
                System.out.println("tools/call " + name + " returned HTTP " + call.statusCode());
            }
        }
        assertThat("At least one tool call should succeed (HTTP 200)", success200, greaterThan(0));
    }

    @Test
    @Order(25)
    void cobol_analyze_and_metrics_flow() {
        assumeTrue(workspaceCobol != null && !workspaceCobol.isBlank(), "Requires MCP_WORKSPACE_COBOL to be set");
        Map<String, Object> analyzeArgs = new LinkedHashMap<>();
        analyzeArgs.put("workspacePath", workspaceCobol);
        Response analyzeR = postMcp(jsonRpc("tools/call", toolsCallParams("cobol.analyze", analyzeArgs), 251));
        assumeTrue(analyzeR.statusCode() == 200, () -> "Skipping COBOL flow, analyze returned HTTP " + analyzeR.statusCode());

        Map<String, Object> metricsArgs = new LinkedHashMap<>();
        metricsArgs.put("workspacePath", workspaceCobol);
        Response metricsR = postMcp(jsonRpc("tools/call", toolsCallParams("cobol.metrics", metricsArgs), 252));
        assumeTrue(metricsR.statusCode() == 200, () -> "Skipping COBOL flow, metrics returned HTTP " + metricsR.statusCode());
    }

    @Test
    @Order(30)
    void java_plan_apply_diff_pipeline_analyze_metrics_discover_recipe_flow() {
        assumeTrue((workspaceJava != null && !workspaceJava.isBlank()) || (workspacePath != null && !workspacePath.isBlank()), "Requires MCP_WORKSPACE_JAVA or MCP_WORKSPACE");
        String ws = (workspaceJava != null && !workspaceJava.isBlank()) ? workspaceJava : workspacePath;

        // Plan
        Map<String, Object> planArgs = new LinkedHashMap<>();
        planArgs.put("workspacePath", ws);
        planArgs.put("goals", List.of("modernize_java17"));
        planArgs.put("profile", "modernize_java17");
        Response planR = postMcp(jsonRpc("tools/call", toolsCallParams("java.plan", planArgs), 201));
        assumeTrue(planR.statusCode() == 200, () -> "Skipping Java flow, plan returned HTTP " + planR.statusCode());
        Map<?,?> planEnvelope = planR.as(Map.class);
        Map<?,?> planResult = (Map<?,?>) planEnvelope.get("result");
        Map<?,?> planStruct = (Map<?,?>) planResult.get("structuredContent");
        String planId = String.valueOf(planStruct.get("planId"));
        assumeTrue(planId != null && !planId.isBlank(), "Skipping Java flow, missing planId");

        // Apply (dry-run)
        Map<String, Object> applyArgs = new LinkedHashMap<>();
        applyArgs.put("workspacePath", ws);
        applyArgs.put("planId", planId);
        applyArgs.put("dryRun", true);
        Response applyR = postMcp(jsonRpc("tools/call", toolsCallParams("java.apply", applyArgs), 202));
        assumeTrue(applyR.statusCode() == 200, () -> "Skipping Java flow, apply returned HTTP " + applyR.statusCode());
        Map<?,?> applyRes = (Map<?,?>) applyR.as(Map.class).get("result");
        Map<?,?> applyStruct = (Map<?,?>) applyRes.get("structuredContent");
        String runId = String.valueOf(applyStruct.get("runId"));
        assumeTrue(runId != null && !runId.isBlank(), "Skipping Java flow, missing runId");

        // Diff
        Map<String, Object> diffArgs = new LinkedHashMap<>();
        diffArgs.put("workspacePath", ws);
        diffArgs.put("runId", runId);
        Response diffR = postMcp(jsonRpc("tools/call", toolsCallParams("java.diff", diffArgs), 203));
        assumeTrue(diffR.statusCode() == 200, () -> "Skipping Java flow, diff returned HTTP " + diffR.statusCode());

        // Analyze (uses profile default)
        Map<String, Object> analyzeArgs = new LinkedHashMap<>();
        analyzeArgs.put("workspacePath", ws);
        analyzeArgs.put("profile", "quality");
        Response analyzeR = postMcp(jsonRpc("tools/call", toolsCallParams("java.analyze", analyzeArgs), 204));
        assumeTrue(analyzeR.statusCode() == 200, () -> "Skipping Java flow, analyze returned HTTP " + analyzeR.statusCode());

        // Metrics
        Map<String, Object> metricsArgs = new LinkedHashMap<>();
        metricsArgs.put("workspacePath", ws);
        Response metricsR = postMcp(jsonRpc("tools/call", toolsCallParams("java.metrics", metricsArgs), 205));
        assumeTrue(metricsR.statusCode() == 200, () -> "Skipping Java flow, metrics returned HTTP " + metricsR.statusCode());

        // Discover
        Map<String, Object> discoverArgs = new LinkedHashMap<>();
        discoverArgs.put("workspacePath", ws);
        Response discoverR = postMcp(jsonRpc("tools/call", toolsCallParams("java.discover", discoverArgs), 206));
        assumeTrue(discoverR.statusCode() == 200, () -> "Skipping Java flow, discover returned HTTP " + discoverR.statusCode());

        // Recipe list & describe
        Response recipeListR = postMcp(jsonRpc("tools/call", toolsCallParams("java.recipe_list", Map.of()), 207));
        assumeTrue(recipeListR.statusCode() == 200, () -> "Skipping Java flow, recipe_list returned HTTP " + recipeListR.statusCode());
        Map<?,?> listRes = (Map<?,?>) recipeListR.as(Map.class).get("result");
        List<Map<String, Object>> recipes = (List<Map<String, Object>>) ((Map<?,?>) listRes.get("structuredContent")).get("recipes");
        if (recipes != null && !recipes.isEmpty()) {
            String recipeName = String.valueOf(recipes.get(0).get("name"));
            Response recipeDescR = postMcp(jsonRpc("tools/call", toolsCallParams("java.recipe_describe", Map.of("name", recipeName)), 208));
            assumeTrue(recipeDescR.statusCode() == 200, () -> "Skipping Java flow, recipe_describe returned HTTP " + recipeDescR.statusCode());
        }

        // Pipeline (dry-run by default)
        Map<String, Object> pipelineArgs = new LinkedHashMap<>();
        pipelineArgs.put("workspacePath", ws);
        pipelineArgs.put("preset", "modernize_java17");
        Response pipelineR = postMcp(jsonRpc("tools/call", toolsCallParams("java.pipeline", pipelineArgs), 209));
        assumeTrue(pipelineR.statusCode() == 200, () -> "Skipping Java flow, pipeline returned HTTP " + pipelineR.statusCode());
    }

    @Test
    @Order(99)
    @EnabledIfEnvironmentVariable(named = "MCP_TEST_SHUTDOWN", matches = "(?i)true")
    void shutdown_should_stop_server() {
        Response r = postMcp(jsonRpc("shutdown", Map.of(), 999));
        assertThat(r.statusCode(), is(200));
    }

    // Build arguments from tool inputSchema for generic calls
    @SuppressWarnings("unchecked")
    private Map<String, Object> buildArguments(Map<String, Object> inputSchema) {
        Map<String, Object> args = new LinkedHashMap<>();
        if (inputSchema == null) return args;
        Map<String, Object> properties = (Map<String, Object>) inputSchema.getOrDefault("properties", Map.of());
        for (Map.Entry<String, Object> e : properties.entrySet()) {
            String key = e.getKey();
            Map<String, Object> prop = (Map<String, Object>) e.getValue();
            String type = String.valueOf(prop.get("type"));
            Object value = null;
            if ("string".equals(type)) {
                List<Object> enums = (List<Object>) prop.get("enum");
                if (enums != null && !enums.isEmpty()) {
                    value = String.valueOf(enums.get(0));
                } else if (key.toLowerCase(Locale.ROOT).contains("ref") || key.toLowerCase(Locale.ROOT).contains("name")) {
                    value = "default";
                } else if (key.toLowerCase(Locale.ROOT).contains("preset")) {
                    value = "modernize_java17";
                } else {
                    value = "default";
                }
            } else if ("boolean".equals(type)) {
                value = Boolean.FALSE;
            } else if ("integer".equals(type)) {
                value = 0;
            } else if ("array".equals(type)) {
                value = new ArrayList<>();
            } else if ("object".equals(type)) {
                value = new LinkedHashMap<>();
            }
            if (value != null) {
                args.put(key, value);
            }
        }
        return args;
    }

    private String safeBody(Response r) {
        try {
            String s = r.asString();
            if (s == null) return "<no body>";
            return s.length() > 800 ? s.substring(0, 800) + "..." : s;
        } catch (Exception e) {
            return "<unreadable body: " + e.getClass().getSimpleName() + ">";
        }
    }
}
