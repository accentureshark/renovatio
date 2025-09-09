package org.shark.renovatio.client;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import okhttp3.*;
import java.io.IOException;
import java.util.Scanner;

public class RenovatioMcpClient {
    private static final String SERVER_URL = "http://localhost:8181/";
    private static final ObjectMapper mapper = new ObjectMapper();
    private static final OkHttpClient client = new OkHttpClient();

    public static void main(String[] args) throws IOException {
        Scanner scanner = new Scanner(System.in);
        String serverUrl = SERVER_URL;
        if (args.length > 0) {
            serverUrl = args[0];
        }
        System.out.println("[MCP-CLIENT] Solicitando lista de herramientas MCP al servidor: " + serverUrl);
        JsonNode toolsList = getToolsList(serverUrl);
        if (toolsList == null || !toolsList.has("result")) {
            System.out.println("No se pudo obtener la lista de herramientas MCP del servidor.");
            return;
        }
        JsonNode tools = toolsList.get("result").get("tools");
        if (tools == null || !tools.isArray() || tools.size() == 0) {
            System.out.println("No hay herramientas MCP disponibles.");
            return;
        }
        System.out.println("Renovatio MCP Client - Herramientas disponibles:");
        int idx = 1;
        for (JsonNode tool : tools) {
            String name = tool.path("name").asText();
            String desc = tool.path("description").asText("");
            System.out.printf("[%d] %s: %s\n", idx, name, desc);
            idx++;
        }
        System.out.print("Selecciona el número de la herramienta a ejecutar: ");
        int sel = Integer.parseInt(scanner.nextLine());
        if (sel < 1 || sel > tools.size()) {
            System.out.println("Selección inválida.");
            return;
        }
        JsonNode selectedTool = tools.get(sel - 1);
        String toolName = selectedTool.path("name").asText();
        System.out.println("Has seleccionado: " + toolName);
        System.out.println("[MCP-CLIENT] Herramienta seleccionada: " + toolName);
        JsonNode params = selectedTool.path("parameters");
        JsonNode example = selectedTool.path("example");
        ObjectNode argsNode = mapper.createObjectNode();
        if (params != null && params.isArray() && params.size() > 0) {
            for (JsonNode param : params) {
                String pname = param.path("name").asText();
                String ptype = param.path("type").asText("string");
                String pdesc = param.path("description").asText("");
                String pexample = example != null ? example.path(pname).asText("") : "";
                String prompt = String.format("Argumento '%s' (%s): %s", pname, ptype, pdesc);
                if (!pexample.isEmpty()) {
                    prompt += " [Ejemplo: " + pexample + "]";
                }
                System.out.print(prompt + " ");
                String pval = scanner.nextLine();
                if (pval.isEmpty() && !pexample.isEmpty()) {
                    pval = pexample;
                }
                argsNode.put(pname, pval);
                System.out.println("[MCP-CLIENT] Argumento ingresado: " + pname + " = " + pval);
            }
        } else {
            System.out.println("No se encontraron parámetros definidos. Puedes ingresar argumentos manualmente.");
            while (true) {
                System.out.print("Nombre del argumento (ENTER para terminar): ");
                String k = scanner.nextLine();
                if (k.isEmpty()) break;
                System.out.print("Valor para '" + k + "': ");
                String v = scanner.nextLine();
                argsNode.put(k, v);
                System.out.println("[MCP-CLIENT] Argumento manual ingresado: " + k + " = " + v);
            }
        }
        System.out.println("[MCP-CLIENT] Argumentos finales enviados: " + argsNode.toString());
        System.out.println("[MCP-CLIENT] Ejecutando herramienta '" + toolName + "' con argumentos: " + argsNode.toString());
        JsonNode result = callTool(serverUrl, toolName, argsNode);
        System.out.println("\nResultado:\n");
        System.out.println(mapper.writerWithDefaultPrettyPrinter().writeValueAsString(result));
    }

    private static JsonNode getToolsList(String serverUrl) throws IOException {
        String payload = "{\"jsonrpc\": \"2.0\", \"id\": \"cli-1\", \"method\": \"tools/list\", \"params\": {}}";
        Request request = new Request.Builder()
                .url(serverUrl)
                .post(RequestBody.create(payload, MediaType.parse("application/json")))
                .build();
        try (Response response = client.newCall(request).execute()) {
            if (!response.isSuccessful()) {
                System.out.println("Error al consultar tools/list: " + response);
                return null;
            }
            return mapper.readTree(response.body().string());
        }
    }

    private static JsonNode callTool(String serverUrl, String toolName, ObjectNode args) throws IOException {
        ObjectNode payload = mapper.createObjectNode();
        payload.put("jsonrpc", "2.0");
        payload.put("id", "cli-2");
        payload.put("method", "tools/run");
        ObjectNode params = mapper.createObjectNode();
        params.put("tool", toolName);
        params.set("args", args);
        payload.set("params", params);
        Request request = new Request.Builder()
                .url(serverUrl)
                .post(RequestBody.create(mapper.writeValueAsBytes(payload), MediaType.parse("application/json")))
                .build();
        try (Response response = client.newCall(request).execute()) {
            if (!response.isSuccessful()) {
                System.out.println("Error al ejecutar herramienta: " + response);
                return null;
            }
            return mapper.readTree(response.body().string());
        }
    }
}

