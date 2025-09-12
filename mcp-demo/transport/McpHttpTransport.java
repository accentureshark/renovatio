package org.shark.melian.mcp.transport;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.eclipse.jetty.server.Server;
import org.eclipse.jetty.servlet.ServletContextHandler;
import org.eclipse.jetty.servlet.ServletHolder;
import org.shark.melian.mcp.PureMcpServer;
import org.shark.melian.mcp.McpDto;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

/**
 * Implementación HTTP transport para MCP server.
 * Proporciona endpoints JSON-RPC y REST.
 */
public class McpHttpTransport {

    private static final Logger log = LoggerFactory.getLogger(McpHttpTransport.class);

    private final PureMcpServer mcpServer;
    private final ObjectMapper objectMapper = new ObjectMapper();
    private Server jettyServer;
    private final int port;
    private final String host;

    public McpHttpTransport(PureMcpServer mcpServer, String host, int port) {
        this.mcpServer = mcpServer;
        this.host = host;
        this.port = port;
    }

    public void start() throws Exception {
        log.info("Starting MCP HTTP transport on {}:{}", host, port);

        jettyServer = new Server();
        jettyServer.addConnector(createConnector());

        ServletContextHandler context = new ServletContextHandler(ServletContextHandler.SESSIONS);
        context.setContextPath("/");

        // MCP JSON-RPC endpoint
        context.addServlet(new ServletHolder(new McpJsonRpcServlet()), "/mcp");

        // Health endpoint
        context.addServlet(new ServletHolder(new HealthServlet()), "/mcp/health");

        // REST endpoints
        context.addServlet(new ServletHolder(new ToolsServlet()), "/mcp/tools");

        jettyServer.setHandler(context);
        jettyServer.start();

        log.info("MCP HTTP transport started successfully on {}:{}", host, port);
    }

    public void stop() throws Exception {
        if (jettyServer != null) {
            log.info("Stopping MCP HTTP transport...");
            jettyServer.stop();
            jettyServer.join();
            log.info("MCP HTTP transport stopped");
        }
    }

    private org.eclipse.jetty.server.ServerConnector createConnector() {
        org.eclipse.jetty.server.ServerConnector connector =
                new org.eclipse.jetty.server.ServerConnector(jettyServer);
        connector.setHost(host);
        connector.setPort(port);
        return connector;
    }

    /**
     * Servlet principal JSON-RPC MCP
     */
    private class McpJsonRpcServlet extends HttpServlet {

        @Override
        protected void doPost(HttpServletRequest req, HttpServletResponse resp) throws IOException {
            resp.setContentType("application/json");
            resp.setCharacterEncoding("UTF-8");

            try {
                McpDto.JsonRpcRequest request = objectMapper.readValue(req.getReader(), McpDto.JsonRpcRequest.class);
                Object result = handleMcpRequest(request);

                McpDto.JsonRpcResponse response = McpDto.JsonRpcResponse.builder()
                        .jsonrpc("2.0")
                        .result(result)
                        .id(request.getId())
                        .build();

                objectMapper.writeValue(resp.getWriter(), response);

            } catch (Exception e) {
                log.error("Error handling MCP request", e);

                McpDto.JsonRpcResponse errorResponse = McpDto.JsonRpcResponse.builder()
                        .jsonrpc("2.0")
                        .error(McpDto.JsonRpcError.builder()
                                .code(-32603)
                                .message("Internal error: " + e.getMessage())
                                .build())
                        .build();

                resp.setStatus(HttpServletResponse.SC_OK); // JSON-RPC errors son HTTP 200
                objectMapper.writeValue(resp.getWriter(), errorResponse);
            }
        }

        private Object handleMcpRequest(McpDto.JsonRpcRequest request) throws Exception {
            String method = request.getMethod();
            Object params = request.getParams();

            log.debug("Handling MCP method: {}", method);

            switch (method) {
                case "initialize": {
                    McpDto.InitializeRequest initReq = objectMapper.convertValue(params, McpDto.InitializeRequest.class);
                    McpDto.InitializeResult result = mcpServer.initialize(initReq);
                    Map<String, Object> response = new HashMap<>();
                    response.put("serverInfo", result.getServerInfo());
                    response.put("capabilities", result.getCapabilities());
                    response.put("protocolVersion", result.getProtocolVersion());
                    return response;
                }
                case "tools/list":
                    return mcpServer.listTools();

                case "tools/call": {
                    McpDto.CallToolRequest callReq = objectMapper.convertValue(params, McpDto.CallToolRequest.class);
                    McpDto.CallToolResult result = mcpServer.callTool(callReq);
                    return result; // Elimina la adaptación especial para get_server_status
                }

                case "resources/list":
                    return mcpServer.listResources();

                case "resources/read":
                    McpDto.ReadResourceRequest readReq = objectMapper.convertValue(params, McpDto.ReadResourceRequest.class);
                    return mcpServer.readResource(readReq);

                default:
                    throw new IllegalArgumentException("Unknown method: " + method);
            }
        }
    }

    /**
     * Health check servlet
     */
    private class HealthServlet extends HttpServlet {

        @Override
        protected void doGet(HttpServletRequest req, HttpServletResponse resp) throws IOException {
            resp.setContentType("application/json");
            resp.setCharacterEncoding("UTF-8");

            try {
                McpDto.HealthStatus health = mcpServer.getHealth();
                objectMapper.writeValue(resp.getWriter(), health);
            } catch (Exception e) {
                log.error("Error getting health status", e);
                resp.setStatus(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
                resp.getWriter().write("{\"status\":\"ERROR\",\"message\":\"" + e.getMessage() + "\"}");
            }
        }
    }

    /**
     * Tools REST endpoint
     */
    private class ToolsServlet extends HttpServlet {

        @Override
        protected void doGet(HttpServletRequest req, HttpServletResponse resp) throws IOException {
            resp.setContentType("application/json");
            resp.setCharacterEncoding("UTF-8");

            try {
                McpDto.ToolsListResult tools = mcpServer.listTools();
                objectMapper.writeValue(resp.getWriter(), tools);
            } catch (Exception e) {
                log.error("Error listing tools", e);
                resp.setStatus(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
                resp.getWriter().write("{\"error\":\"" + e.getMessage() + "\"}");
            }
        }
    }
}