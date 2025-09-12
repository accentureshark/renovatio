package org.shark.melian.mcp.transport;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.shark.melian.mcp.PureMcpServer;
import org.shark.melian.mcp.McpDto;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * Stdio transport implementation for MCP server.
 * Handles JSON-RPC communication over stdin/stdout according to MCP specification.
 */
public class McpStdioTransport {

    private static final Logger log = LoggerFactory.getLogger(McpStdioTransport.class);
    
    private final PureMcpServer mcpServer;
    private final ObjectMapper objectMapper = new ObjectMapper();
    private final AtomicBoolean running = new AtomicBoolean(false);
    private ExecutorService executor;
    private BufferedReader stdin;
    private PrintWriter stdout;

    public McpStdioTransport(PureMcpServer mcpServer) {
        this.mcpServer = mcpServer;
    }

    public void start() {
        log.info("Starting MCP Stdio transport...");
        
        running.set(true);
        stdin = new BufferedReader(new InputStreamReader(System.in));
        stdout = new PrintWriter(System.out, true);
        executor = Executors.newSingleThreadExecutor(r -> {
            Thread t = new Thread(r, "mcp-stdio-transport");
            t.setDaemon(false);
            return t;
        });

        executor.submit(this::processStdio);
        log.info("MCP Stdio transport started");
    }

    public void stop() {
        log.info("Stopping MCP Stdio transport...");
        running.set(false);
        
        if (executor != null) {
            executor.shutdown();
        }
        
        log.info("MCP Stdio transport stopped");
    }

    private void processStdio() {
        log.debug("Starting stdio processing loop");
        
        try {
            String line;
            while (running.get() && (line = stdin.readLine()) != null) {
                if (line.trim().isEmpty()) {
                    continue;
                }
                
                log.debug("Received stdin: {}", line);
                
                try {
                    processJsonRpcMessage(line.trim());
                } catch (Exception e) {
                    log.error("Error processing message: " + line, e);
                    sendErrorResponse(null, -32603, "Internal error: " + e.getMessage());
                }
            }
        } catch (Exception e) {
            log.error("Error in stdio processing", e);
        }
        
        log.debug("Stdio processing loop ended");
    }

    private void processJsonRpcMessage(String message) {
        try {
            McpDto.JsonRpcRequest request = objectMapper.readValue(message, McpDto.JsonRpcRequest.class);
            Object result = handleMcpRequest(request);
            
            McpDto.JsonRpcResponse response = McpDto.JsonRpcResponse.builder()
                    .jsonrpc("2.0")
                    .result(result)
                    .id(request.getId())
                    .build();

            String responseJson = objectMapper.writeValueAsString(response);
            stdout.println(responseJson);
            stdout.flush();
            
            log.debug("Sent response: {}", responseJson);
            
        } catch (Exception e) {
            log.error("Error processing JSON-RPC message", e);
            sendErrorResponse(null, -32700, "Parse error: " + e.getMessage());
        }
    }

    private Object handleMcpRequest(McpDto.JsonRpcRequest request) throws Exception {
        log.debug("Entering handleMcpRequest with request: {}", request);

        String method = request.getMethod();
        Object params = request.getParams();

        log.info("Processing MCP method: {}", method);
        log.debug("Received parameters: {}", params);

        try {
            Object result;
            switch (method) {
                case "initialize":
                    McpDto.InitializeRequest initReq = objectMapper.convertValue(params, McpDto.InitializeRequest.class);
                    result = mcpServer.initialize(initReq);
                    break;

                case "tools/list":
                    result = mcpServer.listTools();
                    break;

                case "tools/call":
                    McpDto.CallToolRequest callReq = objectMapper.convertValue(params, McpDto.CallToolRequest.class);
                    result = mcpServer.callTool(callReq);
                    break;

                case "resources/list":
                    result = mcpServer.listResources();
                    break;

                case "resources/read":
                    McpDto.ReadResourceRequest readReq = objectMapper.convertValue(params, McpDto.ReadResourceRequest.class);
                    result = mcpServer.readResource(readReq);
                    break;

                case "ping":
                    result = "pong";
                    break;

                default:
                    log.warn("Unknown method: {}", method);
                    throw new IllegalArgumentException("Unknown method: " + method);
            }
            log.debug("Result for method {}: {}", method, result);
            return result;
        } catch (Exception e) {
            log.error("Error processing MCP method: {}", method, e);
            throw e;
        }
    }

    private void sendErrorResponse(Object id, int code, String message) {
        try {
            McpDto.JsonRpcResponse errorResponse = McpDto.JsonRpcResponse.builder()
                    .jsonrpc("2.0")
                    .error(McpDto.JsonRpcError.builder()
                            .code(code)
                            .message(message)
                            .build())
                    .id(id)
                    .build();

            String errorJson = objectMapper.writeValueAsString(errorResponse);
            stdout.println(errorJson);
            stdout.flush();
            
            log.debug("Sent error response: {}", errorJson);
            
        } catch (Exception e) {
            log.error("Failed to send error response", e);
        }
    }

    /**
     * Send a notification (no response expected)
     */
    public void sendNotification(String method, Object params) {
        try {
            McpDto.JsonRpcRequest notification = McpDto.JsonRpcRequest.builder()
                    .jsonrpc("2.0")
                    .method(method)
                    .params(params)
                    .build(); // No id for notifications

            String notificationJson = objectMapper.writeValueAsString(notification);
            stdout.println(notificationJson);
            stdout.flush();
            
            log.debug("Sent notification: {}", notificationJson);
            
        } catch (Exception e) {
            log.error("Failed to send notification", e);
        }
    }

    public boolean isRunning() {
        return running.get();
    }
}