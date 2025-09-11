package org.shark.renovatio.core.mcp;

/**
 * DTO for MCP (JSON-RPC 2.0) responses.
 */
public class McpResponse {
    private String jsonrpc = "2.0";
    private Object result;
    private Object error;
    private String id;

    public String getJsonrpc() { return jsonrpc; }
    public void setJsonrpc(String jsonrpc) { this.jsonrpc = jsonrpc; }
    public Object getResult() { return result; }
    public void setResult(Object result) { this.result = result; }
    public Object getError() { return error; }
    public void setError(Object error) { this.error = error; }
    public String getId() { return id; }
    public void setId(String id) { this.id = id; }
}
