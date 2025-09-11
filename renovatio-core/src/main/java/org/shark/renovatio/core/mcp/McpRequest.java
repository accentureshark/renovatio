package org.shark.renovatio.core.mcp;

/**
 * DTO for MCP (JSON-RPC 2.0) requests.
 */
public class McpRequest {
    private String jsonrpc;
    private String method;
    private Object params;
    private String id;

    public String getJsonrpc() { return jsonrpc; }
    public void setJsonrpc(String jsonrpc) { this.jsonrpc = jsonrpc; }
    public String getMethod() { return method; }
    public void setMethod(String method) { this.method = method; }
    public Object getParams() { return params; }
    public void setParams(Object params) { this.params = params; }
    public String getId() { return id; }
    public void setId(String id) { this.id = id; }
}

