package org.shark.renovatio.domain.mcp;

import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;

@Schema(description = "MCP JSON-RPC 2.0 Request")
public class McpRequest {
    @JsonProperty("jsonrpc")
    @Schema(description = "JSON-RPC version", example = "2.0")
    private String jsonrpc = "2.0";
    
    @Schema(description = "Request ID")
    private String id;
    
    @Schema(description = "Method name")
    private String method;
    
    @Schema(description = "Request parameters")
    private Object params;

    public McpRequest() {}

    public McpRequest(String id, String method, Object params) {
        this.id = id;
        this.method = method;
        this.params = params;
    }

    public String getJsonrpc() {
        return jsonrpc;
    }

    public void setJsonrpc(String jsonrpc) {
        this.jsonrpc = jsonrpc;
    }

    public String getId() {
        return id;
    }

    public void setId(String id) {
        this.id = id;
    }

    public String getMethod() {
        return method;
    }

    public void setMethod(String method) {
        this.method = method;
    }

    public Object getParams() {
        return params;
    }

    public void setParams(Object params) {
        this.params = params;
    }
}