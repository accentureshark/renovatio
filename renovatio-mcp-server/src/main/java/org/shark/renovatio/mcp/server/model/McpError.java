package org.shark.renovatio.mcp.server.model;

import io.swagger.v3.oas.annotations.media.Schema;

@Schema(description = "MCP Error object")
public class McpError {
    @Schema(description = "Error code")
    private int code;

    @Schema(description = "Error message")
    private String message;

    @Schema(description = "Error data")
    private Object data;

    public McpError() {
    }

    public McpError(int code, String message) {
        this.code = code;
        this.message = message;
    }

    public McpError(int code, String message, Object data) {
        this.code = code;
        this.message = message;
        this.data = data;
    }

    public int getCode() {
        return code;
    }

    public void setCode(int code) {
        this.code = code;
    }

    public String getMessage() {
        return message;
    }

    public void setMessage(String message) {
        this.message = message;
    }

    public Object getData() {
        return data;
    }

    public void setData(Object data) {
        this.data = data;
    }
}