package org.shark.renovatio.domain.mcp;

import io.swagger.v3.oas.annotations.media.Schema;

import java.util.List;

@Schema(description = "MCP Prompt definition")
public class McpPrompt {
    @Schema(description = "Prompt name")
    private String name;

    @Schema(description = "Prompt description")
    private String description;

    @Schema(description = "Prompt messages")
    private List<Message> messages;

    public McpPrompt() {
    }

    public McpPrompt(String name, String description, List<Message> messages) {
        this.name = name;
        this.description = description;
        this.messages = messages;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public List<Message> getMessages() {
        return messages;
    }

    public void setMessages(List<Message> messages) {
        this.messages = messages;
    }

    @Schema(description = "Prompt message")
    public static class Message {
        @Schema(description = "Message role", example = "user")
        private String role;

        @Schema(description = "Message content")
        private String content;

        public Message() {
        }

        public Message(String role, String content) {
            this.role = role;
            this.content = content;
        }

        public String getRole() {
            return role;
        }

        public void setRole(String role) {
            this.role = role;
        }

        public String getContent() {
            return content;
        }

        public void setContent(String content) {
            this.content = content;
        }
    }
}
