package org.shark.renovatio.domain;

import io.swagger.v3.oas.annotations.media.Schema;

@Schema(description = "Definición de una herramienta MCP")
public class ToolDefinition {
    @Schema(description = "Nombre único de la herramienta")
    private String name;
    @Schema(description = "Descripción de la herramienta")
    private String description;
    @Schema(description = "Comando asociado")
    private String command;

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

    public String getCommand() {
        return command;
    }

    public void setCommand(String command) {
        this.command = command;
    }
}
