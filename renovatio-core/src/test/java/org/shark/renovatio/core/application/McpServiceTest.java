package org.shark.renovatio.core.application;

import org.junit.jupiter.api.Test;
import org.shark.renovatio.core.mcp.McpRequest;
import org.shark.renovatio.core.mcp.McpResponse;
import org.shark.renovatio.core.mcp.McpTool;

import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

class McpServiceTest {

    @Test
    void cliManifestIncludesCommandsFromTools() {
        McpToolingService toolingService = mock(McpToolingService.class);
        McpTool tool = new McpTool("test.tool", "test description", Map.of("type", "object"));
        when(toolingService.getMcpTools()).thenReturn(List.of(tool));

        McpService service = new McpService(toolingService);
        McpRequest request = new McpRequest("1", "cli/manifest", Map.of());
        McpResponse response = service.handleMcpRequest(request);

        assertNull(response.getError());
        @SuppressWarnings("unchecked")
        Map<String, Object> result = (Map<String, Object>) response.getResult();
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> commands = (List<Map<String, Object>>) result.get("commands");
        assertEquals(1, commands.size());
        Map<String, Object> command = commands.get(0);
        assertEquals("test.tool", command.get("name"));
        assertEquals("test description", command.get("description"));
        assertEquals(Map.of("type", "object"), command.get("inputSchema"));
    }
}

