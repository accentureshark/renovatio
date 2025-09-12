
package org.shark.melian.mcp;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.shark.melian.config.MelianProperties;
import org.shark.melian.mcp.transport.McpHttpTransport;
import org.shark.melian.mcp.transport.McpStdioTransport;
import org.shark.melian.service.AggregatedMovieService;
import org.springframework.stereotype.Component;

/**
 * Implementación Spring del servidor MCP.
 * Sigue la especificación Model Context Protocol para acceso a datos de películas.
 */
@Component
@RequiredArgsConstructor
@Slf4j
public class MelianMcpServer {

    private final MelianProperties melianProperties;
    private final AggregatedMovieService aggregatedMovieService;
    private final PureMcpServer mcpServer;

    private McpHttpTransport httpTransport;
    private McpStdioTransport stdioTransport;

    public void start() {
        log.info("Starting MELIAN MCP Server...");

        try {
            // Determine transport mode from environment or arguments
            boolean httpEnabled = isHttpTransportEnabled();
            boolean stdioEnabled = isStdioTransportEnabled();

            if (httpEnabled) {
                startHttpTransport();
            }

            if (stdioEnabled) {
                startStdioTransport();
            }

            if (!httpEnabled && !stdioEnabled) {
                // Default to stdio if nothing specified
                log.info("No transport specified, defaulting to stdio");
                startStdioTransport();
            }

            log.info("MELIAN MCP Server started successfully");

            // Keep the server running
            if (stdioEnabled) {
                // If stdio is enabled, the main thread will handle stdio communication
                waitForStdioCompletion();
            } else {
                // If only HTTP is enabled, keep the main thread alive
                waitForShutdown();
            }

        } catch (Exception e) {
            log.error("Failed to start MCP server", e);
            System.exit(1);
        }
    }

    private boolean isHttpTransportEnabled() {
        String httpEnabled = System.getenv("MCP_SERVER_HTTP_ENABLED");
        if (httpEnabled != null) {
            return "true".equalsIgnoreCase(httpEnabled);
        }
        return melianProperties.getMcp().getServer().getHttp().isEnabled();
    }

    private boolean isStdioTransportEnabled() {
        String stdioEnabled = System.getenv("MCP_SERVER_STDIO_ENABLED");
        return "true".equalsIgnoreCase(stdioEnabled) ||
               System.getProperty("mcp.stdio.enabled", "true").equals("true");
    }

    private void startHttpTransport() throws Exception {
        String host = System.getenv("MCP_SERVER_HOST");
        if (host == null) {
            host = melianProperties.getMcp().getServer().getHost();
        }

        String portStr = System.getenv("MCP_SERVER_PORT");
        int port;
        if (portStr != null) {
            port = Integer.parseInt(portStr);
        } else {
            port = melianProperties.getMcp().getServer().getPort();
        }

        httpTransport = new McpHttpTransport(mcpServer, host, port);
        httpTransport.start();
    }

    private void startStdioTransport() {
        stdioTransport = new McpStdioTransport(mcpServer);
        stdioTransport.start();
    }

    private void waitForStdioCompletion() {
        // Wait for stdio transport to complete
        while (stdioTransport != null && stdioTransport.isRunning()) {
            try {
                Thread.sleep(100);
            } catch (InterruptedException e) {
                log.info("Interrupted, shutting down...");
                break;
            }
        }
    }

    private void waitForShutdown() {
        // Add shutdown hook and wait
        Runtime.getRuntime().addShutdownHook(new Thread(this::shutdown));

        try {
            // Keep the main thread alive
            Object lock = new Object();
            synchronized (lock) {
                lock.wait();
            }
        } catch (InterruptedException e) {
            log.info("Shutting down...");
        }
    }

    public void shutdown() {
        log.info("Shutting down MELIAN MCP Server...");

        try {
            if (stdioTransport != null) {
                stdioTransport.stop();
            }
            if (httpTransport != null) {
                httpTransport.stop();
            }
            // Spring will handle cleanup of services automatically
        } catch (Exception e) {
            log.warn("Error during shutdown", e);
        }

        log.info("MELIAN MCP Server shutdown complete");
    }
}