package org.shark.renovatio.mcp.server.config;

import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.FilterType;

/**
 * Configuration for MCP stdio mode that excludes web-related components.
 */
@Configuration
@ConditionalOnProperty(name = "spring.main.web-application-type", havingValue = "none")
@ComponentScan(
        basePackages = {
                "org.shark.renovatio.mcp.server",
                "org.shark.renovatio.core.service",
                "org.shark.renovatio.core.application",
                "org.shark.renovatio.shared"
        },
        excludeFilters = {
                @ComponentScan.Filter(type = FilterType.REGEX, pattern = "org\\.shark\\.renovatio\\.core\\.api\\..*")
        }
)
public class StdioModeConfiguration {
}
