package org.shark.renovatio.web.config;

import org.shark.renovatio.shared.nql.NqlParserService;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class NqlParserConfig {
    @Bean
    public NqlParserService nqlParserService() {
        return new NqlParserService();
    }
}

