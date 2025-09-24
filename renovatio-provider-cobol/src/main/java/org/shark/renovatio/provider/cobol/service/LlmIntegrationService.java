package org.shark.renovatio.provider.cobol.service;

import org.shark.renovatio.shared.nql.NqlParserService;
import org.shark.renovatio.shared.nql.NqlQuery;
import org.springframework.stereotype.Service;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Service for integrating with LLM systems and parsing NQL queries
 */
@Service
public class LlmIntegrationService {

    private static final Logger logger = LoggerFactory.getLogger(LlmIntegrationService.class);

    private final NqlParserService nqlParserService;

    public LlmIntegrationService(NqlParserService nqlParserService) {
        this.nqlParserService = nqlParserService;
    }

    /**
     * Parse an NQL query string into a structured NqlQuery object
     *
     * @param nqlString the NQL query string to parse
     * @return parsed NqlQuery object
     */
    private NqlQuery parseNqlQuery(String nqlString) {
        logger.debug("Parsing NQL query: {}", nqlString);

        try {
            NqlQuery query = nqlParserService.parse(nqlString);
            if (query != null && query.getType() != null) {
                logger.debug("Successfully parsed NQL query: type={}, target={}",
                           query.getType(), query.getTarget());
                return query;
            } else {
                logger.warn("Failed to parse NQL query, creating fallback query");
                // Create a fallback query for invalid input
                NqlQuery fallbackQuery = new NqlQuery();
                fallbackQuery.setOriginalQuery(nqlString);
                fallbackQuery.setType(null); // Indicates parsing failure
                return fallbackQuery;
            }
        } catch (Exception e) {
            logger.error("Exception while parsing NQL query: {}", e.getMessage(), e);
            // Create a fallback query for exceptions
            NqlQuery fallbackQuery = new NqlQuery();
            fallbackQuery.setOriginalQuery(nqlString);
            fallbackQuery.setType(null); // Indicates parsing failure
            return fallbackQuery;
        }
    }
}
