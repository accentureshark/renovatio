package org.shark.renovatio.shared.nql;

import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.RecognitionException;
import org.springframework.stereotype.Service;
// TODO: Temporarily commented out until ANTLR generation is fixed
// import org.shark.renovatio.shared.nql.antlr.NqlLexer;
// import org.shark.renovatio.shared.nql.antlr.NqlParser;

/**
 * Service that parses NQL queries using ANTLR generated parser.
 * TODO: Currently disabled until ANTLR generation is properly configured
 */
@Service
public class NqlParserService {

    public NqlQuery parse(String queryString) {
        // TODO: Temporary implementation - replace with ANTLR parser once generated
        NqlQuery query = new NqlQuery();
        query.setOriginalQuery(queryString);
        
        // Simple parsing logic for basic queries
        String upper = queryString.toUpperCase();
        if (upper.startsWith("FIND")) {
            query.setType(NqlQuery.QueryType.FIND);
            // Extract target: e.g., FIND programs WHERE ...
            String[] parts = queryString.split(" ", 3);
            if (parts.length >= 2) {
                String target = parts[1];
                // Remove WHERE or other clause if present
                if (target.equalsIgnoreCase("WHERE") && parts.length >= 3) {
                    target = parts[2].split(" ", 2)[0];
                }
                query.setTarget(target);
            }
        } else if (upper.startsWith("PLAN")) {
            query.setType(NqlQuery.QueryType.PLAN);
        } else if (upper.startsWith("APPLY")) {
            query.setType(NqlQuery.QueryType.APPLY);
        } else {
            // Unknown query type, set type to null (or UNKNOWN if available)
            query.setType(null); // Si se agrega UNKNOWN, usar: NqlQuery.QueryType.UNKNOWN
        }
        // Nunca retornar null, siempre retornar el objeto query
        return query;
        
        /*
        try {
            NqlLexer lexer = new NqlLexer(CharStreams.fromString(queryString));
            NqlParser parser = new NqlParser(new CommonTokenStream(lexer));
            NqlParser.QueryContext ctx = parser.query();

            NqlQuery query = new NqlQuery();
            query.setOriginalQuery(queryString);

            String action = ctx.action().getText();
            if ("FIND".equalsIgnoreCase(action)) {
                query.setType(NqlQuery.QueryType.FIND);
            } else if ("PLAN".equalsIgnoreCase(action)) {
                query.setType(NqlQuery.QueryType.PLAN);
            } else if ("APPLY".equalsIgnoreCase(action)) {
                query.setType(NqlQuery.QueryType.APPLY);
            }

            query.setTarget(ctx.target().getText().trim());
            if (ctx.whereClause() != null) {
                query.setPredicate(ctx.whereClause().predicate().getText().trim());
            }
            if (ctx.inClause() != null) {
                query.setScope(ctx.inClause().scope().getText().trim());
            }
            if (ctx.returnClause() != null) {
                query.setReturnClause(ctx.returnClause().returnExpr().getText().trim());
            }
            return query;
        } catch (RecognitionException | NullPointerException e) {
            throw new IllegalArgumentException("Invalid NQL query: " + queryString, e);
        }
        */
    }
}
