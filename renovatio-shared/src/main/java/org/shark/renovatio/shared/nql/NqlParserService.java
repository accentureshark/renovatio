package org.shark.renovatio.shared.nql;

import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.RecognitionException;
import org.shark.renovatio.shared.nql.antlr.NqlLexer;
import org.shark.renovatio.shared.nql.antlr.NqlParser;

/**
 * Service that parses NQL queries using ANTLR generated parser.
 */
public class NqlParserService {

    public NqlQuery parse(String queryString) {
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
    }
}

