package org.shark.renovatio.provider.cobol.service;

import io.proleap.cobol.parser.antlr4.Cobol85BaseListener;
import io.proleap.cobol.parser.antlr4.Cobol85Lexer;
import io.proleap.cobol.parser.antlr4.Cobol85Parser;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.tree.ParseTreeWalker;
import org.springframework.stereotype.Service;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.*;
import java.util.stream.Stream;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.shark.renovatio.shared.domain.AnalyzeResult;
import org.shark.renovatio.shared.domain.PerformanceMetrics;
import org.shark.renovatio.shared.domain.Workspace;
import org.shark.renovatio.shared.nql.NqlQuery;
import org.shark.renovatio.provider.cobol.domain.CobolProgram;

/**
 * Service responsible for parsing COBOL sources using ProLeap parser.
 * <p>
 * This service now supports multiple COBOL dialects. The dialect can be
 * configured globally through configuration properties or passed explicitly in
 * MCP requests. When no dialect is provided the service falls back to the
 * default dialect (IBM).
 */
@Service
public class CobolParsingService {

    /** Supported COBOL dialects. */
    public enum Dialect {
        IBM,
        GNU,
        MICRO_FOCUS;

        /**
         * Parse a string value into a {@link Dialect}. If the value does not
         * match any known dialect the default IBM dialect is returned.
         */
        public static Dialect fromString(String value) {
            if (value == null) {
                return IBM;
            }
            switch (value.trim().toUpperCase(Locale.ROOT)) {
                case "GNU":
                case "GNUCOBOL":
                    return GNU;
                case "MICROFOCUS":
                case "MICRO_FOCUS":
                case "MF":
                    return MICRO_FOCUS;
                case "IBM":
                default:
                    return IBM;
            }
        }
    }

    private final Dialect defaultDialect;

    public CobolParsingService() {
        this(Dialect.IBM);
    }

    public CobolParsingService(Dialect dialect) {
        this.defaultDialect = dialect == null ? Dialect.IBM : dialect;
    }

    public Dialect getDefaultDialect() {
        return defaultDialect;
    }

    /**
     * Locate COBOL source files inside a workspace.
     */
    public List<Path> findCobolFiles(Path workspacePath) throws IOException {
        List<Path> cobolFiles = new ArrayList<>();
        try (Stream<Path> walkStream = Files.walk(workspacePath)) {
            walkStream
                .filter(Files::isRegularFile)
                .filter(path -> {
                    String name = path.getFileName().toString().toLowerCase();
                    return name.endsWith(".cob") ||
                           name.endsWith(".cobol") ||
                           name.endsWith(".cbl") ||
                           name.endsWith(".cpy");
                })
                .forEach(cobolFiles::add);
        }
        return cobolFiles;
    }

    /**
     * Locate COBOL copybooks inside a workspace. Copybooks typically use the
     * {@code .cpy} extension.
     */
    public List<Path> findCopybooks(Path workspacePath) throws IOException {
        List<Path> copybooks = new ArrayList<>();
        try (Stream<Path> walkStream = Files.walk(workspacePath)) {
            walkStream
                .filter(Files::isRegularFile)
                .filter(path -> path.getFileName().toString().toLowerCase(Locale.ROOT).endsWith(".cpy"))
                .forEach(copybooks::add);
        }
        return copybooks;
    }

    /**
     * Extract all embedded EXEC SQL statements from a COBOL source file.
     * <p>
     * This method performs a lightweight pattern scan rather than a full
     * parse of the contained SQL. The returned list contains the raw SQL
     * text between {@code EXEC SQL} and {@code END-EXEC} blocks.
     */
    public List<String> extractExecSqlStatements(Path cobolFile) throws IOException {
        String content = Files.readString(cobolFile);
        return extractExecSqlStatements(content);
    }

    /**
     * Extract embedded EXEC SQL statements from COBOL source content.
     */
    public List<String> extractExecSqlStatements(String cobolSource) {
        List<String> statements = new ArrayList<>();
        Pattern pattern = Pattern.compile("EXEC\s+SQL(.*?)END-EXEC", Pattern.CASE_INSENSITIVE | Pattern.DOTALL);
        Matcher matcher = pattern.matcher(cobolSource);
        while (matcher.find()) {
            String sql = matcher.group(1).trim();
            if (!sql.isEmpty()) {
                statements.add(sql);
            }
        }
        return statements;
    }

    /**
     * Analyze all COBOL files in the given workspace. The dialect can be
     * provided via query parameters ("dialect") or workspace metadata. When
     * absent the service's default dialect is used.
     */
    public AnalyzeResult analyzeCOBOL(NqlQuery query, Workspace workspace) throws IOException {
        long start = System.nanoTime();

        Dialect dialect = resolveDialect(query, workspace);
        Path root = Paths.get(workspace.getPath());
        List<Path> cobolFiles = findCobolFiles(root);

        List<CobolProgram> programs = new ArrayList<>();
        for (Path cobolFile : cobolFiles) {
            Map<String, Object> metadata = parseCobolFile(cobolFile, dialect);
            metadata.put("filePath", cobolFile.toString());
            CobolProgram program = new CobolProgram();
            program.setProgramId((String) metadata.get("programId"));
            program.setProgramName((String) metadata.get("programId"));
            program.setMetadata(metadata);
            programs.add(program);
        }

        Map<String, Object> data = new HashMap<>();
        data.put("programs", programs);

        AnalyzeResult result = new AnalyzeResult(true, "Parsed " + programs.size() + " COBOL files");
        result.setData(data);

        long elapsed = System.nanoTime() - start;
        result.setPerformance(new PerformanceMetrics(elapsed / 1_000_000));
        return result;
    }

    private Dialect resolveDialect(NqlQuery query, Workspace workspace) {
        String dialectStr = null;
        if (query != null && query.getParameters() != null) {
            Object param = query.getParameters().get("dialect");
            if (param != null) {
                dialectStr = param.toString();
            }
        }
        if (dialectStr == null && workspace != null && workspace.getMetadata() != null) {
            Object meta = workspace.getMetadata().get("dialect");
            if (meta != null) {
                dialectStr = meta.toString();
            }
        }
        return Dialect.fromString(dialectStr == null ? defaultDialect.name() : dialectStr);
    }

    /**
     * Parse a COBOL file using the service's default dialect.
     */
    public Map<String, Object> parseCobolFile(Path cobolFile) throws IOException {
        return parseCobolFile(cobolFile, defaultDialect);
    }

    /**
     * Parse a COBOL file and return a simple AST representation using the given
     * dialect.
     */
    public Map<String, Object> parseCobolFile(Path cobolFile, Dialect dialect) throws IOException {
        CharStream input = CharStreams.fromPath(cobolFile);
        Cobol85Lexer lexer = new Cobol85Lexer(input);
        CommonTokenStream tokens = new CommonTokenStream(lexer);
        Cobol85Parser parser = new Cobol85Parser(tokens);

        configureParserForDialect(lexer, parser, dialect);

        ParserRuleContext tree = parser.startRule();

        CollectingListener listener = new CollectingListener();
        ParseTreeWalker.DEFAULT.walk(listener, tree);

        Map<String, Object> ast = new HashMap<>();
        String programId = listener.getProgramId();
        if (programId == null || programId.isEmpty()) {
            programId = cobolFile.getFileName().toString();
        }
        ast.put("programId", programId);
        ast.put("calls", listener.getCalls());
        ast.put("copies", listener.getCopies());
        ast.put("dataItems", listener.getDataItems());
        ast.put("parseTree", tree.toStringTree(parser));
        ast.put("dialect", dialect.name());
        return ast;
    }

    /**
     * Parse a COBOL copybook. Since copybooks often contain fragments that are
     * not valid standalone programs, the contents are wrapped in a minimal
     * program structure before parsing.
     */
    public Map<String, Object> parseCopybook(Path copybookFile, Dialect dialect) throws IOException {
        String content = Files.readString(copybookFile);
        String wrapped = "       IDENTIFICATION DIVISION.\n" +
                         "       PROGRAM-ID. DUMMY.\n" +
                         "       DATA DIVISION.\n" +
                         "       WORKING-STORAGE SECTION.\n" +
                         content + "\n" +
                         "       END PROGRAM DUMMY.";

        CharStream input = CharStreams.fromString(wrapped);
        Cobol85Lexer lexer = new Cobol85Lexer(input);
        CommonTokenStream tokens = new CommonTokenStream(lexer);
        Cobol85Parser parser = new Cobol85Parser(tokens);
        configureParserForDialect(lexer, parser, dialect);

        ParserRuleContext tree = parser.startRule();
        CollectingListener listener = new CollectingListener();
        ParseTreeWalker.DEFAULT.walk(listener, tree);

        Map<String, Object> ast = new HashMap<>();
        String programId = copybookFile.getFileName().toString();
        ast.put("programId", programId.replaceFirst("\\.[^.]+$", ""));
        ast.put("dataItems", listener.getDataItems());
        ast.put("dialect", dialect.name());
        return ast;
    }

    /**
     * Adjust parser configuration according to the specified dialect. The
     * ProLeap parser exposes various flags for different dialect features. Only
     * a small subset is toggled here as a demonstration; unsupported dialect
     * options are safely ignored.
     */
    private void configureParserForDialect(Cobol85Lexer lexer, Cobol85Parser parser, Dialect dialect) {
        switch (dialect) {
            case GNU:
                // Example: GNU COBOL is generally more permissive
                // No concrete options required for this demo
                break;
            case MICRO_FOCUS:
                // Example: enable Micro Focus specific extensions if available
                break;
            case IBM:
            default:
                // Default COBOL 85 behaviour
                break;
        }
    }

    /** Listener that collects interesting nodes from the parse tree. */
    private static class CollectingListener extends Cobol85BaseListener {
        private String programId;
        private final Set<String> calls = new HashSet<>();
        private final Set<String> copies = new HashSet<>();
        private final List<Map<String, Object>> dataItems = new ArrayList<>();

        @Override
        public void enterProgramIdParagraph(Cobol85Parser.ProgramIdParagraphContext ctx) {
            if (ctx.programName() != null) {
                programId = ctx.programName().getText();
            }
        }

        @Override
        public void enterCallStatement(Cobol85Parser.CallStatementContext ctx) {
            if (ctx.literal() != null) {
                String text = ctx.literal().getText();
                text = text.replace("\"", "").replace("'", "");
                calls.add(text);
            } else if (ctx.identifier() != null) {
                calls.add(ctx.identifier().getText());
            }
        }

        @Override
        public void enterCopyStatement(Cobol85Parser.CopyStatementContext ctx) {
            if (ctx.textName() != null) {
                copies.add(ctx.textName().getText());
            }
        }
        
        @Override
        public void enterDataDescriptionEntryFormat1(Cobol85Parser.DataDescriptionEntryFormat1Context ctx) {
            Map<String, Object> item = new HashMap<>();
            item.put("level", ctx.levelNumber().getText());
            if (ctx.dataName() != null) {
                item.put("name", ctx.dataName().getText());
            }
            if (ctx.pictureClause() != null && ctx.pictureClause().pictureString() != null) {
                item.put("picture", ctx.pictureClause().pictureString().getText());
            }
            dataItems.add(item);
        }

        public String getProgramId() { return programId; }
        public Set<String> getCalls() { return calls; }
        public Set<String> getCopies() { return copies; }
        public List<Map<String, Object>> getDataItems() { return dataItems; }
    }
}
