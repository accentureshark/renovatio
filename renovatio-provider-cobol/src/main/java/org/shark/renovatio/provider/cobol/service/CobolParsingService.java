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
import java.util.*;
import java.util.stream.Stream;

/**
 * Service responsible for parsing COBOL sources using ProLeap parser
 */
@Service
public class CobolParsingService {

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
     * Parse a COBOL file and return a simple AST representation.
     */
    public Map<String, Object> parseCobolFile(Path cobolFile) throws IOException {
        CharStream input = CharStreams.fromPath(cobolFile);
        Cobol85Lexer lexer = new Cobol85Lexer(input);
        CommonTokenStream tokens = new CommonTokenStream(lexer);
        Cobol85Parser parser = new Cobol85Parser(tokens);
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
        return ast;
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
