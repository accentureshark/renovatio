package org.shark.renovatio.provider.cobol.service;

import org.apache.lucene.analysis.standard.StandardAnalyzer;
import org.apache.lucene.document.Document;
import org.apache.lucene.document.Field;
import org.apache.lucene.document.StringField;
import org.apache.lucene.document.TextField;
import org.apache.lucene.index.DirectoryReader;
import org.apache.lucene.index.IndexWriter;
import org.apache.lucene.index.IndexWriterConfig;
import org.apache.lucene.queryparser.classic.QueryParser;
import org.apache.lucene.search.IndexSearcher;
import org.apache.lucene.search.Query;
import org.apache.lucene.search.ScoreDoc;
import org.apache.lucene.search.TopDocs;
import org.apache.lucene.store.FSDirectory;
import org.shark.renovatio.shared.domain.Workspace;
import org.springframework.stereotype.Service;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.*;

/**
 * Indexing service using Apache Lucene for fast symbol and code search
 * Indexes COBOL programs, data items, paragraphs, and generates searchable metadata
 */
@Service
public class IndexingService {

    private final StandardAnalyzer analyzer = new StandardAnalyzer();
    private final Map<String, Path> workspaceIndexes = new HashMap<>();

    /**
     * Indexes a workspace for fast searching
     */
    public void indexWorkspace(Workspace workspace) throws IOException {
        Path workspacePath = Paths.get(workspace.getPath());
        Path indexPath = createIndexPath(workspace);

        FSDirectory indexDirectory = FSDirectory.open(indexPath);
        IndexWriterConfig config = new IndexWriterConfig(analyzer);
        config.setOpenMode(IndexWriterConfig.OpenMode.CREATE);

        try (IndexWriter writer = new IndexWriter(indexDirectory, config)) {
            indexCobolFiles(writer, workspacePath);
        }

        workspaceIndexes.put(workspace.getId(), indexPath);
    }

    /**
     * Searches for symbols, paths, or code patterns
     */
    public List<SearchResult> search(String workspaceId, String searchQuery, int maxResults) throws IOException {
        Path indexPath = workspaceIndexes.get(workspaceId);
        if (indexPath == null) {
            return Collections.emptyList();
        }

        FSDirectory indexDirectory = FSDirectory.open(indexPath);
        try (DirectoryReader reader = DirectoryReader.open(indexDirectory)) {
            IndexSearcher searcher = new IndexSearcher(reader);

            QueryParser parser = new QueryParser("content", analyzer);
            Query query = parser.parse(searchQuery);

            TopDocs topDocs = searcher.search(query, maxResults);
            List<SearchResult> results = new ArrayList<>();

            for (ScoreDoc scoreDoc : topDocs.scoreDocs) {
                Document doc = searcher.doc(scoreDoc.doc);
                SearchResult result = new SearchResult();
                result.setFilePath(doc.get("filePath"));
                result.setType(doc.get("type"));
                result.setName(doc.get("name"));
                result.setContent(doc.get("content"));
                result.setScore(scoreDoc.score);
                results.add(result);
            }

            return results;
        } catch (Exception e) {
            throw new IOException("Search failed: " + e.getMessage(), e);
        }
    }

    /**
     * Finds symbol occurrences across the workspace
     */
    public List<SymbolOccurrence> findSymbolOccurrences(String workspaceId, String symbolName) throws IOException {
        List<SearchResult> searchResults = search(workspaceId, "name:" + symbolName, 100);
        List<SymbolOccurrence> occurrences = new ArrayList<>();

        for (SearchResult result : searchResults) {
            SymbolOccurrence occurrence = new SymbolOccurrence();
            occurrence.setSymbolName(symbolName);
            occurrence.setFilePath(result.getFilePath());
            occurrence.setType(result.getType());
            occurrence.setContext(result.getContent());
            occurrences.add(occurrence);
        }

        return occurrences;
    }

    /**
     * Indexes all COBOL files in the workspace
     */
    private void indexCobolFiles(IndexWriter writer, Path workspacePath) throws IOException {
        Files.walk(workspacePath)
                .filter(Files::isRegularFile)
                .filter(path -> {
                    String fileName = path.getFileName().toString().toLowerCase();
                    return fileName.endsWith(".cob") ||
                            fileName.endsWith(".cobol") ||
                            fileName.endsWith(".cbl") ||
                            fileName.endsWith(".cpy");
                })
                .forEach(cobolFile -> {
                    try {
                        indexCobolFile(writer, cobolFile);
                    } catch (IOException e) {
                        System.err.println("Failed to index file " + cobolFile + ": " + e.getMessage());
                    }
                });
    }

    /**
     * Indexes a single COBOL file
     */
    private void indexCobolFile(IndexWriter writer, Path cobolFile) throws IOException {
        String content = Files.readString(cobolFile);
        String filePath = cobolFile.toString();

        // Index the entire file
        Document fileDoc = new Document();
        fileDoc.add(new StringField("filePath", filePath, Field.Store.YES));
        fileDoc.add(new StringField("type", "file", Field.Store.YES));
        fileDoc.add(new StringField("name", cobolFile.getFileName().toString(), Field.Store.YES));
        fileDoc.add(new TextField("content", content, Field.Store.YES));
        writer.addDocument(fileDoc);

        // Index program ID
        indexProgramId(writer, content, filePath);

        // Index data items
        indexDataItems(writer, content, filePath);

        // Index paragraphs and sections
        indexParagraphsAndSections(writer, content, filePath);
    }

    /**
     * Indexes program ID
     */
    private void indexProgramId(IndexWriter writer, String content, String filePath) throws IOException {
        String[] lines = content.split("\\n");
        for (String line : lines) {
            if (line.toUpperCase().contains("PROGRAM-ID")) {
                String[] parts = line.split("\\s+");
                for (int i = 0; i < parts.length - 1; i++) {
                    if (parts[i].toUpperCase().contains("PROGRAM-ID")) {
                        String programId = parts[i + 1].replaceAll("[^a-zA-Z0-9-]", "");
                        if (!programId.isEmpty()) {
                            Document doc = new Document();
                            doc.add(new StringField("filePath", filePath, Field.Store.YES));
                            doc.add(new StringField("type", "program", Field.Store.YES));
                            doc.add(new StringField("name", programId, Field.Store.YES));
                            doc.add(new TextField("content", line.trim(), Field.Store.YES));
                            writer.addDocument(doc);
                        }
                        break;
                    }
                }
                break;
            }
        }
    }

    /**
     * Indexes data items using simple pattern matching
     */
    private void indexDataItems(IndexWriter writer, String content, String filePath) throws IOException {
        String[] lines = content.split("\\n");
        boolean inDataDivision = false;

        for (String line : lines) {
            String upperLine = line.toUpperCase();

            if (upperLine.contains("DATA DIVISION")) {
                inDataDivision = true;
                continue;
            }

            if (upperLine.contains("PROCEDURE DIVISION")) {
                inDataDivision = false;
                break;
            }

            if (inDataDivision && line.trim().matches("^\\s*\\d{2}\\s+\\w+.*")) {
                String[] parts = line.trim().split("\\s+");
                if (parts.length >= 2) {
                    String level = parts[0];
                    String name = parts[1];

                    Document doc = new Document();
                    doc.add(new StringField("filePath", filePath, Field.Store.YES));
                    doc.add(new StringField("type", "data-item", Field.Store.YES));
                    doc.add(new StringField("name", name, Field.Store.YES));
                    doc.add(new StringField("level", level, Field.Store.YES));
                    doc.add(new TextField("content", line.trim(), Field.Store.YES));
                    writer.addDocument(doc);
                }
            }
        }
    }

    /**
     * Indexes paragraphs and sections
     */
    private void indexParagraphsAndSections(IndexWriter writer, String content, String filePath) throws IOException {
        String[] lines = content.split("\\n");
        boolean inProcedureDivision = false;

        for (String line : lines) {
            String upperLine = line.toUpperCase();

            if (upperLine.contains("PROCEDURE DIVISION")) {
                inProcedureDivision = true;
                continue;
            }

            if (inProcedureDivision) {
                String trimmedLine = line.trim();

                // Look for paragraphs (lines ending with period and not containing verbs)
                if (trimmedLine.endsWith(".") && !containsCobolVerb(trimmedLine)) {
                    String paragraphName = trimmedLine.substring(0, trimmedLine.length() - 1).trim();
                    if (!paragraphName.isEmpty() && paragraphName.matches("^[A-Za-z][A-Za-z0-9-]*$")) {
                        Document doc = new Document();
                        doc.add(new StringField("filePath", filePath, Field.Store.YES));
                        doc.add(new StringField("type", "paragraph", Field.Store.YES));
                        doc.add(new StringField("name", paragraphName, Field.Store.YES));
                        doc.add(new TextField("content", trimmedLine, Field.Store.YES));
                        writer.addDocument(doc);
                    }
                }

                // Look for sections
                if (upperLine.contains(" SECTION")) {
                    String sectionName = trimmedLine.replaceAll("(?i)\\s+SECTION.*", "").trim();
                    if (!sectionName.isEmpty()) {
                        Document doc = new Document();
                        doc.add(new StringField("filePath", filePath, Field.Store.YES));
                        doc.add(new StringField("type", "section", Field.Store.YES));
                        doc.add(new StringField("name", sectionName, Field.Store.YES));
                        doc.add(new TextField("content", trimmedLine, Field.Store.YES));
                        writer.addDocument(doc);
                    }
                }
            }
        }
    }

    /**
     * Checks if a line contains COBOL verbs
     */
    private boolean containsCobolVerb(String line) {
        String upperLine = line.toUpperCase();
        String[] cobolVerbs = {"MOVE", "COMPUTE", "IF", "PERFORM", "CALL", "READ", "WRITE",
                "OPEN", "CLOSE", "DISPLAY", "ACCEPT", "ADD", "SUBTRACT", "MULTIPLY", "DIVIDE"};

        for (String verb : cobolVerbs) {
            if (upperLine.contains(verb)) {
                return true;
            }
        }
        return false;
    }

    /**
     * Creates index path for workspace
     */
    private Path createIndexPath(Workspace workspace) throws IOException {
        Path indexesDir = Paths.get(System.getProperty("java.io.tmpdir"), "renovatio-indexes");
        Files.createDirectories(indexesDir);
        return indexesDir.resolve("workspace-" + workspace.getId());
    }

    /**
     * Search result data structure
     */
    public static class SearchResult {
        private String filePath;
        private String type;
        private String name;
        private String content;
        private float score;

        // Getters and setters
        public String getFilePath() {
            return filePath;
        }

        public void setFilePath(String filePath) {
            this.filePath = filePath;
        }

        public String getType() {
            return type;
        }

        public void setType(String type) {
            this.type = type;
        }

        public String getName() {
            return name;
        }

        public void setName(String name) {
            this.name = name;
        }

        public String getContent() {
            return content;
        }

        public void setContent(String content) {
            this.content = content;
        }

        public float getScore() {
            return score;
        }

        public void setScore(float score) {
            this.score = score;
        }
    }

    /**
     * Symbol occurrence data structure
     */
    public static class SymbolOccurrence {
        private String symbolName;
        private String filePath;
        private String type;
        private String context;

        // Getters and setters
        public String getSymbolName() {
            return symbolName;
        }

        public void setSymbolName(String symbolName) {
            this.symbolName = symbolName;
        }

        public String getFilePath() {
            return filePath;
        }

        public void setFilePath(String filePath) {
            this.filePath = filePath;
        }

        public String getType() {
            return type;
        }

        public void setType(String type) {
            this.type = type;
        }

        public String getContext() {
            return context;
        }

        public void setContext(String context) {
            this.context = context;
        }
    }
}