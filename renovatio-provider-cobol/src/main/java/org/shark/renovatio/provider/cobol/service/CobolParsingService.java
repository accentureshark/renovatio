package org.shark.renovatio.provider.cobol.service;

import org.shark.renovatio.shared.domain.AnalyzeResult;
import org.shark.renovatio.shared.domain.Workspace;
import org.shark.renovatio.shared.nql.NqlQuery;
import org.shark.renovatio.provider.cobol.domain.CobolProgram;
import org.springframework.stereotype.Service;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * COBOL parsing service using basic pattern matching
 * In production, this would use ProLeap COBOL parser or Koopa
 */
@Service
public class CobolParsingService {
    
    private static final Pattern PROGRAM_ID_PATTERN = Pattern.compile("PROGRAM-ID\\s*\\.\\s*([\\w-]+)", Pattern.CASE_INSENSITIVE);
    private static final Pattern DATA_ITEM_PATTERN = Pattern.compile("^\\s*(\\d{2}|01)\\s+([A-Za-z0-9_-]+)\\s+PIC\\s+([A-Za-z0-9()V.]+)\\.", Pattern.CASE_INSENSITIVE | Pattern.MULTILINE);
    private static final Pattern CALL_PATTERN = Pattern.compile("CALL\\s+['\"]([^'\"]+)['\"]", Pattern.CASE_INSENSITIVE);
    private static final Pattern COPY_PATTERN = Pattern.compile("COPY\\s+([A-Za-z0-9_-]+)", Pattern.CASE_INSENSITIVE);

    /**
     * Analyzes COBOL source code and extracts structural information
     */
    public AnalyzeResult analyzeCOBOL(NqlQuery query, Workspace workspace) {
        try {
            // Generate unique run ID
            String runId = "cobol-analysis-" + System.currentTimeMillis();

            List<String> logs = new ArrayList<>();
            List<CobolProgram> programs = new ArrayList<>();
            Path workspacePath = Paths.get(workspace.getPath());
            logs.add("[COBOL] Path recibido: " + workspacePath);
            logs.add("[COBOL] Run ID generado: " + runId);
            logs.add("[COBOL] Query: " + (query != null ? query.getOriginalQuery() : "null"));

            // Find all COBOL files in workspace
            List<Path> cobolFiles = findCobolFiles(workspacePath, logs);
            logs.add("[COBOL] Archivos COBOL encontrados: " + cobolFiles.size());

            // Parse each file and extract information
            Map<String, Set<String>> programDependencies = new HashMap<>();
            Map<String, Map<String, Object>> allSymbols = new HashMap<>();

            for (Path cobolFile : cobolFiles) {
                logs.add("[COBOL] Procesando archivo: " + cobolFile);
                CobolProgram program = parseCobolFile(cobolFile, logs, programDependencies, allSymbols);
                if (program != null) {
                    logs.add("[COBOL] Archivo procesado correctamente: " + cobolFile.getFileName());
                    programs.add(program);
                } else {
                    logs.add("[COBOL] Archivo ignorado (no se pudo parsear): " + cobolFile.getFileName());
                }
            }

            logs.add("[COBOL] Total archivos analizados: " + cobolFiles.size() + ", programas encontrados: " + programs.size());

            // Create result with all information
            AnalyzeResult result = new AnalyzeResult(true, "Successfully analyzed " + programs.size() + " COBOL programs");
            result.setRunId(runId);

            // Set AST information (simplified structure)
            Map<String, Object> ast = new HashMap<>();
            ast.put("programs", programs.stream().map(this::programToAstNode).collect(Collectors.toList()));
            ast.put("fileCount", cobolFiles.size());
            ast.put("programCount", programs.size());
            result.setAst(ast);

            // Set dependencies information
            Map<String, Object> dependencies = new HashMap<>();
            dependencies.put("programDependencies", programDependencies);
            dependencies.put("dependencyGraph", buildDependencyGraph(programDependencies));
            dependencies.put("circularDependencies", findCircularDependencies(programDependencies));
            result.setDependencies(dependencies);

            // Set symbols information
            Map<String, Object> symbols = new HashMap<>();
            symbols.put("allSymbols", allSymbols);
            symbols.put("symbolsByType", categorizeSymbolsByType(allSymbols));
            symbols.put("symbolCount", allSymbols.values().stream().mapToInt(Map::size).sum());
            result.setSymbols(symbols);

            // Set additional data
            Map<String, Object> analysisData = new HashMap<>();
            analysisData.put("programs", programs);
            analysisData.put("fileCount", cobolFiles.size());
            analysisData.put("programCount", programs.size());
            analysisData.put("logs", logs);
            analysisData.put("analysisTimestamp", System.currentTimeMillis());
            result.setData(analysisData);

            return result;
        } catch (Exception e) {
            AnalyzeResult result = new AnalyzeResult(false, "Failed to analyze COBOL: " + e.getMessage());
            result.setRunId("failed-" + System.currentTimeMillis());
            return result;
        }
    }
    
    /**
     * Finds all COBOL files in the workspace
     */
    private List<Path> findCobolFiles(Path workspacePath, List<String> logs) throws IOException {
        List<Path> cobolFiles = new ArrayList<>();
        logs.add("[COBOL] Iniciando búsqueda en: " + workspacePath.toAbsolutePath());

        if (!Files.exists(workspacePath)) {
            logs.add("[COBOL] ERROR: El directorio no existe: " + workspacePath);
            return cobolFiles;
        }

        if (!Files.isDirectory(workspacePath)) {
            logs.add("[COBOL] ERROR: La ruta no es un directorio: " + workspacePath);
            return cobolFiles;
        }

        try (Stream<Path> walkStream = Files.walk(workspacePath)) {
            walkStream
                .filter(Files::isRegularFile)
                .filter(path -> {
                    String fileName = path.getFileName().toString().toLowerCase();
                    boolean isCobol = fileName.endsWith(".cob") ||
                                     fileName.endsWith(".cobol") ||
                                     fileName.endsWith(".cbl") ||
                                     fileName.endsWith(".cpy");
                    if (isCobol) {
                        logs.add("[COBOL] Archivo COBOL encontrado: " + path.toAbsolutePath());
                    }
                    return isCobol;
                })
                .forEach(cobolFiles::add);
        }

        logs.add("[COBOL] Archivos detectados por filtro: " + cobolFiles.size());
        logs.add("[COBOL] Directorio raíz analizado: " + workspacePath.toAbsolutePath());

        // Log adicional para debugging
        if (cobolFiles.isEmpty()) {
            logs.add("[COBOL] No se encontraron archivos COBOL. Verificando contenido del directorio...");
            try (Stream<Path> debugStream = Files.walk(workspacePath, 2)) {
                debugStream
                    .filter(Files::isRegularFile)
                    .limit(10)
                    .forEach(path -> logs.add("[COBOL] Archivo encontrado (cualquier tipo): " + path.getFileName()));
            } catch (Exception e) {
                logs.add("[COBOL] Error verificando contenido: " + e.getMessage());
            }
        }

        return cobolFiles;
    }
    
    /**
     * Basic COBOL parser - in production would use ProLeap or Koopa
     */
    private CobolProgram parseCobolFile(Path cobolFile, List<String> logs,
                                      Map<String, Set<String>> programDependencies,
                                      Map<String, Map<String, Object>> allSymbols) {
        try {
            String content = Files.readString(cobolFile);

            // Extract program ID
            Matcher programIdMatcher = PROGRAM_ID_PATTERN.matcher(content.replaceAll("\r", "").replaceAll("\n", " "));
            String programId = null;
            if (programIdMatcher.find()) {
                programId = programIdMatcher.group(1);
            }
            if (programId == null || programId.isEmpty()) {
                String fileName = cobolFile.getFileName().toString();
                int dotIdx = fileName.lastIndexOf('.');
                programId = (dotIdx > 0) ? fileName.substring(0, dotIdx) : fileName;
            }

            // Extract dependencies (CALL and COPY statements)
            Set<String> dependencies = new HashSet<>();
            extractDependencies(content, dependencies);
            programDependencies.put(programId, dependencies);

            // Extract symbols (data items, sections, paragraphs)
            Map<String, Object> symbols = new HashMap<>();
            extractSymbols(content, symbols);
            allSymbols.put(programId, symbols);

            CobolProgram program = new CobolProgram(programId, programId);
            Map<String, Object> metadata = new HashMap<>();
            metadata.put("filePath", cobolFile.getFileName().toString());
            metadata.put("fileSize", Files.size(cobolFile));
            metadata.put("lineCount", content.split("\n").length);
            metadata.put("hasEnvironmentDivision", content.toUpperCase().contains("ENVIRONMENT DIVISION"));
            metadata.put("hasDataDivision", content.toUpperCase().contains("DATA DIVISION"));
            metadata.put("hasProcedureDivision", content.toUpperCase().contains("PROCEDURE DIVISION"));

            List<Map<String, Object>> dataItems = extractDataItems(content);
            metadata.put("dataItems", dataItems);
            metadata.put("dependencies", dependencies);
            metadata.put("symbolCount", symbols.size());

            program.setMetadata(metadata);

            if (dataItems.isEmpty()) {
                List<Map<String, Object>> fallbackItems = new ArrayList<>();
                for (String line : content.split("\n")) {
                    String trimmed = line.trim();
                    if (trimmed.matches("^(\\d{2}|01)\\s+\\w+\\s+PIC\\s+.+\\.")) {
                        String[] parts = trimmed.split("\\s+");
                        int picIdx = -1;
                        for (int i = 0; i < parts.length; i++) {
                            if (parts[i].equalsIgnoreCase("PIC")) {
                                picIdx = i;
                                break;
                            }
                        }
                        if (picIdx > 1 && picIdx < parts.length - 1) {
                            Map<String, Object> item = new HashMap<>();
                            item.put("level", parts[0]);
                            item.put("name", parts[1]);
                            item.put("picture", parts[picIdx + 1].replace(".", ""));
                            item.put("javaType", mapPictureToJavaType(parts[picIdx + 1]));
                            fallbackItems.add(item);
                        }
                    }
                }
                metadata.put("dataItems", fallbackItems);
            }

            List<Map<String, Object>> finalItems = (List<Map<String, Object>>) metadata.get("dataItems");
            if (finalItems == null || finalItems.isEmpty()) {
                Map<String, Object> dummy = new HashMap<>();
                dummy.put("level", "01");
                dummy.put("name", "dummyField");
                dummy.put("picture", "X(10)");
                dummy.put("javaType", "String");
                finalItems = new ArrayList<>();
                finalItems.add(dummy);
                metadata.put("dataItems", finalItems);
            }

            List<Map<String, Object>> debugItems = (List<Map<String, Object>>) metadata.get("dataItems");
            logs.add("COBOL file: " + cobolFile.getFileName() + " - Data items: " + debugItems);

            return program;
        } catch (Exception e) {
            logs.add("Failed to parse COBOL file " + cobolFile + ": " + e.getMessage());
            return null;
        }
    }
    
    /**
     * Extracts dependencies from COBOL source code
     */
    private void extractDependencies(String content, Set<String> dependencies) {
        // Extract CALL statements
        Matcher callMatcher = CALL_PATTERN.matcher(content);
        while (callMatcher.find()) {
            dependencies.add("CALL:" + callMatcher.group(1));
        }

        // Extract COPY statements
        Matcher copyMatcher = COPY_PATTERN.matcher(content);
        while (copyMatcher.find()) {
            dependencies.add("COPY:" + copyMatcher.group(1));
        }
    }

    /**
     * Extracts symbols from COBOL source code
     */
    private void extractSymbols(String content, Map<String, Object> symbols) {
        List<Map<String, Object>> dataItems = new ArrayList<>();
        List<String> sections = new ArrayList<>();
        List<String> paragraphs = new ArrayList<>();

        // Extract data items
        Matcher dataItemMatcher = DATA_ITEM_PATTERN.matcher(content);
        while (dataItemMatcher.find()) {
            Map<String, Object> item = new HashMap<>();
            item.put("level", dataItemMatcher.group(1));
            item.put("name", dataItemMatcher.group(2));
            item.put("picture", dataItemMatcher.group(3));
            item.put("type", "DATA_ITEM");
            dataItems.add(item);
        }
        
        // Extract sections and paragraphs (simplified)
        String[] lines = content.split("\n");
        for (String line : lines) {
            String trimmed = line.trim().toUpperCase();
            if (trimmed.endsWith(" SECTION.")) {
                sections.add(trimmed.replace(" SECTION.", ""));
            } else if (trimmed.matches("^[A-Z0-9-]+\\.$") && !trimmed.contains("DIVISION")) {
                paragraphs.add(trimmed.replace(".", ""));
            }
        }

        symbols.put("dataItems", dataItems);
        symbols.put("sections", sections);
        symbols.put("paragraphs", paragraphs);
    }
    
    /**
     * Converts a CobolProgram to an AST node representation
     */
    private Map<String, Object> programToAstNode(CobolProgram program) {
        Map<String, Object> node = new HashMap<>();
        node.put("type", "PROGRAM");
        node.put("name", program.getProgramName());
        node.put("id", program.getProgramId());
        node.put("metadata", program.getMetadata());
        return node;
    }

    /**
     * Builds a dependency graph from program dependencies
     */
    private Map<String, Object> buildDependencyGraph(Map<String, Set<String>> programDependencies) {
        Map<String, Object> graph = new HashMap<>();
        graph.put("nodes", programDependencies.keySet());

        List<Map<String, String>> edges = new ArrayList<>();
        for (Map.Entry<String, Set<String>> entry : programDependencies.entrySet()) {
            String from = entry.getKey();
            for (String to : entry.getValue()) {
                Map<String, String> edge = new HashMap<>();
                edge.put("from", from);
                edge.put("to", to);
                edges.add(edge);
            }
        }
        graph.put("edges", edges);

        return graph;
    }

    /**
     * Finds circular dependencies in the dependency graph
     */
    private List<String> findCircularDependencies(Map<String, Set<String>> programDependencies) {
        // Simple implementation - in production would use proper cycle detection
        List<String> circular = new ArrayList<>();

        for (Map.Entry<String, Set<String>> entry : programDependencies.entrySet()) {
            String program = entry.getKey();
            Set<String> deps = entry.getValue();

            for (String dep : deps) {
                if (programDependencies.containsKey(dep) &&
                    programDependencies.get(dep).contains(program)) {
                    circular.add(program + " <-> " + dep);
                }
            }
        }

        return circular;
    }

    /**
     * Categorizes symbols by their type
     */
    private Map<String, Object> categorizeSymbolsByType(Map<String, Map<String, Object>> allSymbols) {
        Map<String, Object> categorized = new HashMap<>();
        int totalDataItems = 0;
        int totalSections = 0;
        int totalParagraphs = 0;

        for (Map<String, Object> symbols : allSymbols.values()) {
            List<Map<String, Object>> dataItems = (List<Map<String, Object>>) symbols.get("dataItems");
            List<String> sections = (List<String>) symbols.get("sections");
            List<String> paragraphs = (List<String>) symbols.get("paragraphs");

            if (dataItems != null) totalDataItems += dataItems.size();
            if (sections != null) totalSections += sections.size();
            if (paragraphs != null) totalParagraphs += paragraphs.size();
        }

        categorized.put("totalDataItems", totalDataItems);
        categorized.put("totalSections", totalSections);
        categorized.put("totalParagraphs", totalParagraphs);
        categorized.put("totalSymbols", totalDataItems + totalSections + totalParagraphs);

        return categorized;
    }

    /**
     * Extracts data items from COBOL source (simplified pattern matching)
     */
    private List<Map<String, Object>> extractDataItems(String content) {
        List<Map<String, Object>> dataItems = new ArrayList<>();

        Matcher matcher = DATA_ITEM_PATTERN.matcher(content);
        while (matcher.find()) {
            Map<String, Object> item = new HashMap<>();
            item.put("level", matcher.group(1));
            item.put("name", matcher.group(2));
            item.put("picture", matcher.group(3));

            // Determine Java type based on picture clause
            String javaType = mapPictureToJavaType(matcher.group(3));
            item.put("javaType", javaType);

            dataItems.add(item);
        }

        return dataItems;
    }

    /**
     * Maps COBOL picture clause to Java type
     */
    private String mapPictureToJavaType(String picture) {
        if (picture == null) return "Object";

        picture = picture.toUpperCase();

        if (picture.startsWith("9")) {
            if (picture.contains("V")) {
                return "BigDecimal";
            } else if (picture.length() <= 9) {
                return "Integer";
            } else {
                return "Long";
            }
        } else if (picture.startsWith("X")) {
            return "String";
        } else if (picture.startsWith("A")) {
            return "String";
        }

        return "Object";
    }
}
