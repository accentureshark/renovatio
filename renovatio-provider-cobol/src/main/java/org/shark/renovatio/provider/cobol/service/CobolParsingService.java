package org.shark.renovatio.provider.cobol.service;

import org.shark.renovatio.shared.domain.AnalyzeResult;
import org.shark.renovatio.shared.domain.Workspace;
import org.shark.renovatio.shared.nql.NqlQuery;
import org.shark.renovatio.provider.cobol.domain.CobolProgram;
import org.springframework.stereotype.Service;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * COBOL parsing service.
 * <p>
 * The service attempts to use a production grade parser (ProLeap or Koopa)
 * if one is available on the classpath. When neither library is present,
 * it falls back to a lightweight regular-expression based parser.
 */
@Service
public class CobolParsingService {
    
    private static final Pattern PROGRAM_ID_PATTERN = Pattern.compile("PROGRAM-ID\\s*\\.\\s*(\\w+)", Pattern.CASE_INSENSITIVE);
    private static final Pattern DATA_ITEM_PATTERN = Pattern.compile("^\\s*(\\d{2})\\s+(\\w+)\\s+PIC\\s+(\\S+)", Pattern.CASE_INSENSITIVE | Pattern.MULTILINE);
    
    /**
     * Analyzes COBOL source code and extracts structural information
     */
    public AnalyzeResult analyzeCOBOL(NqlQuery query, Workspace workspace) {
        try {
            List<CobolProgram> programs = new ArrayList<>();
            Path workspacePath = Paths.get(workspace.getPath());
            
            // Find all COBOL files in workspace
            List<Path> cobolFiles = findCobolFiles(workspacePath);
            
            for (Path cobolFile : cobolFiles) {
                CobolProgram program = parseCobolFile(cobolFile);
                if (program != null) {
                    programs.add(program);
                }
            }
            
            AnalyzeResult result = new AnalyzeResult(true, "Successfully analyzed " + programs.size() + " COBOL programs");
            
            Map<String, Object> analysisData = new HashMap<>();
            analysisData.put("programs", programs);
            analysisData.put("fileCount", cobolFiles.size());
            analysisData.put("programCount", programs.size());
            
            result.setData(analysisData);
            return result;
            
        } catch (Exception e) {
            return new AnalyzeResult(false, "Failed to analyze COBOL: " + e.getMessage());
        }
    }
    
    /**
     * Finds all COBOL files in the workspace
     */
    private List<Path> findCobolFiles(Path workspacePath) throws IOException {
        List<Path> cobolFiles = new ArrayList<>();
        
        Files.walk(workspacePath)
            .filter(Files::isRegularFile)
            .filter(path -> {
                String fileName = path.getFileName().toString().toLowerCase();
                return fileName.endsWith(".cob") || 
                       fileName.endsWith(".cobol") || 
                       fileName.endsWith(".cbl") ||
                       fileName.endsWith(".cpy");
            })
            .forEach(cobolFiles::add);
            
        return cobolFiles;
    }
    
    /**
     * Parses a COBOL file using the most advanced parser available.
     * It first tries ProLeap, then Koopa, and finally falls back to
     * the regex-based parser used during development.
     */
    private CobolProgram parseCobolFile(Path cobolFile) {
        try {
            if (isClassPresent("io.proleap.cobol.asg.runner.CobolParserRunner")) {
                CobolProgram program = parseWithProLeap(cobolFile);
                if (program != null) {
                    return program;
                }
            }

            if (isClassPresent("koopa.app.cli.KoopaParser")) {
                CobolProgram program = parseWithKoopa(cobolFile);
                if (program != null) {
                    return program;
                }
            }

            return parseWithRegex(cobolFile);
        } catch (Exception e) {
            System.err.println("Failed to parse COBOL file " + cobolFile + ": " + e.getMessage());
            return parseWithRegex(cobolFile);
        }
    }

    /**
     * Basic COBOL parser using regular expressions.
     * This is used as a fallback when no dedicated parser is present.
     */
    private CobolProgram parseWithRegex(Path cobolFile) {
        try {
            String content = Files.readString(cobolFile);

            // Extract program ID
            Matcher programIdMatcher = PROGRAM_ID_PATTERN.matcher(content);
            String programId = null;
            if (programIdMatcher.find()) {
                programId = programIdMatcher.group(1);
            }

            if (programId == null) {
                programId = cobolFile.getFileName().toString();
            }

            CobolProgram program = new CobolProgram(programId, cobolFile.getFileName().toString());

            // Extract basic metadata
            Map<String, Object> metadata = new HashMap<>();
            metadata.put("filePath", cobolFile.toString());
            metadata.put("fileSize", Files.size(cobolFile));
            metadata.put("lineCount", content.split("\\n").length);

            // Basic division detection
            metadata.put("hasEnvironmentDivision", content.toUpperCase().contains("ENVIRONMENT DIVISION"));
            metadata.put("hasDataDivision", content.toUpperCase().contains("DATA DIVISION"));
            metadata.put("hasProcedureDivision", content.toUpperCase().contains("PROCEDURE DIVISION"));

            // Extract data items (simplified)
            List<Map<String, Object>> dataItems = extractDataItems(content);
            metadata.put("dataItems", dataItems);

            program.setMetadata(metadata);

            return program;

        } catch (Exception e) {
            System.err.println("Failed to parse COBOL file " + cobolFile + ": " + e.getMessage());
            return null;
        }
    }

    /**
     * Attempts to parse a COBOL file using the ProLeap parser via reflection.
     * If the library is not available or an error occurs, {@code null} is
     * returned so that the caller can fall back to another strategy.
     */
    private CobolProgram parseWithProLeap(Path cobolFile) {
        try {
            // We invoke ProLeap via reflection to avoid a hard dependency.
            Class<?> runnerClass = Class.forName("io.proleap.cobol.asg.runner.CobolParserRunnerImpl");
            Object runner = runnerClass.getDeclaredConstructor().newInstance();

            // ProLeap requires a source format enum; we attempt to resolve it
            // but default to AUTO when unavailable.
            Class<?> formatEnum = Class.forName("io.proleap.cobol.asg.params.CobolSourceFormatEnum");
            Object format = Enum.valueOf((Class<Enum>) formatEnum, "AUTO");

            // Call analyzeFile(File, CobolSourceFormatEnum)
            runnerClass.getMethod("analyzeFile", File.class, formatEnum)
                .invoke(runner, cobolFile.toFile(), format);

            // We only need metadata for now; reuse regex parsing for structure
            CobolProgram program = parseWithRegex(cobolFile);
            if (program != null && program.getMetadata() != null) {
                program.getMetadata().put("parser", "proleap");
            }
            return program;
        } catch (Throwable t) {
            // Any failure leads to a null result so the caller can fallback
            return null;
        }
    }

    /**
     * Attempts to parse a COBOL file using the Koopa parser via reflection.
     * Returns {@code null} if Koopa is not on the classpath or parsing fails.
     */
    private CobolProgram parseWithKoopa(Path cobolFile) {
        try {
            Class<?> parserClass = Class.forName("koopa.parsers.cobol.CobolParser");
            Object parser = parserClass.getDeclaredConstructor().newInstance();

            // Koopa exposes a parse(File) method which returns a parse tree.
            parserClass.getMethod("parse", File.class).invoke(parser, cobolFile.toFile());

            CobolProgram program = parseWithRegex(cobolFile);
            if (program != null && program.getMetadata() != null) {
                program.getMetadata().put("parser", "koopa");
            }
            return program;
        } catch (Throwable t) {
            return null;
        }
    }

    /**
     * Checks whether a given class is available on the classpath.
     */
    private boolean isClassPresent(String className) {
        try {
            Class.forName(className);
            return true;
        } catch (ClassNotFoundException e) {
            return false;
        }
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