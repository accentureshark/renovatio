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
 * COBOL parsing service using basic pattern matching
 * In production, this would use ProLeap COBOL parser or Koopa
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
     * Basic COBOL parser - in production would use ProLeap or Koopa
     */
    private CobolProgram parseCobolFile(Path cobolFile) {
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