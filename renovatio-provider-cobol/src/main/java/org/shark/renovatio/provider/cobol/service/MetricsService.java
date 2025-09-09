package org.shark.renovatio.provider.cobol.service;

import org.shark.renovatio.shared.domain.MetricsResult;
import org.shark.renovatio.shared.domain.Scope;
import org.shark.renovatio.shared.domain.Workspace;
import org.springframework.stereotype.Service;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;

/**
 * Metrics calculation service for COBOL codebases
 * Calculates various code quality and complexity metrics
 */
@Service
public class MetricsService {
    
    /**
     * Calculates metrics for the given scope
     */
    public MetricsResult calculateMetrics(Scope scope, Workspace workspace) {
        try {
            Path workspacePath = Paths.get(workspace.getPath());
            
            CodeMetrics metrics = calculateCodeMetrics(workspacePath);
            
            Map<String, Number> metricsMap = new HashMap<>();
            metricsMap.put("totalFiles", metrics.getTotalFiles());
            metricsMap.put("totalLines", metrics.getTotalLines());
            metricsMap.put("totalPrograms", metrics.getTotalPrograms());
            metricsMap.put("totalDataItems", metrics.getTotalDataItems());
            metricsMap.put("totalParagraphs", metrics.getTotalParagraphs());
            metricsMap.put("averageLinesPerFile", metrics.getAverageLinesPerFile());
            metricsMap.put("cyclomaticComplexity", metrics.getCyclomaticComplexity());
            
            Map<String, Object> details = new HashMap<>();
            details.put("fileExtensions", metrics.getFileExtensions());
            details.put("largestFile", metrics.getLargestFile());
            details.put("largestFileLines", metrics.getLargestFileLines());
            details.put("migrationComplexity", assessMigrationComplexity(metrics));
            
            MetricsResult result = new MetricsResult(true, "Metrics calculated successfully");
            result.setMetrics(metricsMap);
            result.setDetails(details);
            
            return result;
            
        } catch (Exception e) {
            return new MetricsResult(false, "Metrics calculation failed: " + e.getMessage());
        }
    }
    
    /**
     * Calculates various code metrics for COBOL files
     */
    private CodeMetrics calculateCodeMetrics(Path workspacePath) throws IOException {
        CodeMetrics metrics = new CodeMetrics();
        AtomicInteger totalFiles = new AtomicInteger(0);
        AtomicLong totalLines = new AtomicLong(0);
        AtomicInteger totalPrograms = new AtomicInteger(0);
        AtomicInteger totalDataItems = new AtomicInteger(0);
        AtomicInteger totalParagraphs = new AtomicInteger(0);
        AtomicInteger totalComplexity = new AtomicInteger(0);
        
        Map<String, Integer> fileExtensions = new HashMap<>();
        String[] largestFile = new String[1];
        int[] largestFileLines = new int[1];
        
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
                    totalFiles.incrementAndGet();
                    
                    // Count file extension
                    String fileName = cobolFile.getFileName().toString().toLowerCase();
                    String extension = fileName.substring(fileName.lastIndexOf('.') + 1);
                    fileExtensions.merge(extension, 1, Integer::sum);
                    
                    String content = Files.readString(cobolFile);
                    String[] lines = content.split("\\n");
                    int lineCount = lines.length;
                    totalLines.addAndGet(lineCount);
                    
                    // Track largest file
                    if (lineCount > largestFileLines[0]) {
                        largestFileLines[0] = lineCount;
                        largestFile[0] = cobolFile.toString();
                    }
                    
                    // Count programs
                    if (content.toUpperCase().contains("PROGRAM-ID")) {
                        totalPrograms.incrementAndGet();
                    }
                    
                    // Count data items
                    totalDataItems.addAndGet(countDataItems(content));
                    
                    // Count paragraphs
                    totalParagraphs.addAndGet(countParagraphs(content));
                    
                    // Calculate cyclomatic complexity
                    totalComplexity.addAndGet(calculateCyclomaticComplexity(content));
                    
                } catch (IOException e) {
                    System.err.println("Failed to process file " + cobolFile + ": " + e.getMessage());
                }
            });
        
        metrics.setTotalFiles(totalFiles.get());
        metrics.setTotalLines(totalLines.get());
        metrics.setTotalPrograms(totalPrograms.get());
        metrics.setTotalDataItems(totalDataItems.get());
        metrics.setTotalParagraphs(totalParagraphs.get());
        metrics.setCyclomaticComplexity(totalComplexity.get());
        metrics.setFileExtensions(fileExtensions);
        metrics.setLargestFile(largestFile[0]);
        metrics.setLargestFileLines(largestFileLines[0]);
        
        if (totalFiles.get() > 0) {
            metrics.setAverageLinesPerFile((double) totalLines.get() / totalFiles.get());
        }
        
        return metrics;
    }
    
    /**
     * Counts data items in COBOL content
     */
    private int countDataItems(String content) {
        String[] lines = content.split("\\n");
        int count = 0;
        boolean inDataDivision = false;
        
        for (String line : lines) {
            String upperLine = line.toUpperCase();
            
            if (upperLine.contains("DATA DIVISION")) {
                inDataDivision = true;
                continue;
            }
            
            if (upperLine.contains("PROCEDURE DIVISION")) {
                break;
            }
            
            if (inDataDivision && line.trim().matches("^\\s*\\d{2}\\s+\\w+.*")) {
                count++;
            }
        }
        
        return count;
    }
    
    /**
     * Counts paragraphs in COBOL content
     */
    private int countParagraphs(String content) {
        String[] lines = content.split("\\n");
        int count = 0;
        boolean inProcedureDivision = false;
        
        for (String line : lines) {
            String upperLine = line.toUpperCase();
            
            if (upperLine.contains("PROCEDURE DIVISION")) {
                inProcedureDivision = true;
                continue;
            }
            
            if (inProcedureDivision) {
                String trimmedLine = line.trim();
                if (trimmedLine.endsWith(".") && 
                    !containsCobolVerb(trimmedLine) && 
                    trimmedLine.matches("^[A-Za-z][A-Za-z0-9-]*\\.$")) {
                    count++;
                }
            }
        }
        
        return count;
    }
    
    /**
     * Calculates cyclomatic complexity for COBOL content
     */
    private int calculateCyclomaticComplexity(String content) {
        int complexity = 1; // Base complexity
        String upperContent = content.toUpperCase();
        
        // Count decision points
        complexity += countOccurrences(upperContent, " IF ");
        complexity += countOccurrences(upperContent, " WHEN ");
        complexity += countOccurrences(upperContent, " PERFORM UNTIL ");
        complexity += countOccurrences(upperContent, " PERFORM VARYING ");
        complexity += countOccurrences(upperContent, " AND ");
        complexity += countOccurrences(upperContent, " OR ");
        
        return complexity;
    }
    
    /**
     * Counts occurrences of a substring
     */
    private int countOccurrences(String text, String substring) {
        int count = 0;
        int index = 0;
        while ((index = text.indexOf(substring, index)) != -1) {
            count++;
            index += substring.length();
        }
        return count;
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
     * Assesses migration complexity based on metrics
     */
    private String assessMigrationComplexity(CodeMetrics metrics) {
        int complexity = 0;
        
        // File count factor
        if (metrics.getTotalFiles() > 100) complexity += 3;
        else if (metrics.getTotalFiles() > 50) complexity += 2;
        else if (metrics.getTotalFiles() > 10) complexity += 1;
        
        // Lines of code factor
        if (metrics.getTotalLines() > 100000) complexity += 3;
        else if (metrics.getTotalLines() > 50000) complexity += 2;
        else if (metrics.getTotalLines() > 10000) complexity += 1;
        
        // Cyclomatic complexity factor
        double avgComplexity = (double) metrics.getCyclomaticComplexity() / Math.max(1, metrics.getTotalPrograms());
        if (avgComplexity > 20) complexity += 3;
        else if (avgComplexity > 10) complexity += 2;
        else if (avgComplexity > 5) complexity += 1;
        
        // Data item factor
        double avgDataItems = (double) metrics.getTotalDataItems() / Math.max(1, metrics.getTotalPrograms());
        if (avgDataItems > 100) complexity += 2;
        else if (avgDataItems > 50) complexity += 1;
        
        if (complexity >= 8) return "Very High";
        else if (complexity >= 6) return "High";
        else if (complexity >= 4) return "Medium";
        else if (complexity >= 2) return "Low";
        else return "Very Low";
    }
    
    /**
     * Code metrics data structure
     */
    private static class CodeMetrics {
        private int totalFiles;
        private long totalLines;
        private int totalPrograms;
        private int totalDataItems;
        private int totalParagraphs;
        private int cyclomaticComplexity;
        private double averageLinesPerFile;
        private Map<String, Integer> fileExtensions;
        private String largestFile;
        private int largestFileLines;
        
        // Getters and setters
        public int getTotalFiles() { return totalFiles; }
        public void setTotalFiles(int totalFiles) { this.totalFiles = totalFiles; }
        
        public long getTotalLines() { return totalLines; }
        public void setTotalLines(long totalLines) { this.totalLines = totalLines; }
        
        public int getTotalPrograms() { return totalPrograms; }
        public void setTotalPrograms(int totalPrograms) { this.totalPrograms = totalPrograms; }
        
        public int getTotalDataItems() { return totalDataItems; }
        public void setTotalDataItems(int totalDataItems) { this.totalDataItems = totalDataItems; }
        
        public int getTotalParagraphs() { return totalParagraphs; }
        public void setTotalParagraphs(int totalParagraphs) { this.totalParagraphs = totalParagraphs; }
        
        public int getCyclomaticComplexity() { return cyclomaticComplexity; }
        public void setCyclomaticComplexity(int cyclomaticComplexity) { this.cyclomaticComplexity = cyclomaticComplexity; }
        
        public double getAverageLinesPerFile() { return averageLinesPerFile; }
        public void setAverageLinesPerFile(double averageLinesPerFile) { this.averageLinesPerFile = averageLinesPerFile; }
        
        public Map<String, Integer> getFileExtensions() { return fileExtensions; }
        public void setFileExtensions(Map<String, Integer> fileExtensions) { this.fileExtensions = fileExtensions; }
        
        public String getLargestFile() { return largestFile; }
        public void setLargestFile(String largestFile) { this.largestFile = largestFile; }
        
        public int getLargestFileLines() { return largestFileLines; }
        public void setLargestFileLines(int largestFileLines) { this.largestFileLines = largestFileLines; }
    }
}