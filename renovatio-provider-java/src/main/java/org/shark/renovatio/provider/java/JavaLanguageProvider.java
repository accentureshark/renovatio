package org.shark.renovatio.provider.java;

import org.shark.renovatio.shared.spi.BaseLanguageProvider;
import org.shark.renovatio.shared.domain.*;
import org.shark.renovatio.shared.nql.NqlQuery;

import java.io.IOException;
import java.nio.file.*;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

public class JavaLanguageProvider extends BaseLanguageProvider {

    // Patterns for basic Java analysis
    private static final Pattern CLASS_PATTERN = Pattern.compile(
        "(?:public\\s+)?(?:abstract\\s+)?(?:final\\s+)?(?:class|interface|enum)\\s+(\\w+)(?:\\s+extends\\s+(\\w+))?(?:\\s+implements\\s+([\\w\\s,]+))?\\s*\\{");

    private static final Pattern METHOD_PATTERN = Pattern.compile(
        "(?:public|private|protected)?\\s*(?:static\\s+)?(?:final\\s+)?(?:abstract\\s+)?(\\w+(?:<[^>]+>)?(?:\\[\\])?(?:\\s*\\.\\.\\.)?\\s+)(\\w+)\\s*\\([^)]*\\)\\s*(?:throws\\s+[^{]+)?\\s*[{;]");

    private static final Pattern FIELD_PATTERN = Pattern.compile(
        "(?:public|private|protected)?\\s*(?:static\\s+)?(?:final\\s+)?(\\w+(?:<[^>]+>)?(?:\\[\\])?\\s+)(\\w+)\\s*[=;]");

    private static final Pattern IMPORT_PATTERN = Pattern.compile(
        "import\\s+(?:static\\s+)?((?:[\\w\\.]+(?:\\.\\*)?));");

    public JavaLanguageProvider() {}


    @Override
    public String language() {
        return "java";
    }

    @Override
    public Set<Capabilities> capabilities() {
        return EnumSet.of(Capabilities.ANALYZE, Capabilities.PLAN, Capabilities.APPLY, Capabilities.DIFF, Capabilities.METRICS);
    }

    @Override
    public AnalyzeResult analyze(NqlQuery query, Workspace workspace) {
        AnalyzeResult result = new AnalyzeResult();

        // Initialize result with default values to avoid null returns
        result.setSuccess(false);
        result.setMessage("Analysis not completed");
        result.setRunId(generateRunId());
        result.setData(new HashMap<>());
        result.setAst(new HashMap<>());
        result.setDependencies(new HashMap<>());
        result.setSymbols(new HashMap<>());

        // Add detailed logging for debugging
        System.out.println("=== JavaLanguageProvider.analyze DEBUG ===");
        System.out.println("Workspace path: " + workspace.getPath());
        System.out.println("Query: " + (query != null ? query.getOriginalQuery() : "null"));

        try {
            // Validate input parameters
            if (workspace == null || workspace.getPath() == null) {
                result.setMessage("Invalid workspace: workspace or path is null");
                System.out.println("ERROR: Invalid workspace");
                return result;
            }

            Path workspacePath = Paths.get(workspace.getPath());
            System.out.println("Resolved workspace path: " + workspacePath.toAbsolutePath());
            System.out.println("Path exists: " + Files.exists(workspacePath));
            System.out.println("Is directory: " + Files.isDirectory(workspacePath));

            if (!Files.exists(workspacePath)) {
                result.setMessage("Workspace path does not exist: " + workspace.getPath());
                System.out.println("ERROR: Workspace path does not exist");
                return result;
            }

            // Find all Java files
            List<Path> javaFiles = findJavaFiles(workspacePath);
            System.out.println("Found Java files: " + javaFiles.size());
            for (Path file : javaFiles) {
                System.out.println("  - " + file.toString());
            }

            if (javaFiles.isEmpty()) {
                result.setMessage("No Java files found in workspace: " + workspace.getPath());
                System.out.println("WARNING: No Java files found, but analysis considered successful");
                // Change this to success=true for empty directories
                result.setSuccess(true);
                return result;
            }

            // Analyze each Java file
            Map<String, Object> analysisData = new HashMap<>();
            List<String> classes = new ArrayList<>();
            List<String> methods = new ArrayList<>();
            List<String> imports = new ArrayList<>();

            for (Path javaFile : javaFiles) {
                try {
                    String content = Files.readString(javaFile);
                    String relativePath = workspacePath.relativize(javaFile).toString();
                    System.out.println("Analyzing file: " + relativePath + " (size: " + content.length() + " chars)");

                    // Analyze this file
                    Map<String, Object> fileAnalysis = analyzeJavaFile(content, relativePath);
                    analysisData.put(relativePath, fileAnalysis);

                    // Collect all classes, methods, imports
                    @SuppressWarnings("unchecked")
                    List<String> fileClasses = (List<String>) fileAnalysis.get("classes");
                    if (fileClasses != null) {
                        classes.addAll(fileClasses);
                        System.out.println("  Found classes: " + fileClasses);
                    }

                    @SuppressWarnings("unchecked")
                    List<String> fileMethods = (List<String>) fileAnalysis.get("methods");
                    if (fileMethods != null) {
                        methods.addAll(fileMethods);
                        System.out.println("  Found methods: " + fileMethods.size());
                    }

                    @SuppressWarnings("unchecked")
                    List<String> fileImports = (List<String>) fileAnalysis.get("imports");
                    if (fileImports != null) {
                        imports.addAll(fileImports);
                        System.out.println("  Found imports: " + fileImports.size());
                    }

                } catch (IOException e) {
                    // Skip files that can't be read but don't fail the entire analysis
                    System.err.println("Could not read Java file: " + javaFile + " - " + e.getMessage());
                }
            }

            // Build summary
            Map<String, Object> summary = new HashMap<>();
            summary.put("totalJavaFiles", javaFiles.size());
            summary.put("totalClasses", classes.size());
            summary.put("totalMethods", methods.size());
            summary.put("totalImports", imports.size());
            summary.put("workspacePath", workspace.getPath());
            summary.put("analysisTimestamp", System.currentTimeMillis());

            // Set successful result
            result.setSuccess(true);
            result.setMessage("Successfully analyzed " + javaFiles.size() + " Java files with " +
                            classes.size() + " classes and " + methods.size() + " methods");
            result.setData(analysisData);
            result.setAst(summary);

            // Set dependencies (convert List to Map)
            Map<String, Object> dependenciesMap = new HashMap<>();
            dependenciesMap.put("imports", imports.stream().distinct().collect(Collectors.toList()));
            dependenciesMap.put("totalImports", imports.size());
            dependenciesMap.put("uniqueImports", imports.stream().distinct().count());
            result.setDependencies(dependenciesMap);

            // Set symbols (convert List to Map)
            Map<String, Object> symbolsMap = new HashMap<>();
            symbolsMap.put("classes", classes);
            symbolsMap.put("methods", methods);
            symbolsMap.put("totalSymbols", classes.size() + methods.size());
            result.setSymbols(symbolsMap);

            System.out.println("Analysis completed successfully:");
            System.out.println("  Files analyzed: " + javaFiles.size());
            System.out.println("  Classes found: " + classes.size());
            System.out.println("  Methods found: " + methods.size());
            System.out.println("  Imports found: " + imports.size());

        } catch (Exception e) {
            result.setSuccess(false);
            result.setMessage("Error analyzing Java code: " + e.getMessage());
            System.err.println("EXCEPTION in JavaLanguageProvider.analyze: " + e.getMessage());
            e.printStackTrace();

            // Ensure we have some basic data even on error
            Map<String, Object> errorData = new HashMap<>();
            errorData.put("error", e.getMessage());
            errorData.put("errorType", e.getClass().getSimpleName());
            result.setData(errorData);
        }

        return result;
    }

    private List<Path> findJavaFiles(Path startPath) throws IOException {
        System.out.println("=== findJavaFiles DEBUG ===");
        System.out.println("Start path: " + startPath.toAbsolutePath());
        System.out.println("Path exists: " + Files.exists(startPath));
        System.out.println("Is directory: " + Files.isDirectory(startPath));

        if (!Files.exists(startPath)) {
            System.out.println("ERROR: Start path does not exist");
            return Collections.emptyList();
        }

        if (Files.isRegularFile(startPath) && startPath.toString().endsWith(".java")) {
            System.out.println("Single Java file detected: " + startPath);
            return Arrays.asList(startPath);
        }

        if (Files.isDirectory(startPath)) {
            System.out.println("Scanning directory for Java files...");
            List<Path> javaFiles = Files.walk(startPath)
                    .filter(Files::isRegularFile)
                    .filter(path -> path.toString().endsWith(".java"))
                    .collect(Collectors.toList());

            System.out.println("Found " + javaFiles.size() + " Java files:");
            for (Path file : javaFiles) {
                System.out.println("  - " + file.toAbsolutePath());
            }

            return javaFiles;
        }

        System.out.println("ERROR: Path is neither file nor directory");
        return Collections.emptyList();
    }

    private Map<String, Object> analyzeJavaFile(String content, String filePath) {
        Map<String, Object> analysis = new HashMap<>();

        // Extract package
        Pattern packagePattern = Pattern.compile("package\\s+([\\w\\.]+);");
        Matcher packageMatcher = packagePattern.matcher(content);
        if (packageMatcher.find()) {
            analysis.put("package", packageMatcher.group(1));
        }

        // Extract imports
        List<String> imports = new ArrayList<>();
        Matcher importMatcher = IMPORT_PATTERN.matcher(content);
        while (importMatcher.find()) {
            imports.add(importMatcher.group(1));
        }
        analysis.put("imports", imports);

        // Extract classes/interfaces/enums
        List<String> classes = new ArrayList<>();
        Matcher classMatcher = CLASS_PATTERN.matcher(content);
        while (classMatcher.find()) {
            String className = classMatcher.group(1);
            classes.add(className);
        }
        analysis.put("classes", classes);

        // Extract methods
        List<String> methods = new ArrayList<>();
        Matcher methodMatcher = METHOD_PATTERN.matcher(content);
        while (methodMatcher.find()) {
            String returnType = methodMatcher.group(1).trim();
            String methodName = methodMatcher.group(2);
            methods.add(returnType + " " + methodName + "()");
        }
        analysis.put("methods", methods);

        // Extract fields
        List<String> fields = new ArrayList<>();
        Matcher fieldMatcher = FIELD_PATTERN.matcher(content);
        while (fieldMatcher.find()) {
            String fieldType = fieldMatcher.group(1).trim();
            String fieldName = fieldMatcher.group(2);
            fields.add(fieldType + " " + fieldName);
        }
        analysis.put("fields", fields);

        // Basic metrics
        Map<String, Object> metrics = new HashMap<>();
        metrics.put("linesOfCode", content.split("\n").length);
        metrics.put("classCount", classes.size());
        metrics.put("methodCount", methods.size());
        metrics.put("fieldCount", fields.size());
        metrics.put("importCount", imports.size());
        analysis.put("metrics", metrics);

        analysis.put("filePath", filePath);

        return analysis;
    }

    @Override
    public PlanResult plan(NqlQuery query, Scope scope, Workspace workspace) {
        PlanResult result = new PlanResult();
        result.setSuccess(true);
        result.setMessage("Java refactoring plan generated (placeholder implementation)");
        result.setPlanId("java-plan-" + System.currentTimeMillis());
        result.setPlanContent("# Java Refactoring Plan\n\nQuery: " + query.getOriginalQuery() + "\nWorkspace: " + workspace.getPath());

        // Convert List to Map for steps
        Map<String, Object> stepsMap = new HashMap<>();
        stepsMap.put("step1", "Analyze code structure");
        stepsMap.put("step2", "Identify refactoring opportunities");
        stepsMap.put("step3", "Generate transformation rules");
        result.setSteps(stepsMap);

        return result;
    }

    @Override
    public ApplyResult apply(String planId, boolean dryRun, Workspace workspace) {
        ApplyResult result = new ApplyResult();
        result.setSuccess(true);
        result.setMessage("Java transformations applied (placeholder implementation)");
        result.setDryRun(dryRun);
        result.setDiff("No changes made (placeholder implementation)");

        // Convert List to Map for changes
        Map<String, Object> changesMap = new HashMap<>();
        changesMap.put("change1", "Updated method signature");
        result.setChanges(changesMap);

        return result;
    }

    @Override
    public DiffResult diff(String runId, Workspace workspace) {
        DiffResult result = new DiffResult();
        result.setSuccess(true);
        result.setMessage("Java diff generated (placeholder implementation)");
        result.setUnifiedDiff("--- a/Example.java\n+++ b/Example.java\n@@ -1,3 +1,3 @@\n-// Old code\n+// New code");

        // Convert String to Map for semanticDiff
        Map<String, Object> semanticDiffMap = new HashMap<>();
        semanticDiffMap.put("changes", "Semantic changes: method signature updated");
        semanticDiffMap.put("type", "method_signature_change");
        result.setSemanticDiff(semanticDiffMap);

        // Convert List to Map for hunks
        Map<String, Object> hunksMap = new HashMap<>();
        hunksMap.put("hunk1", "Method signature change");
        result.setHunks(hunksMap);

        return result;
    }

    @Override
    public Optional<StubResult> generateStubs(NqlQuery query, Workspace workspace) {
        return Optional.empty();
    }

    @Override
    public MetricsResult metrics(Scope scope, Workspace workspace) {
        MetricsResult result = new MetricsResult();
        Map<String, Number> metricsMap = new HashMap<>();

        System.out.println("=== JavaLanguageProvider.metrics DEBUG ===");
        System.out.println("Workspace path: " + (workspace != null ? workspace.getPath() : "null"));
        System.out.println("Scope paths: " + (scope != null && scope.getPaths() != null ? scope.getPaths() : "null"));

        try {
            // Validate input parameters
            if (workspace == null || workspace.getPath() == null || workspace.getPath().trim().isEmpty()) {
                result.setSuccess(false);
                result.setMessage("Invalid workspace: workspace or path is null/empty");
                System.out.println("ERROR: Invalid workspace");
                metricsMap.put("error_occurred", 1);
                result.setMetrics(metricsMap);
                return result;
            }

            Path workspacePath = Paths.get(workspace.getPath());
            System.out.println("Resolved workspace path: " + workspacePath.toAbsolutePath());
            System.out.println("Path exists: " + Files.exists(workspacePath));
            System.out.println("Is directory: " + Files.isDirectory(workspacePath));

            if (!Files.exists(workspacePath)) {
                result.setSuccess(false);
                result.setMessage("Workspace path does not exist: " + workspace.getPath());
                System.out.println("ERROR: Workspace path does not exist");
                metricsMap.put("error_occurred", 1);
                result.setMetrics(metricsMap);
                return result;
            }

            // Find all Java files
            List<Path> javaFiles = findJavaFiles(workspacePath);
            System.out.println("Found Java files for metrics: " + javaFiles.size());

            // Even if no Java files, return success with zero metrics
            if (javaFiles.isEmpty()) {
                System.out.println("No Java files found, returning zero metrics");
                metricsMap.put("total_files", 0);
                metricsMap.put("total_classes", 0);
                metricsMap.put("total_methods", 0);
                metricsMap.put("lines_of_code", 0);
                result.setSuccess(true);
                result.setMessage("No Java files found in workspace, but metrics calculation completed successfully");
                result.setMetrics(metricsMap);
                return result;
            }

            // Initialize metrics counters
            int totalFiles = javaFiles.size();
            int totalClasses = 0;
            int totalMethods = 0;
            int totalFields = 0;
            int totalImports = 0;
            int totalLinesOfCode = 0;
            int totalBlankLines = 0;
            int totalCommentLines = 0;
            double totalComplexity = 0.0;

            // Calculate detailed metrics for each file
            for (Path javaFile : javaFiles) {
                try {
                    String content = Files.readString(javaFile);
                    String relativePath = workspacePath.relativize(javaFile).toString();
                    System.out.println("Calculating metrics for file: " + relativePath);

                    // Analyze this file for metrics
                    Map<String, Object> fileAnalysis = analyzeJavaFile(content, relativePath);

                    // Extract metrics from file analysis
                    @SuppressWarnings("unchecked")
                    Map<String, Object> fileMetrics = (Map<String, Object>) fileAnalysis.get("metrics");
                    if (fileMetrics != null) {
                        totalClasses += (Integer) fileMetrics.getOrDefault("classCount", 0);
                        totalMethods += (Integer) fileMetrics.getOrDefault("methodCount", 0);
                        totalFields += (Integer) fileMetrics.getOrDefault("fieldCount", 0);
                        totalImports += (Integer) fileMetrics.getOrDefault("importCount", 0);
                        totalLinesOfCode += (Integer) fileMetrics.getOrDefault("linesOfCode", 0);
                    }

                    // Calculate additional metrics
                    String[] lines = content.split("\n");
                    for (String line : lines) {
                        String trimmedLine = line.trim();
                        if (trimmedLine.isEmpty()) {
                            totalBlankLines++;
                        } else if (trimmedLine.startsWith("//") || trimmedLine.startsWith("/*") || trimmedLine.startsWith("*")) {
                            totalCommentLines++;
                        }
                    }

                    // Simple cyclomatic complexity estimation
                    totalComplexity += calculateCyclomaticComplexity(content);

                } catch (IOException e) {
                    System.err.println("Could not read Java file for metrics: " + javaFile + " - " + e.getMessage());
                    // Continue with other files, don't fail entirely
                }
            }

            // Calculate derived metrics
            double avgMethodsPerClass = totalClasses > 0 ? (double) totalMethods / totalClasses : 0.0;
            double avgLinesPerFile = totalFiles > 0 ? (double) totalLinesOfCode / totalFiles : 0.0;
            double avgLinesPerMethod = totalMethods > 0 ? (double) totalLinesOfCode / totalMethods : 0.0;
            double commentRatio = totalLinesOfCode > 0 ? (double) totalCommentLines / totalLinesOfCode : 0.0;
            double avgComplexityPerMethod = totalMethods > 0 ? totalComplexity / totalMethods : 0.0;

            // Populate metrics map with comprehensive Java quality indicators
            metricsMap.put("total_files", totalFiles);
            metricsMap.put("total_classes", totalClasses);
            metricsMap.put("total_methods", totalMethods);
            metricsMap.put("total_fields", totalFields);
            metricsMap.put("total_imports", totalImports);
            metricsMap.put("lines_of_code", totalLinesOfCode);
            metricsMap.put("blank_lines", totalBlankLines);
            metricsMap.put("comment_lines", totalCommentLines);
            metricsMap.put("cyclomatic_complexity", totalComplexity);
            metricsMap.put("avg_methods_per_class", avgMethodsPerClass);
            metricsMap.put("avg_lines_per_file", avgLinesPerFile);
            metricsMap.put("avg_lines_per_method", avgLinesPerMethod);
            metricsMap.put("comment_ratio", commentRatio);
            metricsMap.put("avg_complexity_per_method", avgComplexityPerMethod);

            // Quality indicators
            metricsMap.put("maintainability_index", calculateMaintainabilityIndex(totalLinesOfCode, totalComplexity, totalCommentLines));
            metricsMap.put("code_coverage_potential", calculateCodeCoveragePotential(totalMethods, totalClasses));

            // Set successful result - THIS IS CRITICAL
            result.setSuccess(true);
            result.setMessage("Successfully calculated metrics for " + totalFiles + " Java files: " +
                            totalClasses + " classes, " + totalMethods + " methods, " + totalLinesOfCode + " LOC");
            result.setMetrics(metricsMap);

            System.out.println("Metrics calculation completed successfully:");
            System.out.println("  Files: " + totalFiles);
            System.out.println("  Classes: " + totalClasses);
            System.out.println("  Methods: " + totalMethods);
            System.out.println("  Lines of Code: " + totalLinesOfCode);
            System.out.println("  SUCCESS FLAG SET TO: " + result.isSuccess());

        } catch (Exception e) {
            System.err.println("EXCEPTION in JavaLanguageProvider.metrics: " + e.getMessage());
            e.printStackTrace();

            result.setSuccess(false);
            result.setMessage("Error calculating Java metrics: " + e.getMessage());

            // Set basic error metrics
            metricsMap.put("error_occurred", 1);
            metricsMap.put("total_files", 0);
            result.setMetrics(metricsMap);
        }

        System.out.println("Returning metrics result with success=" + result.isSuccess());
        return result;
    }

    /**
     * Calculate cyclomatic complexity for Java code using basic pattern matching
     */
    private double calculateCyclomaticComplexity(String content) {
        double complexity = 1.0; // Base complexity

        // Count decision points that increase complexity
        String[] complexityKeywords = {
            "if\\s*\\(", "else\\s+if\\s*\\(", "while\\s*\\(", "for\\s*\\(",
            "do\\s*\\{", "switch\\s*\\(", "case\\s+", "catch\\s*\\(",
            "\\?\\s*", "&&", "\\|\\|"
        };

        for (String keyword : complexityKeywords) {
            Pattern pattern = Pattern.compile(keyword);
            Matcher matcher = pattern.matcher(content);
            while (matcher.find()) {
                complexity += 1.0;
            }
        }

        return complexity;
    }

    /**
     * Calculate maintainability index (simplified version)
     * Higher values indicate better maintainability
     */
    private double calculateMaintainabilityIndex(int linesOfCode, double complexity, int commentLines) {
        if (linesOfCode == 0) return 100.0;

        // Simplified maintainability index calculation
        double volume = linesOfCode * Math.log(linesOfCode + 1); // Halstead volume approximation
        double commentRatio = commentLines > 0 ? (double) commentLines / linesOfCode : 0.0;

        double maintainabilityIndex = Math.max(0,
            171 - 5.2 * Math.log(volume) - 0.23 * complexity - 16.2 * Math.log(linesOfCode) + 50 * Math.sin(Math.sqrt(2.4 * commentRatio))
        );

        return Math.min(100.0, maintainabilityIndex);
    }

    /**
     * Calculate code coverage potential based on methods and classes
     */
    private double calculateCodeCoveragePotential(int totalMethods, int totalClasses) {
        if (totalMethods == 0 && totalClasses == 0) return 0.0;

        // Simple heuristic: more methods relative to classes suggests better testability
        double methodToClassRatio = totalClasses > 0 ? (double) totalMethods / totalClasses : totalMethods;

        // Normalize to 0-100 scale, with diminishing returns for very high ratios
        return Math.min(100.0, 20.0 + (methodToClassRatio * 10.0) / (1.0 + methodToClassRatio * 0.1));
    }
}
