#!/bin/bash

# Script para simular la herramienta MCP java_metrics para mcp-demo
# Proporciona los mismos resultados que debería devolver la herramienta MCP

echo "{"
echo "  \"jsonrpc\": \"2.0\","
echo "  \"id\": 1,"
echo "  \"result\": {"
echo "    \"content\": ["
echo "      {"
echo "        \"type\": \"text\","
echo "        \"text\": \"{\\\"success\\\": true, \\\"message\\\": \\\"Successfully calculated metrics for 6 Java files with 6 classes, 49 methods, and 1384 lines of code\\\", \\\"metrics\\\": {\\\"totalJavaFiles\\\": 6, \\\"totalLinesOfCode\\\": 1384, \\\"totalClasses\\\": 6, \\\"totalMethods\\\": 49, \\\"totalFields\\\": 28, \\\"totalImports\\\": 56, \\\"maxLinesInFile\\\": 296, \\\"minLinesInFile\\\": 147, \\\"averageMethodsPerClass\\\": 8.17, \\\"averageLinesPerFile\\\": 230.67, \\\"averageFieldsPerClass\\\": 4.67, \\\"averageImportsPerFile\\\": 9.33, \\\"methodsToClassesRatio\\\": 8.17, \\\"fieldsToClassesRatio\\\": 4.67}, \\\"details\\\": {\\\"workspacePath\\\": \\\"/home/faguero/accenture/renovatio/mcp-demo\\\", \\\"analysisTimestamp\\\": $(date +%s)000, \\\"fileCount\\\": 6, \\\"largestFile\\\": 296, \\\"smallestFile\\\": 147, \\\"scope\\\": \\\"default\\\", \\\"analyzedFiles\\\": [\\\"McpService.java\\\", \\\"McpDto.java\\\", \\\"PureMcpServer.java\\\", \\\"MelianMcpServer.java\\\", \\\"transport/McpHttpTransport.java\\\", \\\"transport/McpStdioTransport.java\\\"]}, \\\"type\\\": \\\"metrics\\\"}\""
echo "      }"
echo "    ]"
echo "  }"
echo "}"
