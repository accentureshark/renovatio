#!/bin/bash

# Test script to validate Java metrics functionality after fixing the implementation
echo "=== Testing Java Metrics Implementation (Fixed) ==="

# Set up environment
export JAVA_HOME=${JAVA_HOME:-/usr/lib/jvm/java-17-openjdk-amd64}
export PATH=$JAVA_HOME/bin:$PATH

cd /home/faguero/accenture/renovatio

echo "Building project to ensure latest changes are included..."
mvn clean compile -q -DskipTests

echo -e "\n=== Testing JavaLanguageProvider.metrics directly ==="

# Create a direct Java test for metrics
cat > TestJavaMetrics.java << 'EOF'
import org.shark.renovatio.provider.java.JavaLanguageProvider;
import org.shark.renovatio.shared.domain.*;
import java.util.Map;

public class TestJavaMetrics {
    public static void main(String[] args) {
        try {
            System.out.println("=== Direct Java Metrics Test ===");

            JavaLanguageProvider provider = new JavaLanguageProvider();

            // Create workspace pointing to mcp-demo directory
            Workspace workspace = new Workspace();
            workspace.setId("test-metrics");
            workspace.setPath("/home/faguero/accenture/renovatio/mcp-demo");
            workspace.setBranch("main");

            // Create scope (can be null for whole workspace)
            Scope scope = new Scope();

            System.out.println("Provider language: " + provider.language());
            System.out.println("Provider capabilities: " + provider.capabilities());
            System.out.println("Workspace path: " + workspace.getPath());

            // Call metrics method
            System.out.println("Calling provider.metrics()...");
            MetricsResult result = provider.metrics(scope, workspace);

            System.out.println("\n=== Metrics Result ===");
            System.out.println("Success: " + result.isSuccess());
            System.out.println("Message: " + result.getMessage());

            Map<String, Number> metrics = result.getMetrics();
            if (metrics != null && !metrics.isEmpty()) {
                System.out.println("\n=== Detailed Metrics ===");
                metrics.entrySet().stream()
                    .sorted(Map.Entry.comparingByKey())
                    .forEach(entry ->
                        System.out.printf("%-25s: %s%n", entry.getKey(), entry.getValue())
                    );

                // Highlight key metrics
                System.out.println("\n=== Key Quality Indicators ===");
                System.out.println("Files analyzed: " + metrics.get("total_files"));
                System.out.println("Classes found: " + metrics.get("total_classes"));
                System.out.println("Methods found: " + metrics.get("total_methods"));
                System.out.println("Lines of code: " + metrics.get("lines_of_code"));
                System.out.println("Cyclomatic complexity: " + metrics.get("cyclomatic_complexity"));

                Number avgComplexity = metrics.get("avg_complexity_per_method");
                if (avgComplexity != null) {
                    System.out.printf("Average complexity per method: %.2f%n", avgComplexity.doubleValue());
                }

                Number maintainability = metrics.get("maintainability_index");
                if (maintainability != null) {
                    System.out.printf("Maintainability index: %.2f%n", maintainability.doubleValue());
                }

            } else {
                System.out.println("No metrics returned!");
            }

        } catch (Exception e) {
            System.err.println("Error: " + e.getMessage());
            e.printStackTrace();
        }
    }
}
EOF

# Compile and run the test
echo "Compiling direct test..."
javac -cp "$(find . -name "*.jar" | tr '\n' ':')target/classes:renovatio-shared/target/classes:renovatio-provider-java/target/classes" TestJavaMetrics.java

echo "Running direct test..."
java -cp ".:$(find . -name "*.jar" | tr '\n' ':')target/classes:renovatio-shared/target/classes:renovatio-provider-java/target/classes" TestJavaMetrics

echo -e "\n=== Testing via MCP Server ==="

# Start MCP server in background for testing
echo "Starting MCP server for testing..."
cd /home/faguero/accenture/renovatio/renovatio-mcp-server

# Kill any existing server
pkill -f "renovatio-mcp-server" 2>/dev/null || true
sleep 2

# Start server in background
nohup java -jar target/renovatio-mcp-server-0.0.1-SNAPSHOT.jar > test-server.log 2>&1 &
SERVER_PID=$!
echo "MCP Server started with PID: $SERVER_PID"

# Wait for server to start
echo "Waiting for server to start..."
sleep 5

# Test metrics endpoint via HTTP
echo "Testing java.metrics tool via HTTP..."
curl -X POST http://localhost:8080/mcp/call \
  -H "Content-Type: application/json" \
  -d '{
    "jsonrpc": "2.0",
    "id": "test-metrics",
    "method": "tools/call",
    "params": {
      "name": "java.metrics",
      "arguments": {
        "workspacePath": "/home/faguero/accenture/renovatio/mcp-demo",
        "scope": "/home/faguero/accenture/renovatio/mcp-demo",
        "nql": "metrics for all classes, methods, and the project as a whole, including lines of code (LOC), number of classes, number of methods, cyclomatic complexity, and other standard Java quality indicators"
      }
    }
  }' | python3 -m json.tool

echo -e "\n=== Cleanup ==="
kill $SERVER_PID 2>/dev/null || true
rm -f TestJavaMetrics.java TestJavaMetrics.class
cd ..

echo -e "\n=== Test Complete ==="
echo "Check the output above to verify that:"
echo "1. Direct provider test shows successful metrics calculation"
echo "2. MCP server responds with metrics data (success=true)"
echo "3. Metrics include: total_files, total_classes, total_methods, lines_of_code, cyclomatic_complexity"
