# Renovatio Client

Standalone MCP client for interacting with Renovatio servers and testing integrations.

## Overview

`renovatio-client` is a lightweight MCP client that provides:

- **MCP protocol implementation**: Full JSON-RPC 2.0 client
- **Command-line interface**: CLI for testing and automation
- **Integration testing**: Validate MCP server implementations
- **Scripting support**: Batch operations and automation

## Features

### MCP Client
- Protocol-compliant JSON-RPC 2.0 implementation
- HTTP and STDIO transport support
- Automatic tool discovery and invocation
- Session management and connection handling

### CLI Interface
- Interactive and batch modes
- Tool listing and description
- Direct tool execution
- Result formatting and output

### Testing & Validation
- MCP server compatibility testing
- Protocol compliance validation
- Performance benchmarking
- Integration test automation

## Usage

### Start Interactive CLI
```bash
cd renovatio-client
mvn exec:java -Dexec.mainClass="org.shark.renovatio.client.RenovatioMcpClient"
```

### Command Examples

List available tools:
```bash
java -jar renovatio-client.jar --server http://localhost:8080 --command tools:list
```

Execute a tool:
```bash
java -jar renovatio-client.jar \
  --server http://localhost:8080 \
  --command tools:call \
  --tool java_analyze \
  --args '{"workspacePath": "/path/to/project", "nql": "FIND classes"}'
```

Batch execution:
```bash
java -jar renovatio-client.jar \
  --server http://localhost:8080 \
  --batch-file commands.json
```

### Batch File Format
```json
{
  "commands": [
    {
      "method": "tools/call",
      "params": {
        "name": "java_discover",
        "arguments": {"workspacePath": "/project1"}
      }
    },
    {
      "method": "tools/call", 
      "params": {
        "name": "java_analyze",
        "arguments": {
          "workspacePath": "/project1",
          "nql": "FIND classes WHERE complexity > 10"
        }
      }
    }
  ]
}
```

## Configuration

### Client Configuration
```yaml
renovatio:
  client:
    server-url: "http://localhost:8080"
    timeout: 30000
    retries: 3
    output-format: "json" # or "yaml", "table"
```

### Connection Options
```bash
# HTTP connection
--server http://localhost:8080

# STDIO connection (for process-based servers)
--stdio --server-cmd "java -jar renovatio-mcp-server.jar"

# WebSocket connection
--server ws://localhost:8080/ws
```

## Integration

### GitHub Actions
```yaml
- name: Run Renovatio Analysis
  run: |
    java -jar renovatio-client.jar \
      --server ${{ secrets.RENOVATIO_SERVER }} \
      --command tools:call \
      --tool java_analyze \
      --args '{"workspacePath": ".", "nql": "FIND security-issues"}'
```

### Jenkins Pipeline
```groovy
stage('Code Analysis') {
    steps {
        sh '''
            java -jar renovatio-client.jar \
                --server http://renovatio:8080 \
                --batch-file analysis-pipeline.json \
                --output results.json
        '''
    }
}
```

## Development

### Build
```bash
mvn clean package
```

### Run Tests
```bash
mvn test
```

### Create Executable JAR
```bash
mvn package
java -jar target/renovatio-client-0.0.1-SNAPSHOT.jar --help
```