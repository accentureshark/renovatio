# Renovatio Agent

Distributed execution agent for running large-scale migrations and refactoring operations.

## Overview

`renovatio-agent` provides distributed execution capabilities for Renovatio, enabling:

- **Distributed processing**: Execute migrations across multiple agents
- **Git integration**: Clone, process, and push changes to repositories
- **Batch operations**: Handle large codebases efficiently
- **Progress tracking**: Monitor long-running operations
- **Resilience**: Retry mechanisms and error recovery

## Features

### Distributed Execution
- Agent registration and discovery
- Work distribution and load balancing
- Parallel processing capabilities
- Result aggregation and reporting

### Git Operations
- Repository cloning and management
- Branch creation and switching
- Commit and push operations
- Merge conflict resolution

### Monitoring & Reporting
- Real-time progress updates
- Execution metrics and statistics
- Error tracking and logging
- Health checks and status reporting

## Configuration

### Agent Configuration
```yaml
renovatio:
  agent:
    id: "agent-001"
    capacity: 10
    workspace-path: "/tmp/renovatio-workspace"
    git:
      default-branch: "renovatio-migration"
      commit-author: "Renovatio Agent <agent@renovatio.org>"
```

### Distributed Setup
```yaml
renovatio:
  coordinator:
    enabled: true
    port: 8090
  agents:
    discovery:
      type: "consul" # or "kubernetes", "static"
      endpoints: ["http://consul:8500"]
```

## Usage

### Start Agent
```bash
cd renovatio-agent
mvn spring-boot:run -Dspring.profiles.active=agent
```

### Submit Distributed Job
```bash
curl -X POST http://localhost:8080/ \
  -H "Content-Type: application/json" \
  -d '{
    "jsonrpc": "2.0",
    "id": "1",
    "method": "tools/call",
    "params": {
      "name": "agent_execute_distributed",
      "arguments": {
        "repositories": [
          "https://github.com/org/repo1.git",
          "https://github.com/org/repo2.git"
        ],
        "operation": "java_modernize",
        "parameters": {
          "targetJavaVersion": "17"
        }
      }
    }
  }'
```

## Deployment

### Docker
```dockerfile
FROM openjdk:17-jdk-slim
COPY renovatio-agent.jar /app/
WORKDIR /app
ENTRYPOINT ["java", "-jar", "renovatio-agent.jar"]
```

### Kubernetes
```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: renovatio-agent
spec:
  replicas: 3
  selector:
    matchLabels:
      app: renovatio-agent
  template:
    metadata:
      labels:
        app: renovatio-agent
    spec:
      containers:
      - name: agent
        image: renovatio/agent:latest
        env:
        - name: SPRING_PROFILES_ACTIVE
          value: "agent,kubernetes"
```

## Integration

The agent integrates with:
- **JGit**: Git operations and repository management
- **Spring Boot**: Configuration and lifecycle management  
- **Micrometer**: Metrics and monitoring
- **Resilience4j**: Circuit breakers and retry logic