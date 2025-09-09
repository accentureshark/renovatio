#!/bin/bash
# Ejecuta el cliente MCP Java Renovatio

SERVER_URL=${1:-http://localhost:8181/}

mvn -pl renovatio-agent compile

CLASSPATH="renovatio-agent/target/renovatio-agent-0.0.1-SNAPSHOT.jar:renovatio-shared/target/renovatio-shared-0.0.1-SNAPSHOT.jar:renovatio-core/target/renovatio-core-0.0.1-SNAPSHOT.jar"

java -cp "$CLASSPATH" org.shark.renovatio.client.RenovatioMcpClient "$SERVER_URL"
