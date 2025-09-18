PORT=${PORT:-8080}
PROFILE=${PROFILE:-default}
LOG_FILE=${LOG_FILE:-server.log}

echo "[Renovatio MCP] Starting MCP server (module: $MODULE, port: $PORT, profile: $PROFILE)"

cd "$(dirname "$0")/$MODULE"

# Permite pasar argumentos extra a Maven
EXTRA_ARGS="$@"

# Ejecuta el servidor usando Spring Boot
mvn spring-boot:run -Dspring-boot.run.arguments="--server.port=$PORT --spring.profiles.active=$PROFILE" $EXTRA_ARGS | tee "$LOG_FILE"

cd - > /dev/null

echo "[Renovatio MCP] Server started. MCP endpoint available at http://localhost:$PORT/mcp"
echo "Logs: $MODULE/$LOG_FILE"

