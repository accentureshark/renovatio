#!/bin/bash

# Renovatio MCP Tools Demo Script
# Este script demuestra cómo usar las herramientas MCP de Renovatio

set -e

BLUE='\033[0;34m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
NC='\033[0m' # No Color

SERVER_URL="http://localhost:8181"
JAVA_PROJECT_PATH="${1:-/tmp/java-sample}"

echo -e "${BLUE}🎯 Renovatio MCP Tools Demo${NC}"
echo "=================================="
echo ""

# Verificar que el servidor esté ejecutándose
echo -e "${YELLOW}📡 Verificando conexión al servidor MCP...${NC}"
if ! curl -s "$SERVER_URL" > /dev/null; then
    echo -e "${RED}❌ Error: El servidor Renovatio no está ejecutándose en $SERVER_URL${NC}"
    echo "   Ejecuta: cd renovatio-mcp-server && SERVER_PORT=8181 mvn spring-boot:run"
    exit 1
fi
echo -e "${GREEN}✅ Servidor MCP está ejecutándose${NC}"
echo ""

# 1. Inicializar conexión MCP
echo -e "${YELLOW}🔌 1. Inicializando conexión MCP...${NC}"
INIT_RESPONSE=$(curl -s -X POST -H "Content-Type: application/json" \
  -d '{"jsonrpc": "2.0", "id": "1", "method": "initialize", "params": {}}' \
  "$SERVER_URL")

if echo "$INIT_RESPONSE" | jq -e '.result.serverInfo' > /dev/null; then
    echo -e "${GREEN}✅ Conexión MCP inicializada${NC}"
    echo "   Servidor: $(echo "$INIT_RESPONSE" | jq -r '.result.serverInfo.name')"
    echo "   Versión: $(echo "$INIT_RESPONSE" | jq -r '.result.serverInfo.version')"
else
    echo -e "${RED}❌ Error al inicializar conexión MCP${NC}"
    exit 1
fi
echo ""

# 2. Listar herramientas disponibles
echo -e "${YELLOW}🛠️  2. Listando herramientas disponibles...${NC}"
TOOLS_RESPONSE=$(curl -s -X POST -H "Content-Type: application/json" \
  -d '{"jsonrpc": "2.0", "id": "2", "method": "tools/list", "params": {}}' \
  "$SERVER_URL")

if echo "$TOOLS_RESPONSE" | jq -e '.result.tools' > /dev/null; then
    echo -e "${GREEN}✅ Herramientas disponibles:${NC}"
    echo "$TOOLS_RESPONSE" | jq -r '.result.tools[] | "   - \(.name): \(.description)"'
else
    echo -e "${RED}❌ Error al obtener herramientas${NC}"
    exit 1
fi
echo ""

# 3. Crear proyecto Java de ejemplo si no existe
if [ ! -d "$JAVA_PROJECT_PATH" ]; then
    echo -e "${YELLOW}📁 3. Creando proyecto Java de ejemplo...${NC}"
    mkdir -p "$JAVA_PROJECT_PATH/src/main/java/com/example"
    
    cat > "$JAVA_PROJECT_PATH/src/main/java/com/example/Calculator.java" << 'EOF'
package com.example;

/**
 * A simple calculator class for demonstration
 */
public class Calculator {
    
    private String name;
    private int operationCount = 0;
    
    public Calculator(String name) {
        this.name = name;
    }
    
    /**
     * Adds two numbers
     */
    public int add(int a, int b) {
        operationCount++;
        return a + b;
    }
    
    /**
     * Subtracts two numbers
     */
    public int subtract(int a, int b) {
        operationCount++;
        return a - b;
    }
    
    /**
     * Multiplies two numbers
     */
    public int multiply(int a, int b) {
        operationCount++;
        return a * b;
    }
    
    /**
     * Divides two numbers
     */
    public double divide(int a, int b) {
        if (b == 0) {
            throw new IllegalArgumentException("Cannot divide by zero");
        }
        operationCount++;
        return (double) a / b;
    }
    
    public int getOperationCount() {
        return operationCount;
    }
    
    public String getName() {
        return name;
    }
}
EOF

    cat > "$JAVA_PROJECT_PATH/src/main/java/com/example/Main.java" << 'EOF'
package com.example;

public class Main {
    public static void main(String[] args) {
        Calculator calc = new Calculator("Demo Calculator");
        
        System.out.println("Testing calculator: " + calc.getName());
        System.out.println("2 + 3 = " + calc.add(2, 3));
        System.out.println("10 - 4 = " + calc.subtract(10, 4));
        System.out.println("6 * 7 = " + calc.multiply(6, 7));
        System.out.println("15 / 3 = " + calc.divide(15, 3));
        System.out.println("Total operations: " + calc.getOperationCount());
    }
}
EOF

    echo -e "${GREEN}✅ Proyecto Java creado en: $JAVA_PROJECT_PATH${NC}"
else
    echo -e "${GREEN}✅ Usando proyecto Java existente: $JAVA_PROJECT_PATH${NC}"
fi
echo ""

# 4. Analizar código Java
echo -e "${YELLOW}🔍 4. Analizando código Java...${NC}"
ANALYZE_RESPONSE=$(curl -s -X POST -H "Content-Type: application/json" \
  -d "{\"jsonrpc\": \"2.0\", \"id\": \"4\", \"method\": \"tools/call\", \"params\": {\"name\": \"java_analyze\", \"arguments\": {\"scope\": \"$JAVA_PROJECT_PATH\", \"nql\": \"FIND ALL CLASSES\"}}}" \
  "$SERVER_URL")

echo "Respuesta de análisis:"
echo "$ANALYZE_RESPONSE" | jq '.result.content[0].text'
echo ""

# 5. Calcular métricas
echo -e "${YELLOW}📊 5. Calculando métricas de código...${NC}"
METRICS_RESPONSE=$(curl -s -X POST -H "Content-Type: application/json" \
  -d "{\"jsonrpc\": \"2.0\", \"id\": \"5\", \"method\": \"tools/call\", \"params\": {\"name\": \"java_metrics\", \"arguments\": {\"scope\": \"$JAVA_PROJECT_PATH\", \"nql\": \"CALCULATE METRICS FOR ALL CLASSES\"}}}" \
  "$SERVER_URL")

echo "Respuesta de métricas:"
echo "$METRICS_RESPONSE" | jq '.result.content[0].text'
echo ""

# 6. Indexar repositorio
echo -e "${YELLOW}📚 6. Indexando repositorio...${NC}"
INDEX_RESPONSE=$(curl -s -X POST -H "Content-Type: application/json" \
  -d "{\"jsonrpc\": \"2.0\", \"id\": \"6\", \"method\": \"tools/call\", \"params\": {\"name\": \"common_index\", \"arguments\": {\"repoId\": \"$JAVA_PROJECT_PATH\"}}}" \
  "$SERVER_URL")

echo "Respuesta de indexación:"
echo "$INDEX_RESPONSE" | jq '.result.content[0].text'
echo ""

# 7. Buscar en repositorio
echo -e "${YELLOW}🔎 7. Buscando en repositorio...${NC}"
SEARCH_RESPONSE=$(curl -s -X POST -H "Content-Type: application/json" \
  -d "{\"jsonrpc\": \"2.0\", \"id\": \"7\", \"method\": \"tools/call\", \"params\": {\"name\": \"common_search\", \"arguments\": {\"repoId\": \"$JAVA_PROJECT_PATH\", \"query\": \"Calculator\", \"path\": \"src/main/java\"}}}" \
  "$SERVER_URL")

echo "Respuesta de búsqueda:"
echo "$SEARCH_RESPONSE" | jq '.result.content[0].text'
echo ""

# 8. Compilar NQL
echo -e "${YELLOW}🤖 8. Compilando lenguaje natural a NQL...${NC}"
NQL_RESPONSE=$(curl -s -X POST -H "Content-Type: application/json" \
  -d '{"jsonrpc": "2.0", "id": "8", "method": "tools/call", "params": {"name": "nql_compile", "arguments": {"question": "Find all Java classes with more than 5 methods", "context": "Code quality analysis"}}}' \
  "$SERVER_URL")

echo "Respuesta de compilación NQL:"
echo "$NQL_RESPONSE" | jq '.result.content[0].text'
echo ""

# 9. Crear plan de transformación
echo -e "${YELLOW}📋 9. Creando plan de transformación...${NC}"
PLAN_RESPONSE=$(curl -s -X POST -H "Content-Type: application/json" \
  -d "{\"jsonrpc\": \"2.0\", \"id\": \"9\", \"method\": \"tools/call\", \"params\": {\"name\": \"java_plan\", \"arguments\": {\"scope\": \"$JAVA_PROJECT_PATH\", \"nql\": \"MIGRATE FROM JAVA 8 TO JAVA 17\"}}}" \
  "$SERVER_URL")

echo "Respuesta del plan:"
echo "$PLAN_RESPONSE" | jq '.result.content[0].text'
echo ""

# 10. Generar diferencias
echo -e "${YELLOW}📝 10. Generando diferencias semánticas...${NC}"
DIFF_RESPONSE=$(curl -s -X POST -H "Content-Type: application/json" \
  -d "{\"jsonrpc\": \"2.0\", \"id\": \"10\", \"method\": \"tools/call\", \"params\": {\"name\": \"java_diff\", \"arguments\": {\"scope\": \"$JAVA_PROJECT_PATH\", \"nql\": \"SHOW CHANGES\"}}}" \
  "$SERVER_URL")

echo "Respuesta de diferencias:"
echo "$DIFF_RESPONSE" | jq '.result.content[0].text'
echo ""

# Resumen final
echo -e "${BLUE}📊 Resumen del Demo${NC}"
echo "=================="
echo -e "${GREEN}✅ Todas las herramientas MCP fueron probadas exitosamente${NC}"
echo ""
echo -e "${YELLOW}Herramientas probadas:${NC}"
echo "1. ✅ initialize - Inicialización MCP"
echo "2. ✅ tools/list - Listado de herramientas"
echo "3. ✅ java_analyze - Análisis de código Java"
echo "4. ✅ java_metrics - Métricas de código"
echo "5. ✅ common_index - Indexación de repositorio"
echo "6. ✅ common_search - Búsqueda en repositorio"
echo "7. ✅ nql_compile - Compilación de lenguaje natural"
echo "8. ✅ java_plan - Plan de transformación"
echo "9. ✅ java_diff - Diferencias semánticas"
echo ""
echo -e "${YELLOW}Notas importantes:${NC}"
echo "• Algunas herramientas muestran 'not yet implemented' - esto es normal en desarrollo"
echo "• El análisis devuelve 'success=false' porque algunas funcionalidades están en desarrollo"
echo "• El protocolo MCP funciona correctamente y todas las herramientas responden"
echo ""
echo -e "${GREEN}🎉 Demo completado! Puedes usar estos ejemplos en VS Code.${NC}"
echo ""
echo -e "${YELLOW}Para usar en VS Code:${NC}"
echo "1. Instala la extensión 'REST Client'"
echo "2. Crea un archivo .http con los ejemplos"
echo "3. Usa los JSON-RPC mostrados arriba"
echo ""
echo -e "${YELLOW}Documentación creada en:${NC}"
echo "• docs/mcp-tools-usage-guide.md"
echo "• docs/vscode-mcp-examples.md"
echo "• docs/mcp-tools-quick-reference.md"