#!/bin/bash
# Renovatio MCP CLI en Bash
# Requiere: jq, curl

SERVER_URL="http://localhost:8081/"

# 1. Obtener lista de herramientas MCP
TOOLS_JSON=$(curl -s -X POST -H "Content-Type: application/json" \
  -d '{"jsonrpc": "2.0", "id": "cli-1", "method": "tools/list", "params": {}}' \
  "$SERVER_URL")

TOOLS=$(echo "$TOOLS_JSON" | jq -c '.result.tools')

if [ "$TOOLS" == "null" ] || [ -z "$TOOLS" ]; then
  echo "No se encontraron herramientas MCP en el servidor."
  exit 1
fi

# 2. Mostrar herramientas disponibles
COUNT=$(echo "$TOOLS" | jq 'length')
echo "Renovatio MCP CLI - Herramientas disponibles:"
for i in $(seq 0 $((COUNT-1))); do
  NAME=$(echo "$TOOLS" | jq -r ".[$i].name")
  DESC=$(echo "$TOOLS" | jq -r ".[$i].description? // \"\"")
  echo "[$((i+1))] $NAME: $DESC"
done

# 3. Seleccionar herramienta
read -p "Selecciona el número de la herramienta a ejecutar: " SEL
IDX=$((SEL-1))
TOOL_NAME=$(echo "$TOOLS" | jq -r ".[$IDX].name")
if [ -z "$TOOL_NAME" ] || [ "$TOOL_NAME" == "null" ]; then
  echo "Selección inválida."
  exit 1
fi

echo "Has seleccionado: $TOOL_NAME"

# 4. Solicitar argumentos
PARAMS=$(echo "$TOOLS" | jq -c ".[$IDX].parameters // []")
EXAMPLE=$(echo "$TOOLS" | jq -c ".[$IDX].example // {}")
ARGS="{}"
if [ "$PARAMS" != "[]" ]; then
  ARGS="{"
  LEN=$(echo "$PARAMS" | jq 'length')
  for j in $(seq 0 $((LEN-1))); do
    PNAME=$(echo "$PARAMS" | jq -r ".[$j].name")
    PTYPE=$(echo "$PARAMS" | jq -r ".[$j].type // \"string\"")
    PDESC=$(echo "$PARAMS" | jq -r ".[$j].description? // \"\"")
    # Safe dynamic lookup using --arg
    PEXAMPLE=$(echo "$EXAMPLE" | jq -r --arg k "$PNAME" '.[$k] // ""')
    PROMPT="Argumento '$PNAME' ($PTYPE): $PDESC"
    if [ -n "$PEXAMPLE" ]; then
      PROMPT+=" [Ejemplo: $PEXAMPLE]"
    fi
    read -p "$PROMPT " PVAL
    if [ -z "$PVAL" ] && [ -n "$PEXAMPLE" ]; then
      PVAL="$PEXAMPLE"
    fi
    # Si es número, no poner comillas
    if [[ "$PTYPE" == "int" || "$PTYPE" == "float" || "$PTYPE" == "number" ]]; then
      ARGS+="\"$PNAME\": $PVAL"
    else
      ARGS+="\"$PNAME\": \"$PVAL\""
    fi
    if [ $j -lt $((LEN-1)) ]; then
      ARGS+=", "
    fi
  done
  ARGS+="}"
else
  echo "No se encontraron parámetros definidos. Puedes ingresar argumentos manualmente."
  ARGS="{"
  while true; do
    read -p "Nombre del argumento (ENTER para terminar): " K
    if [ -z "$K" ]; then break; fi
    read -p "Valor para '$K': " V
    ARGS+="\"$K\": \"$V\"," # Siempre string
  done
  # Eliminar última coma si existe
  ARGS=$(echo "$ARGS" | sed 's/,$//')
  ARGS+="}"
fi

echo "Ejecutando herramienta..."

# 5. Ejecutar herramienta MCP
REQ_JSON="{\"jsonrpc\": \"2.0\", \"id\": \"cli-2\", \"method\": \"tools/call\", \"params\": {\"name\": \"$TOOL_NAME\", \"arguments\": $ARGS}}"

RESULT=$(curl -s -X POST -H "Content-Type: application/json" -d "$REQ_JSON" "$SERVER_URL")

echo -e "\nResultado:\n"
echo "$RESULT" | jq
