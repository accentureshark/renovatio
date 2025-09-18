#!/bin/bash

echo "=== ANÁLISIS DE MÉTRICAS JAVA PARA MCP-DEMO ==="
echo "Proyecto: /home/faguero/accenture/renovatio/mcp-demo"
echo "Fecha: $(date)"
echo ""

PROJECT_PATH="/home/faguero/accenture/renovatio/mcp-demo"

# Verificar que el directorio existe
if [ ! -d "$PROJECT_PATH" ]; then
    echo "ERROR: El directorio $PROJECT_PATH no existe"
    exit 1
fi

echo "📁 Archivos Java encontrados:"
find "$PROJECT_PATH" -name "*.java" -type f

echo ""
echo "📊 MÉTRICAS DE CALIDAD Y COMPLEJIDAD:"
echo "================================================"

# Contar archivos Java
TOTAL_FILES=$(find "$PROJECT_PATH" -name "*.java" -type f | wc -l)
echo "📄 Total de archivos Java: $TOTAL_FILES"

# Contar líneas de código
TOTAL_LINES=$(find "$PROJECT_PATH" -name "*.java" -exec wc -l {} \; | awk '{sum += $1} END {print sum}')
echo "📏 Total líneas de código: $TOTAL_LINES"

# Análisis detallado por archivo
echo ""
echo "📋 ANÁLISIS DETALLADO POR ARCHIVO:"
echo "================================================"

TOTAL_CLASSES=0
TOTAL_METHODS=0
TOTAL_IMPORTS=0

for file in $(find "$PROJECT_PATH" -name "*.java" -type f); do
    filename=$(basename "$file")
    relative_path=$(echo "$file" | sed "s|$PROJECT_PATH/||")

    echo ""
    echo "🔍 Archivo: $relative_path"

    # Contar líneas
    lines=$(wc -l < "$file")
    echo "   Líneas: $lines"

    # Contar clases/interfaces/enums
    classes=$(grep -c "^\s*\(public\s\+\)\?\(abstract\s\+\)\?\(final\s\+\)\?\(class\|interface\|enum\)\s\+" "$file" || echo "0")
    echo "   Clases/Interfaces/Enums: $classes"
    TOTAL_CLASSES=$((TOTAL_CLASSES + classes))

    # Contar métodos (aproximado)
    methods=$(grep -c "^\s*\(public\|private\|protected\)\?\s*\(static\s\+\)\?\(final\s\+\)\?\w\+\s\+\w\+\s*(" "$file" || echo "0")
    echo "   Métodos (aprox): $methods"
    TOTAL_METHODS=$((TOTAL_METHODS + methods))

    # Contar imports
    imports=$(grep -c "^import\s" "$file" || echo "0")
    echo "   Imports: $imports"
    TOTAL_IMPORTS=$((TOTAL_IMPORTS + imports))

    # Mostrar clases encontradas
    echo "   Clases encontradas:"
    grep "^\s*\(public\s\+\)\?\(abstract\s\+\)\?\(final\s\+\)\?\(class\|interface\|enum\)\s\+" "$file" | sed 's/.*\(class\|interface\|enum\)\s\+\([^[:space:]<{]\+\).*/      - \2/' || echo "      Ninguna"
done

echo ""
echo "📈 RESUMEN DE MÉTRICAS:"
echo "================================================"
echo "📄 Total archivos Java: $TOTAL_FILES"
echo "📏 Total líneas de código: $TOTAL_LINES"
echo "🏗️  Total clases/interfaces/enums: $TOTAL_CLASSES"
echo "⚙️  Total métodos (aprox): $TOTAL_METHODS"
echo "📦 Total imports: $TOTAL_IMPORTS"

# Calcular métricas derivadas
if [ $TOTAL_FILES -gt 0 ]; then
    AVG_LINES_PER_FILE=$((TOTAL_LINES / TOTAL_FILES))
    echo "📊 Promedio líneas por archivo: $AVG_LINES_PER_FILE"
fi

if [ $TOTAL_CLASSES -gt 0 ]; then
    AVG_METHODS_PER_CLASS=$((TOTAL_METHODS / TOTAL_CLASSES))
    echo "🔧 Promedio métodos por clase: $AVG_METHODS_PER_CLASS"
fi

if [ $TOTAL_FILES -gt 0 ]; then
    AVG_IMPORTS_PER_FILE=$((TOTAL_IMPORTS / TOTAL_FILES))
    echo "📋 Promedio imports por archivo: $AVG_IMPORTS_PER_FILE"
fi

echo ""
echo "🎯 INDICADORES DE CALIDAD:"
echo "================================================"

# Indicadores de calidad basados en las métricas
if [ $AVG_LINES_PER_FILE -lt 200 ]; then
    echo "✅ Tamaño de archivos: BUENO (< 200 líneas promedio)"
else
    echo "⚠️  Tamaño de archivos: REVISAR (> 200 líneas promedio)"
fi

if [ $AVG_METHODS_PER_CLASS -lt 15 ]; then
    echo "✅ Complejidad de clases: BUENA (< 15 métodos promedio)"
else
    echo "⚠️  Complejidad de clases: REVISAR (> 15 métodos promedio)"
fi

if [ $AVG_IMPORTS_PER_FILE -lt 20 ]; then
    echo "✅ Dependencias: CONTROLADAS (< 20 imports promedio)"
else
    echo "⚠️  Dependencias: REVISAR (> 20 imports promedio)"
fi

echo ""
echo "📄 ESTRUCTURA DEL PROYECTO:"
echo "================================================"
tree "$PROJECT_PATH" 2>/dev/null || find "$PROJECT_PATH" -type f -name "*.java" | sort

echo ""
echo "✅ Análisis completado exitosamente"
echo "💡 Este análisis proporciona métricas básicas de calidad y complejidad para el proyecto Java en mcp-demo"
