# Ejemplos de AST COBOL

Estos fragmentos muestran cómo los parsers generan un árbol de sintaxis abstracta (AST) a partir de un programa sencillo.

## Programa de ejemplo

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO.
       PROCEDURE DIVISION.
           DISPLAY "HELLO".
           STOP RUN.
```

## AST con ProLeap

```text
Program
└─CompilationUnit
  └─ProgramUnit (HELLO)
    ├─IdentificationDivision
    └─ProcedureDivision
      ├─DisplayStatement("HELLO")
      └─StopStatement
```

## AST con Koopa

```text
compilation-unit
└─program-unit name=HELLO
  └─procedure-division
    ├─display-statement value="HELLO"
    └─stop-statement
```

Estos árboles pueden utilizarse como referencia para validar el parsing en pruebas automáticas o análisis manuales.

