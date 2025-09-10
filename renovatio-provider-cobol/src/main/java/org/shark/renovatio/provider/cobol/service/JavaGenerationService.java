package org.shark.renovatio.provider.cobol.service;

import com.squareup.javapoet.*;
import org.shark.renovatio.shared.domain.StubResult;
import org.shark.renovatio.shared.domain.Workspace;
import org.shark.renovatio.shared.nql.NqlQuery;
import org.springframework.stereotype.Service;

import javax.lang.model.element.Modifier;
import java.io.IOException;
import java.math.BigDecimal;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.*;

/**
 * Java code generation service using JavaPoet
 * Generates Java classes, DTOs, and interface stubs from COBOL structures
 */
@Service
public class JavaGenerationService {
    
    private final CobolParsingService parsingService;
    private final TemplateCodeGenerationService templateService;

    public JavaGenerationService(CobolParsingService parsingService, TemplateCodeGenerationService templateService) {
        this.parsingService = parsingService;
        this.templateService = templateService;
    }
    
    /**
     * Generates Java interface stubs for COBOL programs
     */
    public StubResult generateInterfaceStubs(NqlQuery query, Workspace workspace) {
        try {
            // Parse COBOL programs first
            var analyzeResult = parsingService.analyzeCOBOL(query, workspace);
            if (!analyzeResult.isSuccess()) {
                return new StubResult(false, "Failed to analyze COBOL: " + analyzeResult.getMessage());
            }

            @SuppressWarnings("unchecked")
            List<org.shark.renovatio.provider.cobol.domain.CobolProgram> programs = (List<org.shark.renovatio.provider.cobol.domain.CobolProgram>)
                ((Map<String, Object>) analyzeResult.getData()).get("programs");

            Map<String, String> generatedFiles = new HashMap<>();

            for (org.shark.renovatio.provider.cobol.domain.CobolProgram program : programs) {
                Map<String, Object> metadata = program.getMetadata();
                String fileName = (String) metadata.get("filePath");
                String baseName = Paths.get(fileName).getFileName().toString();
                // Clean and sanitize the class base name
                String classBase = sanitizeClassName(toPascalCase(baseName));

                System.out.println("DEBUG: Processing file: " + fileName + ", baseName: " + baseName);
                System.out.println("DEBUG: Generated classBase: " + classBase);

                try {
                    // Generate DTO class for data structures
                    String dtoClass = generateDataTransferObject(classBase, metadata);
                    generatedFiles.put(classBase + "DTO.java", dtoClass);
                    // Generate service interface
                    String serviceInterface = generateServiceInterface(classBase, metadata);
                    generatedFiles.put(classBase + "Service.java", serviceInterface);
                    // Generate implementation template
                    String serviceImpl = generateServiceImplementation(classBase, metadata);
                    generatedFiles.put(classBase + "ServiceImpl.java", serviceImpl);

                    @SuppressWarnings("unchecked")
                    Set<String> cics = (Set<String>) metadata.get("cicsCommands");
                    if (cics != null && !cics.isEmpty()) {
                        Map<String, Object> tmplData = new HashMap<>();
                        tmplData.put("className", classBase + "CicsController");
                        tmplData.put("transactions", cics);
                        String controller = templateService.generateCicsController(tmplData);
                        generatedFiles.put(classBase + "CicsController.java", controller);
                    }
                } catch (Exception e) {
                    System.out.println("DEBUG: Error generating for classBase '" + classBase + "': " + e.getMessage());
                    throw e;
                }
            }

            // Write generated files to disk
            String outputPath = writeGeneratedFilesToDisk(generatedFiles, workspace);

            // Debug: print generated keys
            System.out.println("Claves generadas: " + generatedFiles.keySet());
            System.out.println("Archivos escritos en: " + outputPath);

            boolean success = !generatedFiles.isEmpty();
            String message = success ?
                "Generated " + generatedFiles.size() + " Java files in: " + outputPath :
                "No Java files generated";

            StubResult result = new StubResult(success, message);
            result.setGeneratedCode(generatedFiles);
            return result;
        } catch (Exception e) {
            return new StubResult(false, "Stub generation failed: " + e.getMessage());
        }
    }
    
    /**
     * Generates a Java DTO class from COBOL data structures
     */
    private String generateDataTransferObject(String cleanClassName, Map<String, Object> programData) {
        // Asegurar que el cleanClassName esté completamente limpio
        String sanitizedClassName = sanitizeClassName(cleanClassName);
        String className = sanitizedClassName + "DTO";

        System.out.println("DEBUG: generateDataTransferObject - original: '" + cleanClassName + "', sanitized: '" + sanitizedClassName + "', final: '" + className + "'");

        TypeSpec.Builder classBuilder = TypeSpec.classBuilder(className)
            .addModifiers(Modifier.PUBLIC)
            .addJavadoc("Data Transfer Object generated from COBOL program: $L\n", sanitizedClassName);

        // Add default constructor
        classBuilder.addMethod(MethodSpec.constructorBuilder()
            .addModifiers(Modifier.PUBLIC)
            .build());
        
        // Extract data items from metadata
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> dataItems = (List<Map<String, Object>>) programData.get("dataItems");

        if (dataItems != null) {
            for (Map<String, Object> item : dataItems) {
                String fieldName = (String) item.get("name");
                String javaType = (String) item.get("javaType");
                
                if (fieldName != null && javaType != null) {
                    addFieldToClass(classBuilder, fieldName, javaType);
                }
            }
        }
        
        TypeSpec classSpec = classBuilder.build();
        
        JavaFile javaFile = JavaFile.builder("org.shark.renovatio.generated.cobol", classSpec)
            .build();
        
        return javaFile.toString();
    }
    
    /**
     * Generates a service interface for COBOL program functionality
     */
    private String generateServiceInterface(String cleanClassName, Map<String, Object> programData) {
        // Asegurar que el cleanClassName esté completamente limpio
        String sanitizedClassName = sanitizeClassName(cleanClassName);
        String interfaceName = sanitizedClassName + "Service";
        String dtoName = sanitizedClassName + "DTO";

        System.out.println("DEBUG: generateServiceInterface - original: '" + cleanClassName + "', sanitized: '" + sanitizedClassName + "'");

        ClassName dtoClass = ClassName.get("org.shark.renovatio.generated.cobol", dtoName);

        TypeSpec.Builder interfaceBuilder = TypeSpec.interfaceBuilder(interfaceName)
            .addModifiers(Modifier.PUBLIC)
            .addJavadoc("Service interface for COBOL program: $L\n", sanitizedClassName);

        // Add process method
        MethodSpec processMethod = MethodSpec.methodBuilder("process")
            .addModifiers(Modifier.PUBLIC, Modifier.ABSTRACT)
            .addParameter(dtoClass, "input")
            .returns(dtoClass)
            .addJavadoc("Process the COBOL program logic with given input\n")
            .addJavadoc("@param input Input data structure\n")
            .addJavadoc("@return Processed output data structure\n")
            .build();
        
        interfaceBuilder.addMethod(processMethod);
        
        // Add validation method
        MethodSpec validateMethod = MethodSpec.methodBuilder("validate")
            .addModifiers(Modifier.PUBLIC, Modifier.ABSTRACT)
            .addParameter(dtoClass, "input")
            .returns(boolean.class)
            .addJavadoc("Validate input data structure\n")
            .addJavadoc("@param input Input data to validate\n")
            .addJavadoc("@return true if valid, false otherwise\n")
            .build();
        
        interfaceBuilder.addMethod(validateMethod);
        
        TypeSpec interfaceSpec = interfaceBuilder.build();
        
        JavaFile javaFile = JavaFile.builder("org.shark.renovatio.generated.cobol", interfaceSpec)
            .build();
        
        return javaFile.toString();
    }
    
    /**
     * Generates a service implementation template
     */
    private String generateServiceImplementation(String cleanClassName, Map<String, Object> programData) {
        // Asegurar que el cleanClassName esté completamente limpio
        String sanitizedClassName = sanitizeClassName(cleanClassName);
        String className = sanitizedClassName + "ServiceImpl";
        String interfaceName = sanitizedClassName + "Service";
        String dtoName = sanitizedClassName + "DTO";

        System.out.println("DEBUG: generateServiceImplementation - original: '" + cleanClassName + "', sanitized: '" + sanitizedClassName + "'");

        ClassName interfaceClass = ClassName.get("org.shark.renovatio.generated.cobol", interfaceName);
        ClassName dtoClass = ClassName.get("org.shark.renovatio.generated.cobol", dtoName);

        TypeSpec.Builder classBuilder = TypeSpec.classBuilder(className)
            .addModifiers(Modifier.PUBLIC)
            .addSuperinterface(interfaceClass)
            .addAnnotation(ClassName.get("org.springframework.stereotype", "Service"))
            .addJavadoc("Implementation of $L\n", interfaceName)
            .addJavadoc("Generated from COBOL program: $L\n", sanitizedClassName);

        // Implement process method
        MethodSpec processMethod = MethodSpec.methodBuilder("process")
            .addModifiers(Modifier.PUBLIC)
            .addAnnotation(Override.class)
            .addParameter(dtoClass, "input")
            .returns(dtoClass)
            .addStatement("// TODO: Implement COBOL business logic")
            .addStatement("// Original COBOL program: $L", cleanClassName)
            .addStatement("$T output = new $T()", dtoClass, dtoClass)
            .addStatement("return output")
            .build();
        
        classBuilder.addMethod(processMethod);
        
        // Implement validate method
        MethodSpec validateMethod = MethodSpec.methodBuilder("validate")
            .addModifiers(Modifier.PUBLIC)
            .addAnnotation(Override.class)
            .addParameter(dtoClass, "input")
            .returns(boolean.class)
            .addStatement("// TODO: Implement validation logic")
            .addStatement("return input != null")
            .build();
        
        classBuilder.addMethod(validateMethod);
        
        TypeSpec classSpec = classBuilder.build();
        
        JavaFile javaFile = JavaFile.builder("org.shark.renovatio.generated.cobol", classSpec)
            .build();
        
        return javaFile.toString();
    }
    
    /**
     * Adds a field with getter and setter to a class builder
     */
    private void addFieldToClass(TypeSpec.Builder classBuilder, String fieldName, String javaType) {
        String normalizedFieldName = toCamelCase(fieldName);
        ClassName fieldType = getClassNameForType(javaType);
        
        // Add field
        FieldSpec field = FieldSpec.builder(fieldType, normalizedFieldName)
            .addModifiers(Modifier.PRIVATE)
            .build();
        classBuilder.addField(field);
        
        // Add getter
        String getterName = "get" + toPascalCase(normalizedFieldName);
        MethodSpec getter = MethodSpec.methodBuilder(getterName)
            .addModifiers(Modifier.PUBLIC)
            .returns(fieldType)
            .addStatement("return $L", normalizedFieldName)
            .build();
        classBuilder.addMethod(getter);
        
        // Add setter
        String setterName = "set" + toPascalCase(normalizedFieldName);
        MethodSpec setter = MethodSpec.methodBuilder(setterName)
            .addModifiers(Modifier.PUBLIC)
            .addParameter(fieldType, normalizedFieldName)
            .addStatement("this.$L = $L", normalizedFieldName, normalizedFieldName)
            .build();
        classBuilder.addMethod(setter);
    }
    
    /**
     * Maps Java type string to ClassName
     */
    private ClassName getClassNameForType(String javaType) {
        switch (javaType) {
            case "String": return ClassName.get(String.class);
            case "Integer": return ClassName.get(Integer.class);
            case "Long": return ClassName.get(Long.class);
            case "BigDecimal": return ClassName.get(BigDecimal.class);
            default: return ClassName.get(Object.class);
        }
    }
    
    /**
     * Converts string to PascalCase (preserva mayúsculas en siglas y nombres COBOL, separa por punto también)
     */
    private String toPascalCase(String input) {
        if (input == null || input.isEmpty()) return "CobolProgram";

        System.out.println("DEBUG: toPascalCase input: '" + input + "'");

        // Limpiar caracteres especiales que no son válidos en nombres de clase Java (incluyendo apostrofes)
        // Primero quitar la extensión del archivo si existe
        String withoutExtension = input.replaceAll("\\.(cob|cobol|cbl|cpy)$", "");

        // Limpiar todos los caracteres especiales incluyendo apostrofes, guiones, espacios, etc.
        String cleaned = withoutExtension.replaceAll("[^a-zA-Z0-9]", " ");
        System.out.println("DEBUG: after cleaning: '" + cleaned + "'");

        // Dividir por espacios múltiples y procesar cada parte
        String[] parts = cleaned.trim().split("\\s+");
        StringBuilder result = new StringBuilder();

        System.out.println("DEBUG: parts array: " + java.util.Arrays.toString(parts));

        for (String part : parts) {
            if (part.isEmpty()) continue;

            // Ignorar palabras comunes que no aportan valor al nombre de clase
            if (part.equalsIgnoreCase("cob") || part.equalsIgnoreCase("cobol") ||
                part.equalsIgnoreCase("cbl") || part.equalsIgnoreCase("cpy") ||
                part.equalsIgnoreCase("program") || part.equalsIgnoreCase("file")) {
                System.out.println("DEBUG: skipping common word: '" + part + "'");
                continue;
            }

            System.out.println("DEBUG: processing part: '" + part + "'");
            // Capitalizar primera letra y hacer el resto lowercase
            result.append(part.substring(0, 1).toUpperCase());
            if (part.length() > 1) {
                result.append(part.substring(1).toLowerCase());
            }
        }

        // Si el resultado está vacío, usar un nombre por defecto
        String finalResult = result.toString();
        if (finalResult.isEmpty()) {
            System.out.println("DEBUG: empty result, using default");
            finalResult = "CobolProgram";
        }

        // Asegurar que el nombre comience con una letra
        if (!Character.isLetter(finalResult.charAt(0))) {
            finalResult = "Cobol" + finalResult;
        }

        // Validación final para asegurar que el nombre sea válido para Java
        // Solo permitir letras, números y guiones bajos, debe comenzar con letra
        finalResult = finalResult.replaceAll("[^A-Za-z0-9]", "");
        if (finalResult.isEmpty() || !Character.isLetter(finalResult.charAt(0))) {
            finalResult = "CobolProgram";
        }

        System.out.println("DEBUG: toPascalCase final output: '" + finalResult + "'");
        return finalResult;
    }

    /**
     * Converts string to camelCase (preserva mayúsculas en siglas y nombres COBOL)
     */
    private String toCamelCase(String input) {
        if (input == null || input.isEmpty()) return input;

        // Limpiar caracteres especiales que no son válidos en nombres de variable Java
        String cleaned = input.replaceAll("[^a-zA-Z0-9\\s\\-_]", "");

        String[] parts = cleaned.split("[-_\\s]+");
        if (parts.length == 0) return "field";

        StringBuilder result = new StringBuilder(parts[0].toLowerCase());
        for (int i = 1; i < parts.length; i++) {
            if (parts[i].isEmpty()) continue;
            result.append(parts[i].substring(0, 1).toUpperCase());
            if (parts[i].length() > 1) {
                result.append(parts[i].substring(1).toLowerCase());
            }
        }

        // Si el resultado está vacío o comienza con número, agregar prefijo
        String finalResult = result.toString();
        if (finalResult.isEmpty() || Character.isDigit(finalResult.charAt(0))) {
            return "field" + capitalize(finalResult);
        }

        return finalResult;
    }
    
    /**
     * Capitalizes first letter of string
     */
    private String capitalize(String input) {
        if (input == null || input.isEmpty()) return input;
        return input.substring(0, 1).toUpperCase() + input.substring(1).toLowerCase();
    }

    /**
     * Sanitizes the class name to ensure it is a valid Java identifier
     */
    private String sanitizeClassName(String className) {
        if (className == null || className.isEmpty()) return "CobolProgram";

        System.out.println("DEBUG: sanitizeClassName input: '" + className + "'");

        // Limpiar caracteres especiales que no son válidos en nombres de clase Java
        String sanitized = className.replaceAll("[^a-zA-Z0-9_$]", " ");

        // Dividir por espacios múltiples y procesar cada parte
        String[] parts = sanitized.trim().split("\\s+");
        StringBuilder result = new StringBuilder();

        for (String part : parts) {
            if (part.isEmpty()) continue;

            // Ignorar palabras comunes que no aportan valor al nombre de clase
            if (part.equalsIgnoreCase("cob") || part.equalsIgnoreCase("cobol") ||
                part.equalsIgnoreCase("cbl") || part.equalsIgnoreCase("cpy") ||
                part.equalsIgnoreCase("program") || part.equalsIgnoreCase("file")) {
                continue;
            }

            // Capitalizar primera letra y hacer el resto lowercase
            result.append(part.substring(0, 1).toUpperCase());
            if (part.length() > 1) {
                result.append(part.substring(1).toLowerCase());
            }
        }

        // Si el resultado está vacío, usar un nombre por defecto
        String finalResult = result.toString();
        if (finalResult.isEmpty()) {
            finalResult = "CobolProgram";
        }

        // Asegurar que el nombre comience con una letra
        if (!Character.isLetter(finalResult.charAt(0))) {
            finalResult = "Cobol" + finalResult;
        }

        // Validación final para asegurar que el nombre sea válido para Java
        // Solo permitir letras, números y guiones bajos, debe comenzar con letra
        finalResult = finalResult.replaceAll("[^A-Za-z0-9]", "");
        if (finalResult.isEmpty() || !Character.isLetter(finalResult.charAt(0))) {
            finalResult = "CobolProgram";
        }

        System.out.println("DEBUG: sanitizeClassName output: '" + finalResult + "'");
        return finalResult;
    }

    /**
     * Writes the generated Java files to disk
     */
    private String writeGeneratedFilesToDisk(Map<String, String> generatedFiles, Workspace workspace) {
        try {
            // Crear directorio de salida dentro del workspace
            Path workspacePath = Paths.get(workspace.getPath());
            Path outputDir = workspacePath.resolve("generated-java-stubs");

            // Crear directorio si no existe
            if (!java.nio.file.Files.exists(outputDir)) {
                java.nio.file.Files.createDirectories(outputDir);
            }

            // Escribir cada archivo generado
            for (Map.Entry<String, String> entry : generatedFiles.entrySet()) {
                String fileName = entry.getKey();
                String fileContent = entry.getValue();

                Path filePath = outputDir.resolve(fileName);
                java.nio.file.Files.write(filePath, fileContent.getBytes(java.nio.charset.StandardCharsets.UTF_8));

                System.out.println("Archivo escrito: " + filePath.toString());
            }

            return outputDir.toAbsolutePath().toString();

        } catch (Exception e) {
            System.err.println("Error escribiendo archivos: " + e.getMessage());
            e.printStackTrace();
            return "Error: No se pudieron escribir los archivos - " + e.getMessage();
        }
    }
}
