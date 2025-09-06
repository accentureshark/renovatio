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
    
    public JavaGenerationService(CobolParsingService parsingService) {
        this.parsingService = parsingService;
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
            List<Map<String, Object>> programs = (List<Map<String, Object>>) 
                ((Map<String, Object>) analyzeResult.getData()).get("programs");
            
            Map<String, String> generatedFiles = new HashMap<>();
            
            for (Map<String, Object> programData : programs) {
                String programName = (String) programData.get("programName");
                
                // Generate DTO class for data structures
                String dtoClass = generateDataTransferObject(programName, programData);
                generatedFiles.put(programName + "DTO.java", dtoClass);
                
                // Generate service interface
                String serviceInterface = generateServiceInterface(programName, programData);
                generatedFiles.put(programName + "Service.java", serviceInterface);
                
                // Generate implementation template
                String serviceImpl = generateServiceImplementation(programName, programData);
                generatedFiles.put(programName + "ServiceImpl.java", serviceImpl);
            }
            
            StubResult result = new StubResult(true, "Generated " + generatedFiles.size() + " Java files");
            result.setGeneratedCode(generatedFiles);
            
            return result;
            
        } catch (Exception e) {
            return new StubResult(false, "Stub generation failed: " + e.getMessage());
        }
    }
    
    /**
     * Generates a Java DTO class from COBOL data structures
     */
    private String generateDataTransferObject(String programName, Map<String, Object> programData) {
        String className = toPascalCase(programName) + "DTO";
        
        TypeSpec.Builder classBuilder = TypeSpec.classBuilder(className)
            .addModifiers(Modifier.PUBLIC)
            .addJavadoc("Data Transfer Object generated from COBOL program: $L\n", programName);
        
        // Add default constructor
        classBuilder.addMethod(MethodSpec.constructorBuilder()
            .addModifiers(Modifier.PUBLIC)
            .build());
        
        // Extract data items from metadata
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> dataItems = (List<Map<String, Object>>) 
            ((Map<String, Object>) programData.get("metadata")).get("dataItems");
        
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
    private String generateServiceInterface(String programName, Map<String, Object> programData) {
        String interfaceName = toPascalCase(programName) + "Service";
        String dtoName = toPascalCase(programName) + "DTO";
        
        ClassName dtoClass = ClassName.get("org.shark.renovatio.generated.cobol", dtoName);
        
        TypeSpec.Builder interfaceBuilder = TypeSpec.interfaceBuilder(interfaceName)
            .addModifiers(Modifier.PUBLIC)
            .addJavadoc("Service interface for COBOL program: $L\n", programName);
        
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
    private String generateServiceImplementation(String programName, Map<String, Object> programData) {
        String className = toPascalCase(programName) + "ServiceImpl";
        String interfaceName = toPascalCase(programName) + "Service";
        String dtoName = toPascalCase(programName) + "DTO";
        
        ClassName interfaceClass = ClassName.get("org.shark.renovatio.generated.cobol", interfaceName);
        ClassName dtoClass = ClassName.get("org.shark.renovatio.generated.cobol", dtoName);
        
        TypeSpec.Builder classBuilder = TypeSpec.classBuilder(className)
            .addModifiers(Modifier.PUBLIC)
            .addSuperinterface(interfaceClass)
            .addAnnotation(ClassName.get("org.springframework.stereotype", "Service"))
            .addJavadoc("Implementation of $L\n", interfaceName)
            .addJavadoc("Generated from COBOL program: $L\n", programName);
        
        // Implement process method
        MethodSpec processMethod = MethodSpec.methodBuilder("process")
            .addModifiers(Modifier.PUBLIC)
            .addAnnotation(Override.class)
            .addParameter(dtoClass, "input")
            .returns(dtoClass)
            .addStatement("// TODO: Implement COBOL business logic")
            .addStatement("// Original COBOL program: $L", programName)
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
     * Converts string to camelCase
     */
    private String toCamelCase(String input) {
        if (input == null || input.isEmpty()) return input;
        
        String[] parts = input.split("[-_\\s]+");
        StringBuilder result = new StringBuilder(parts[0].toLowerCase());
        
        for (int i = 1; i < parts.length; i++) {
            result.append(capitalize(parts[i]));
        }
        
        return result.toString();
    }
    
    /**
     * Converts string to PascalCase
     */
    private String toPascalCase(String input) {
        String camelCase = toCamelCase(input);
        return capitalize(camelCase);
    }
    
    /**
     * Capitalizes first letter of string
     */
    private String capitalize(String input) {
        if (input == null || input.isEmpty()) return input;
        return input.substring(0, 1).toUpperCase() + input.substring(1).toLowerCase();
    }
}