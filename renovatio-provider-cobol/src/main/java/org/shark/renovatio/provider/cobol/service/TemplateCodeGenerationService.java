package org.shark.renovatio.provider.cobol.service;

import freemarker.template.Configuration;
import freemarker.template.Template;
import freemarker.template.TemplateException;
import org.springframework.stereotype.Service;

import java.io.IOException;
import java.io.StringReader;
import java.io.StringWriter;
import java.util.HashMap;
import java.util.Map;
import java.util.List;
import java.util.ArrayList;
import java.util.Locale;
import com.github.mustachejava.DefaultMustacheFactory;
import com.github.mustachejava.Mustache;

/**
 * Template-based code generation service using Freemarker
 * Generates more sophisticated Java code from templates
 */
@Service
public class TemplateCodeGenerationService {
    
    private final Configuration freemarkerConfig;
    
    public TemplateCodeGenerationService() {
        this.freemarkerConfig = new Configuration(Configuration.VERSION_2_3_32);
        freemarkerConfig.setDefaultEncoding("UTF-8");
    }
    
    /**
     * Generates Java service class from template
     */
    public String generateServiceClass(Map<String, Object> templateData) throws IOException, TemplateException {
        String templateContent = """
        package org.shark.renovatio.generated.cobol;
        
        import org.springframework.stereotype.Service;
        import java.math.BigDecimal;
        import java.util.List;
        import java.util.Map;
        
        /**
         * Generated service for COBOL program: ${programName}
         * Original file: ${fileName}
         */
        @Service
        public class ${className} {
        
        <#list dataItems as item>
            // COBOL: ${item.level} ${item.name} PIC ${item.picture}
        </#list>
        
            /**
             * Main business logic method
             */
            public ${className}Response process(${className}Request request) {
                ${className}Response response = new ${className}Response();
                
                // TODO: Implement business logic from COBOL program
                <#list businessRules as rule>
                // ${rule}
                </#list>
                
                return response;
            }
            
            /**
             * Validation method
             */
            public boolean validate(${className}Request request) {
                if (request == null) {
                    return false;
                }
                
                <#list validationRules as validation>
                // ${validation}
                </#list>
                
                return true;
            }
        }
        """;
        
        Template template = new Template("serviceClass", new StringReader(templateContent), freemarkerConfig);
        StringWriter writer = new StringWriter();
        template.process(templateData, writer);
        return writer.toString();
    }
    
    /**
     * Generates Java DTO class from template
     */
    public String generateDtoClass(Map<String, Object> templateData) throws IOException, TemplateException {
        String templateContent = """
        package org.shark.renovatio.generated.cobol;
        
        import java.math.BigDecimal;
        import java.util.Objects;
        
        /**
         * Generated DTO for COBOL program: ${programName}
         * Represents COBOL data structures in Java
         */
        public class ${className} {
        
        <#list fields as field>
            /**
             * COBOL: ${field.level} ${field.cobolName} PIC ${field.picture}
             */
            private ${field.javaType} ${field.javaName};
            
        </#list>
        
            // Default constructor
            public ${className}() {}
            
            // Constructor with all fields
            public ${className}(<#list fields as field>${field.javaType} ${field.javaName}<#if field_has_next>, </#if></#list>) {
        <#list fields as field>
                this.${field.javaName} = ${field.javaName};
        </#list>
            }
            
        <#list fields as field>
            /**
             * Getter for ${field.javaName}
             */
            public ${field.javaType} get${field.javaName?cap_first}() {
                return ${field.javaName};
            }
            
            /**
             * Setter for ${field.javaName}
             */
            public void set${field.javaName?cap_first}(${field.javaType} ${field.javaName}) {
                this.${field.javaName} = ${field.javaName};
            }
            
        </#list>
        
            @Override
            public boolean equals(Object o) {
                if (this == o) return true;
                if (o == null || getClass() != o.getClass()) return false;
                ${className} that = (${className}) o;
                return <#list fields as field>Objects.equals(${field.javaName}, that.${field.javaName})<#if field_has_next> &&
                       </#if></#list>;
            }
            
            @Override
            public int hashCode() {
                return Objects.hash(<#list fields as field>${field.javaName}<#if field_has_next>, </#if></#list>);
            }
            
            @Override
            public String toString() {
                return "${className}{" +
        <#list fields as field>
                       "${field.javaName}=" + ${field.javaName} +<#if field_has_next>
                       ", " +</#if>
        </#list>
                       '}';
            }
        }
        """;
        
        Template template = new Template("dtoClass", new StringReader(templateContent), freemarkerConfig);
        StringWriter writer = new StringWriter();
        template.process(templateData, writer);
        return writer.toString();
    }
    
    /**
     * Generates Spring Boot REST controller from template
     */
    public String generateRestController(Map<String, Object> templateData) throws IOException, TemplateException {
        String templateContent = """
        package org.shark.renovatio.generated.cobol;
        
        import org.springframework.web.bind.annotation.*;
        import org.springframework.http.ResponseEntity;
        import io.swagger.v3.oas.annotations.Operation;
        import io.swagger.v3.oas.annotations.tags.Tag;
        
        /**
         * Generated REST controller for COBOL program: ${programName}
         * Exposes COBOL business logic as REST endpoints
         */
        @RestController
        @RequestMapping("/api/cobol/${programName?lower_case}")
        @Tag(name = "${programName}", description = "COBOL program ${programName} operations")
        public class ${className}Controller {
        
            private final ${serviceName} service;
            
            public ${className}Controller(${serviceName} service) {
                this.service = service;
            }
            
            @PostMapping("/process")
            @Operation(summary = "Process ${programName} business logic")
            public ResponseEntity<${responseName}> process(@RequestBody ${requestName} request) {
                if (!service.validate(request)) {
                    return ResponseEntity.badRequest().build();
                }
                
                ${responseName} response = service.process(request);
                return ResponseEntity.ok(response);
            }
            
            @PostMapping("/validate")
            @Operation(summary = "Validate ${programName} input data")
            public ResponseEntity<Boolean> validate(@RequestBody ${requestName} request) {
                boolean isValid = service.validate(request);
                return ResponseEntity.ok(isValid);
            }
        }
        """;
        
        Template template = new Template("restController", new StringReader(templateContent), freemarkerConfig);
        StringWriter writer = new StringWriter();
        template.process(templateData, writer);
        return writer.toString();
    }

    /**
     * Generates a simple REST controller exposing CICS transactions as endpoints.
     */
    public String generateCicsController(Map<String, Object> templateData) throws IOException, TemplateException {
        String templateContent = """
        package org.shark.renovatio.generated.cobol;

        import org.springframework.web.bind.annotation.*;
        import org.springframework.http.ResponseEntity;
        import java.util.Map;
        import org.shark.renovatio.provider.cobol.service.CicsService;

        /**
         * Generated controller that forwards REST calls to CICS transactions.
         */
        @RestController
        @RequestMapping("/api/cics")
        public class ${className} {

            private final CicsService cicsService;

            public ${className}(CicsService cicsService) {
                this.cicsService = cicsService;
            }

            <#list transactions as tx>
            @PostMapping("/${tx?lower_case}")
            public ResponseEntity<String> ${tx?lower_case}(@RequestBody Map<String, Object> payload) {
                String result = cicsService.invokeTransaction("${tx}", payload);
                return ResponseEntity.ok(result);
            }
            </#list>
        }
        """;

        Template template = new Template("cicsController", new StringReader(templateContent), freemarkerConfig);
        StringWriter writer = new StringWriter();
        template.process(templateData, writer);
        return writer.toString();
    }
    
    /**
     * Generates MapStruct mapper interface
     */
    public String generateMapper(Map<String, Object> templateData) throws IOException, TemplateException {
        String templateContent = """
        package org.shark.renovatio.generated.cobol;
        
        import org.mapstruct.Mapper;
        import org.mapstruct.Mapping;
        import org.mapstruct.factory.Mappers;
        
        /**
         * Generated MapStruct mapper for COBOL program: ${programName}
         * Maps between COBOL structures and Java DTOs
         */
        @Mapper(componentModel = "spring")
        public interface ${className}Mapper {
        
            ${className}Mapper INSTANCE = Mappers.getMapper(${className}Mapper.class);
            
        <#list mappings as mapping>
            @Mapping(source = "${mapping.source}", target = "${mapping.target}")
        </#list>
            ${targetClass} map(${sourceClass} source);
            
        <#list mappings as mapping>
            @Mapping(source = "${mapping.target}", target = "${mapping.source}")
        </#list>
            ${sourceClass} mapReverse(${targetClass} target);
        }
        """;
        
        Template template = new Template("mapper", new StringReader(templateContent), freemarkerConfig);
        StringWriter writer = new StringWriter();
        template.process(templateData, writer);
        return writer.toString();
    }

    /**
     * Generates a simple exporter class that produces SQL insert statements for RDBMS targets.
     */
    public String generateRdbmsExporter(Map<String, Object> templateData) throws IOException, TemplateException {
        String templateContent = """
        package org.shark.renovatio.generated.cobol;

        /**
         * Utility class for exporting DTOs to a relational database.
         */
        public class ${className}RdbmsExporter {

            public String toInsertSql(${className} dto) {
                // Simplified example - build INSERT statement from DTO
                return "INSERT INTO ${tableName} (...) VALUES (...);";
            }
        }
        """;

        Template template = new Template("rdbmsExporter", new StringReader(templateContent), freemarkerConfig);
        StringWriter writer = new StringWriter();
        template.process(templateData, writer);
        return writer.toString();
    }

    /**
     * Generates a basic exporter class that uploads DTO representations to S3 using Mustache templates.
     */
    public String generateS3Exporter(Map<String, Object> templateData) throws IOException {
        String templateContent = "package org.shark.renovatio.generated.cobol;\n\n" +
                "public class {{className}}S3Exporter {\n" +
                "    public void export({{className}} dto) {\n" +
                "        // TODO: stream DTO to S3 bucket {{bucketName}}\n" +
                "    }\n" +
                "}\n";
        DefaultMustacheFactory mf = new DefaultMustacheFactory();
        Mustache mustache = mf.compile(new StringReader(templateContent), "s3Exporter");
        StringWriter writer = new StringWriter();
        mustache.execute(writer, templateData).flush();
        return writer.toString();
    }

    /**
     * Generate a set of Java artifacts (service, DTO, controller and mapper)
     * from a parsed copybook structure.
     */
    public Map<String, String> generateFromCopybook(String copybookName, Map<String, Object> metadata)
            throws IOException, TemplateException {
        String baseName = toPascalCase(copybookName);
        Map<String, String> generated = new HashMap<>();

        // Service class
        Map<String, Object> serviceData = new HashMap<>();
        serviceData.put("programName", baseName);
        serviceData.put("fileName", metadata.getOrDefault("filePath", copybookName));
        serviceData.put("className", baseName + "Service");
        serviceData.put("dataItems", metadata.get("dataItems"));
        serviceData.put("businessRules", List.of());
        serviceData.put("validationRules", List.of());
        generated.put(baseName + "Service.java", generateServiceClass(serviceData));

        // DTO class
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> items = (List<Map<String, Object>>) metadata.get("dataItems");
        List<Map<String, Object>> fields = new ArrayList<>();
        if (items != null) {
            for (Map<String, Object> item : items) {
                String cobolName = (String) item.get("name");
                Map<String, Object> field = new HashMap<>();
                field.put("level", item.get("level"));
                field.put("cobolName", cobolName);
                field.put("picture", item.get("picture"));
                field.put("javaName", toCamelCase(cobolName));
                field.put("javaType", mapPictureToJavaType((String) item.get("picture")));
                fields.add(field);
            }
        }
        Map<String, Object> dtoData = new HashMap<>();
        dtoData.put("programName", baseName);
        dtoData.put("className", baseName + "DTO");
        dtoData.put("fields", fields);
        generated.put(baseName + "DTO.java", generateDtoClass(dtoData));

        // REST controller
        Map<String, Object> controllerData = new HashMap<>();
        controllerData.put("programName", baseName);
        controllerData.put("className", baseName);
        controllerData.put("serviceName", baseName + "Service");
        controllerData.put("requestName", baseName + "Request");
        controllerData.put("responseName", baseName + "Response");
        generated.put(baseName + "Controller.java", generateRestController(controllerData));

        // Mapper
        Map<String, Object> mapperData = new HashMap<>();
        mapperData.put("programName", baseName);
        mapperData.put("className", baseName + "Mapper");
        mapperData.put("sourceClass", baseName + "DTO");
        mapperData.put("targetClass", baseName + "Entity");
        mapperData.put("mappings", List.of());
        generated.put(baseName + "Mapper.java", generateMapper(mapperData));

        return generated;
    }

    private String toPascalCase(String text) {
        if (text == null || text.isEmpty()) return "";
        String[] parts = text.toLowerCase(Locale.ROOT).split("[^a-z0-9]");
        StringBuilder sb = new StringBuilder();
        for (String part : parts) {
            if (part.isEmpty()) continue;
            sb.append(Character.toUpperCase(part.charAt(0))).append(part.substring(1));
        }
        return sb.toString();
    }

    private String toCamelCase(String text) {
        String pascal = toPascalCase(text);
        if (pascal.isEmpty()) return pascal;
        return Character.toLowerCase(pascal.charAt(0)) + pascal.substring(1);
    }

    private String mapPictureToJavaType(String picture) {
        if (picture == null) {
            return "String";
        }
        if (picture.contains("9")) {
            return "Integer";
        }
        return "String";
    }
}
