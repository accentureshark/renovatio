package org.shark.renovatio.provider.cobol.service;

import freemarker.template.Configuration;
import freemarker.template.Template;
import freemarker.template.TemplateException;
import org.springframework.stereotype.Service;

import java.io.IOException;
import java.io.StringReader;
import java.io.StringWriter;
import java.nio.file.Path;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Service that migrates embedded DB2 EXEC SQL statements in COBOL programs
 * into simple JPA entity and repository classes using template generation.
 */
@Service
public class Db2MigrationService {

    private final CobolParsingService parsingService;
    private final Configuration freemarkerConfig;

    public Db2MigrationService(CobolParsingService parsingService) {
        this.parsingService = parsingService;
        this.freemarkerConfig = new Configuration(Configuration.VERSION_2_3_32);
        freemarkerConfig.setDefaultEncoding("UTF-8");
    }

    /**
     * Generate JPA artifacts from EXEC SQL statements found in a COBOL file.
     * The resulting map contains file names mapped to their Java source code.
     */
    public Map<String, String> migrateCobolFile(Path cobolFile) throws IOException, TemplateException {
        List<String> statements = parsingService.extractExecSqlStatements(cobolFile);
        Map<String, String> generated = new HashMap<>();
        for (String sql : statements) {
            String table = extractTableName(sql);
            if (table == null) {
                continue;
            }
            String entity = toPascalCase(table);
            Map<String, Object> data = Map.of("entity", entity, "table", table);
            generated.put(entity + ".java", generateEntity(data));
            generated.put(entity + "Repository.java", generateRepository(data));
        }
        return generated;
    }

    private String extractTableName(String sql) {
        Pattern p = Pattern.compile("from\\s+([a-zA-Z0-9_]+)", Pattern.CASE_INSENSITIVE);
        Matcher m = p.matcher(sql);
        if (m.find()) {
            return m.group(1);
        }
        p = Pattern.compile("into\\s+([a-zA-Z0-9_]+)", Pattern.CASE_INSENSITIVE);
        m = p.matcher(sql);
        if (m.find()) {
            return m.group(1);
        }
        return null;
    }

    private String generateEntity(Map<String, Object> data) throws IOException, TemplateException {
        String templateContent = """
                package org.shark.renovatio.generated.cobol;
                
                import jakarta.persistence.Entity;
                import jakarta.persistence.Id;
                
                @Entity
                public class ${entity} {
                    @Id
                    private Long id;
                    // TODO: map columns from ${table}
                }
                """;
        Template template = new Template("entity", new StringReader(templateContent), freemarkerConfig);
        StringWriter writer = new StringWriter();
        template.process(data, writer);
        return writer.toString();
    }

    private String generateRepository(Map<String, Object> data) throws IOException, TemplateException {
        String templateContent = """
                package org.shark.renovatio.generated.cobol;
                
                import org.springframework.data.jpa.repository.JpaRepository;
                
                public interface ${entity}Repository extends JpaRepository<${entity}, Long> {
                    // TODO: customise queries for ${table}
                }
                """;
        Template template = new Template("repository", new StringReader(templateContent), freemarkerConfig);
        StringWriter writer = new StringWriter();
        template.process(data, writer);
        return writer.toString();
    }

    private String toPascalCase(String text) {
        if (text == null || text.isEmpty()) {
            return "";
        }
        String[] parts = text.toLowerCase(Locale.ROOT).split("[^a-z0-9]");
        StringBuilder sb = new StringBuilder();
        for (String part : parts) {
            if (part.isEmpty()) {
                continue;
            }
            sb.append(Character.toUpperCase(part.charAt(0))).append(part.substring(1));
        }
        return sb.toString();
    }
}

