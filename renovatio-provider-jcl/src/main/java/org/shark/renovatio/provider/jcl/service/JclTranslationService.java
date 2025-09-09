package org.shark.renovatio.provider.jcl.service;

import org.shark.renovatio.provider.jcl.domain.JclStep;
import org.springframework.stereotype.Service;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Basic parser and translator for JCL scripts.
 * Extracts EXEC steps and can convert them into shell scripts or
 * simple CI workflow steps (GitHub Actions style).
 */
@Service
public class JclTranslationService {
    private static final Pattern STEP_PATTERN =
            Pattern.compile("^//([^\s]+)\\s+EXEC\\s+PGM=([^\\s,]+)", Pattern.CASE_INSENSITIVE);

    /**
     * Parse the provided JCL content and extract executable steps.
     */
    public Object parseAst(String jclContent) {
        try {
            Class<?> parserClass = Class.forName("org.zowe.jclparser.JclParser");
            Object parser = parserClass.getDeclaredConstructor().newInstance();
            Method parse = parserClass.getMethod("parse", String.class);
            return parse.invoke(parser, jclContent);
        } catch (Exception e) {
            return null;
        }
    }

    /**
     * Parse the provided JCL content and extract executable steps.
     * Uses jcl4j when available, otherwise falls back to a regex parser.
     */
    public List<JclStep> parseSteps(String jclContent) {
        List<JclStep> steps = new ArrayList<>();
        Object ast = parseAst(jclContent);
        if (ast != null) {
            try {
                Method getSteps = ast.getClass().getMethod("getSteps");
                List<?> parsedSteps = (List<?>) getSteps.invoke(ast);
                for (Object s : parsedSteps) {
                    Method getName = s.getClass().getMethod("getName");
                    Method getProgram = s.getClass().getMethod("getProgram");
                    String name = (String) getName.invoke(s);
                    String program = (String) getProgram.invoke(s);
                    steps.add(new JclStep(name, program));
                }
                return steps;
            } catch (Exception ignored) {
                // fall back to regex parsing
            }
        }
        if (jclContent == null) {
            return steps;
        }
        for (String line : jclContent.split("\\R")) {
            Matcher m = STEP_PATTERN.matcher(line.trim());
            if (m.find()) {
                String name = m.group(1);
                String program = m.group(2);
                steps.add(new JclStep(name, program));
            }
        }
        return steps;
    }

    /**
     * Convert JCL steps into a simple shell script.
     */
    public String toShellScript(String jclContent) {
        List<JclStep> steps = parseSteps(jclContent);
        StringBuilder sb = new StringBuilder();
        sb.append("#!/bin/sh\n");
        sb.append("# Generated from JCL\n");
        for (JclStep step : steps) {
            sb.append("# ").append(step.getName()).append('\n');
            sb.append(step.getProgram()).append("\n");
        }
        return sb.toString();
    }

    /**
     * Convert JCL steps into a minimal GitHub Actions workflow.
     */
    public String toGithubActions(String jclContent) {
        List<JclStep> steps = parseSteps(jclContent);
        StringBuilder sb = new StringBuilder();
        sb.append("jobs:\n");
        sb.append("  jcl:\n");
        sb.append("    runs-on: ubuntu-latest\n");
        sb.append("    steps:\n");
        for (JclStep step : steps) {
            sb.append("      - name: ").append(step.getName()).append("\n");
            sb.append("        run: ").append(step.getProgram()).append("\n");
        }
        return sb.toString();
    }

    /**
     * Convert JCL steps into a simple Spring Batch XML job definition.
     */
    public String toSpringBatchJob(String jclContent) {
        List<JclStep> steps = parseSteps(jclContent);
        StringBuilder sb = new StringBuilder();
        sb.append("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n");
        sb.append("<beans xmlns=\"http://www.springframework.org/schema/beans\"\n");
        sb.append("       xmlns:batch=\"http://www.springframework.org/schema/batch\"\n");
        sb.append("       xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"\n");
        sb.append("       xsi:schemaLocation=\"http://www.springframework.org/schema/beans https://www.springframework.org/schema/beans/spring-beans.xsd\n");
        sb.append("                           http://www.springframework.org/schema/batch https://www.springframework.org/schema/batch/spring-batch.xsd\">\n");
        sb.append("  <batch:job id=\"jclJob\">\n");
        for (JclStep step : steps) {
            sb.append("    <batch:step id=\"").append(step.getName()).append("\">\n");
            sb.append("      <batch:tasklet>\n");
            sb.append("        <batch:script>").append(step.getProgram()).append("</batch:script>\n");
            sb.append("      </batch:tasklet>\n");
            sb.append("    </batch:step>\n");
        }
        sb.append("  </batch:job>\n");
        sb.append("</beans>\n");
        return sb.toString();
    }

    /**
     * Convert JCL steps into a minimal Airflow DAG using BashOperator tasks.
     */
    public String toAirflowDag(String jclContent) {
        List<JclStep> steps = parseSteps(jclContent);
        StringBuilder sb = new StringBuilder();
        sb.append("from airflow import DAG\n");
        sb.append("from airflow.operators.bash import BashOperator\n");
        sb.append("from datetime import datetime\n\n");
        sb.append("with DAG('jcl_job', start_date=datetime(2024, 1, 1), schedule_interval=None) as dag:\n");
        JclStep previous = null;
        for (JclStep step : steps) {
            String taskId = step.getName().toLowerCase();
            sb.append("    ").append(taskId).append(" = BashOperator(task_id='")
              .append(taskId).append("', bash_command='")
              .append(step.getProgram()).append("')\n");
            if (previous != null) {
                sb.append("    ")
                  .append(previous.getName().toLowerCase())
                  .append(" >> ")
                  .append(taskId)
                  .append("\n");
            }
            previous = step;
        }
        return sb.toString();
    }
}
