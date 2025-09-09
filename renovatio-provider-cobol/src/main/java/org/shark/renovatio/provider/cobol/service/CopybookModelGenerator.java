package org.shark.renovatio.provider.cobol.service;

import net.sf.cb2xml.Cb2Xml;
import net.sf.JRecord.External.CopybookLoader;
import net.sf.JRecord.External.CopybookLoaderFactory;
import net.sf.JRecord.External.ExternalRecord;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.Details.FieldDetail;
import net.sf.JRecord.Numeric.ICopybookDialects;

import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;

/**
 * Service that evaluates CB2XML and jRecord for generating simple
 * Java model representations from COBOL copybooks.
 */
public class CopybookModelGenerator {

    /**
     * Converts a COBOL copybook into its XML representation using CB2XML.
     *
     * @param copybookPath path to the copybook file
     * @return XML representation of the copybook
     */
    public String copybookToXml(Path copybookPath) throws IOException {
        try (InputStream is = Files.newInputStream(copybookPath)) {
            return Cb2Xml.convertToXMLString(is);
        }
    }

    /**
     * Loads a copybook as a jRecord {@link ExternalRecord}.
     */
    public ExternalRecord loadExternalRecord(Path copybookPath) throws IOException {
        return CopybookLoaderFactory.getInstance()
                .getLoader(CopybookLoaderFactory.COBOL_LOADER)
                .loadCopyBook(copybookPath.toString(), CopybookLoader.SPLIT_01_LEVEL,
                        0, "", ICopybookDialects.FMT_MAINFRAME, 0, 0, null);
    }

    /**
     * Generates a very simple Java POJO with String fields for every copybook field.
     * This demonstrates the capability of jRecord to provide structural information
     * that can drive Java model generation.
     */
    public String generateJavaModel(Path copybookPath, String className) throws IOException {
        ExternalRecord record = loadExternalRecord(copybookPath);
        LayoutDetail layout = record.asLayoutDetail();
        StringBuilder sb = new StringBuilder("public class ").append(className).append(" {\n");
        for (int i = 0; i < layout.getFieldCount(); i++) {
            FieldDetail field = layout.getField(i);
            sb.append("    private String ").append(field.getName()).append(";\n");
        }
        sb.append("}\n");
        return sb.toString();
    }
}
