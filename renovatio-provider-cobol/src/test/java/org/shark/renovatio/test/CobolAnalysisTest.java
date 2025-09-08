package org.shark.renovatio.test;

import org.shark.renovatio.provider.cobol.service.CobolParsingService;
import org.shark.renovatio.shared.domain.AnalyzeResult;
import org.shark.renovatio.shared.domain.Workspace;
import org.shark.renovatio.shared.nql.NqlQuery;

public class CobolAnalysisTest {
    public static void main(String[] args) {
        CobolParsingService service = new CobolParsingService();
        
        Workspace workspace = new Workspace();
        workspace.setId("test-workspace");
        workspace.setPath("/home/faguero/accenture/renovatio/samples/cobol/Cobol-Programming-Collection");
        workspace.setBranch("main");
        
        NqlQuery query = new NqlQuery();
        query.setType(NqlQuery.QueryType.FIND);
        query.setTarget("programs");
        query.setLanguage("cobol");
        
        System.out.println("=== PRUEBA DIRECTA DEL SERVICIO DE PARSING COBOL ===");
        System.out.println("Directorio: " + workspace.getPath());
        
        try {
            AnalyzeResult result = service.analyzeCOBOL(query, workspace);
            
            System.out.println("Éxito: " + result.isSuccess());
            System.out.println("Mensaje: " + result.getMessage());
            System.out.println("Datos: " + result.getData());
            
            if (result.getData() != null) {
                System.out.println("Contenido de datos: " + result.getData().toString());
            }
            
        } catch (Exception e) {
            System.out.println("Error: " + e.getMessage());
            e.printStackTrace();
        }
    }
}
