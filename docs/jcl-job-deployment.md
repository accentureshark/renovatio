# Despliegue de Jobs Generados desde JCL

Los jobs convertidos desde JCL pueden ejecutarse en distintos frameworks de orquestación. A continuación se describen opciones para desplegarlos en **Spring Batch**, **Airflow** y ejecutarlos en **Kubernetes** u otros schedulers.

## Spring Batch en Kubernetes
1. Construir una imagen de contenedor que incluya la aplicación Spring Boot con el job Spring Batch generado.
2. Definir un `ConfigMap` o `Secret` con el XML del job si se requiere externalizarlo.
3. Crear un `Job` de Kubernetes que invoque la clase `CommandLineJobRunner` o un `CronJob` para ejecuciones periódicas.
4. Monitorear la ejecución mediante los logs del `Pod` o integrando herramientas como Prometheus y Grafana.

## Airflow
1. Colocar el DAG generado en el directorio de DAGs de Airflow.
2. Si se usa Airflow en Kubernetes (mediante Helm o el operador oficial), empaquetar el DAG como parte de la imagen o montarlo como volumen.
3. Configurar las dependencias necesarias en la imagen de Airflow para que los comandos del DAG se ejecuten correctamente.
4. Programar el DAG desde la UI de Airflow o mediante una definición de `schedule_interval` en el archivo Python.

## Otros Schedulers
- **Cron tradicional**: Guardar el script generado y registrarlo con `crontab` en un servidor Unix/Linux.
- **Herramientas de CI/CD**: Integrar el workflow generado en sistemas como Jenkins, GitHub Actions o GitLab CI.

Estas estrategias permiten ejecutar los jobs resultantes de la migración JCL en plataformas modernas, facilitando la adopción de contenedores y orquestadores contemporáneos.
