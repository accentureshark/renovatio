package org.shark.renovatio.web;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.context.annotation.ComponentScan;

@SpringBootApplication
@EnableScheduling
@ComponentScan({
    "org.shark.renovatio.web",
    "org.shark.renovatio.core",
    "org.shark.renovatio.provider.java",
    "org.shark.renovatio.provider.cobol",
    "org.shark.renovatio.shared.nql"
})
public class RenovatioWebApplication {
    public static void main(String[] args) {
        SpringApplication.run(RenovatioWebApplication.class, args);
    }
}
