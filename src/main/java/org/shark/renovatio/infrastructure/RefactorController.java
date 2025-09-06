package org.shark.renovatio.infrastructure;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.shark.renovatio.domain.RefactorRequest;
import org.shark.renovatio.domain.RefactorResponse;
import org.shark.renovatio.application.RefactorService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/api/refactor")
@Tag(name = "Refactor")
public class RefactorController {
    @Autowired
    private RefactorService refactorService;

    @PostMapping
    @Operation(summary = "Refactoriza código Java usando OpenRewrite")
    public RefactorResponse refactor(@RequestBody RefactorRequest request) {
        return refactorService.refactor(request);
    }
}
