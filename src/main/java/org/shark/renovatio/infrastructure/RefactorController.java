package org.shark.renovatio.infrastructure;

import org.shark.renovatio.domain.RefactorRequest;
import org.shark.renovatio.domain.RefactorResponse;
import org.shark.renovatio.application.RefactorService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/api/refactor")
public class RefactorController {
    @Autowired
    private RefactorService refactorService;

    @PostMapping
    public RefactorResponse refactor(@RequestBody RefactorRequest request) {
        return refactorService.refactorCode(request);
    }
}
