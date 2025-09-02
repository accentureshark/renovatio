package org.shark.renovatio.controller;

import org.shark.renovatio.model.RefactorRequest;
import org.shark.renovatio.model.RefactorResponse;
import org.shark.renovatio.service.RefactorService;
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
