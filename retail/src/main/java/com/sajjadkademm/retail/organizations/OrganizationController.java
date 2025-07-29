package com.sajjadkademm.retail.organizations;

import com.sajjadkademm.retail.organizations.dto.CreateOrganizationRequest;
import com.sajjadkademm.retail.organizations.dto.OrganizationResponse;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import jakarta.validation.Valid;
import java.util.List;

@Slf4j
@RestController
@RequestMapping("/api/organizations")
@RequiredArgsConstructor
public class OrganizationController {

    private final OrganizationService organizationService;

    /**
     * Create organization endpoint
     */
    @PostMapping
    public ResponseEntity<OrganizationResponse> createOrganization(
            @Valid @RequestBody CreateOrganizationRequest request,
            @RequestHeader(value = "User-ID", required = false) String userId) {
        OrganizationResponse response = organizationService.createOrganization(request, userId);
        return ResponseEntity.ok(response);
    }

    /**
     * Get organization by ID endpoint
     */
    @GetMapping("/{id}")
    public ResponseEntity<OrganizationResponse> getOrganizationById(@PathVariable String id) {
        OrganizationResponse response = organizationService.getOrganizationById(id);
        return ResponseEntity.ok(response);
    }

    /**
     * Get all organizations endpoint
     */
    @GetMapping
    public ResponseEntity<List<OrganizationResponse>> getAllOrganizations() {
        List<OrganizationResponse> response = organizationService.getAllOrganizations();
        return ResponseEntity.ok(response);
    }

    /**
     * Search organizations endpoint
     */
    @GetMapping("/search")
    public ResponseEntity<List<OrganizationResponse>> searchOrganizations(@RequestParam String q) {
        List<OrganizationResponse> response = organizationService.searchOrganizations(q);
        return ResponseEntity.ok(response);
    }

    /**
     * Check if organization exists by domain
     */
    @GetMapping("/exists/domain/{domain}")
    public ResponseEntity<Boolean> organizationExistsByDomain(@PathVariable String domain) {
        boolean exists = organizationService.organizationExistsByDomain(domain);
        return ResponseEntity.ok(exists);
    }
}