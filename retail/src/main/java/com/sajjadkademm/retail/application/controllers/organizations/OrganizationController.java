package com.sajjadkademm.retail.application.controllers.organizations;

import com.sajjadkademm.retail.domain.organization.model.Organization;
import com.sajjadkademm.retail.application.services.organizations.OrganizationService;
import com.sajjadkademm.retail.application.dto.organizations.CreateOrganizationRequest;
import com.sajjadkademm.retail.application.dto.organizations.UpdateOrganizationRequest;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import jakarta.validation.Valid;
import java.util.List;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.ExampleObject;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.tags.Tag;

/**
 * Organization management controller providing CRUD operations for
 * organizations. All operations are scoped to the current authenticated user.
 * 
 * @author Sajjad Kadem
 * @version 1.0
 * @since 2024-12-19
 */
@Slf4j
@RestController
@RequestMapping("/api/organizations")
@RequiredArgsConstructor
@Tag(name = "Organizations", description = "Organization management endpoints (user-scoped)")
public class OrganizationController {

    private final OrganizationService organizationService;

    /**
     * Create organization endpoint
     * Creates a new organization for the current authenticated user
     */
    @Operation(summary = "Create Organization", description = "Create a new organization for the current authenticated user", operationId = "createOrganization")
    @ApiResponse(responseCode = "200", description = "Organization created successfully", content = @Content(mediaType = "application/json", schema = @Schema(implementation = Organization.class), examples = @ExampleObject(name = "Created Organization", value = """
            {
                "id": "org123",
                "name": "Acme Corporation",
                "domain": "acme.com",
                "description": "A leading retail company specializing in electronics and home appliances",
                "address": "123 Business Street, New York, NY 10001",
                "phone": "+1-555-123-4567",
                "status": "ACTIVE",
                "createdAt": "2024-12-19T10:30:00",
                "updatedAt": "2024-12-19T10:30:00",
                "createdBy": "user123"
            }
            """)))
    @PostMapping
    public ResponseEntity<Organization> createOrganization(
            @Parameter(description = "Organization creation request", required = true, content = @Content(schema = @Schema(implementation = CreateOrganizationRequest.class), examples = @ExampleObject(name = "Create Organization Request", value = """
                    {
                        "name": "Acme Corporation",
                        "domain": "acme.com",
                        "description": "A leading retail company specializing in electronics and home appliances",
                        "address": "123 Business Street, New York, NY 10001",
                        "phone": "+1-555-123-4567"
                    }
                    """))) @Valid @RequestBody CreateOrganizationRequest request) {
        Organization response = organizationService.createOrganization(request);
        return ResponseEntity.ok(response);
    }

    /**
     * Update organization endpoint
     * Updates an organization (only if the current user is the creator)
     */
    @Operation(summary = "Update Organization", description = "Update an existing organization (only if current user is the creator)", operationId = "updateOrganization")
    @ApiResponse(responseCode = "200", description = "Organization updated successfully", content = @Content(mediaType = "application/json", schema = @Schema(implementation = Organization.class), examples = @ExampleObject(name = "Updated Organization", value = """
            {
                "id": "org123",
                "name": "Acme Corporation Updated",
                "domain": "acme.com",
                "description": "Updated description for the leading retail company",
                "address": "456 Updated Business Ave, New York, NY 10002",
                "phone": "+1-555-123-4567",
                "status": "DISABLED",
                "createdAt": "2024-12-19T10:30:00",
                "updatedAt": "2024-12-19T11:30:00",
                "createdBy": "user123"
            }
            """)))
    @PutMapping("/{id}")
    public ResponseEntity<Organization> updateOrganization(
            @Parameter(description = "Organization ID", required = true, example = "org123") @PathVariable String id,
            @Parameter(description = "Organization update request", required = true, content = @Content(schema = @Schema(implementation = UpdateOrganizationRequest.class), examples = @ExampleObject(name = "Update Organization Request", value = """
                    {
                        "name": "Acme Corporation Updated",
                        "description": "Updated description for the leading retail company",
                        "address": "456 Updated Business Ave, New York, NY 10002",
                        "status": "DISABLED"
                    }
                    """))) @Valid @RequestBody UpdateOrganizationRequest request) {
        Organization response = organizationService.updateOrganization(id, request);
        return ResponseEntity.ok(response);
    }

    /**
     * Get organization by ID endpoint
     * Returns organization details only if the current user is the creator
     */
    @Operation(summary = "Get Organization by ID", description = "Retrieve organization details by ID (only if current user is the creator)", operationId = "getOrganizationById")
    @ApiResponse(responseCode = "200", description = "Organization found", content = @Content(mediaType = "application/json", schema = @Schema(implementation = Organization.class), examples = @ExampleObject(name = "Organization Details", value = """
            {
                "id": "org123",
                "name": "Acme Corporation",
                "domain": "acme.com",
                "description": "A leading retail company specializing in electronics and home appliances",
                "address": "123 Business Street, New York, NY 10001",
                "phone": "+1-555-123-4567",
                "status": "ACTIVE",
                "createdAt": "2024-12-19T10:30:00",
                "updatedAt": "2024-12-19T10:30:00",
                "createdBy": "user123"
            }
            """)))
    @GetMapping("/{id}")
    public ResponseEntity<Organization> getOrganizationById(
            @Parameter(description = "Organization ID", required = true, example = "org123") @PathVariable String id) {
        Organization response = organizationService.getOrganizationById(id);
        return ResponseEntity.ok(response);
    }

    /**
     * Get all organizations for the current user
     */
    @Operation(summary = "Get User Organizations", description = "Retrieve all organizations created by the current authenticated user", operationId = "getUserOrganizations")
    @ApiResponse(responseCode = "200", description = "List of user organizations retrieved successfully", content = @Content(mediaType = "application/json", schema = @Schema(implementation = Organization.class, type = "array"), examples = @ExampleObject(name = "User Organizations List", value = """
            [
                {
                    "id": "org123",
                    "name": "Acme Corporation",
                    "domain": "acme.com",
                    "description": "A leading retail company",
                    "address": "123 Business Street, New York, NY 10001",
                    "phone": "+1-555-123-4567",
                    "status": "ACTIVE",
                    "createdAt": "2024-12-19T10:30:00",
                    "updatedAt": "2024-12-19T10:30:00",
                    "createdBy": "user123"
                }
            ]
            """)))
    @GetMapping
    public ResponseEntity<List<Organization>> getUserOrganizations() {
        List<Organization> response = organizationService.getAllOrganizations();
        return ResponseEntity.ok(response);
    }

    /**
     * Search organizations for the current user
     */
    @Operation(summary = "Search User Organizations", description = "Search organizations created by the current authenticated user", operationId = "searchUserOrganizations")
    @ApiResponse(responseCode = "200", description = "Search results retrieved successfully", content = @Content(mediaType = "application/json", schema = @Schema(implementation = Organization.class, type = "array"), examples = @ExampleObject(name = "Search Results", value = """
            [
                {
                    "id": "org123",
                    "name": "Acme Corporation",
                    "domain": "acme.com",
                    "description": "A leading retail company",
                    "address": "123 Business Street, New York, NY 10001",
                    "phone": "+1-555-123-4567",
                    "status": "ACTIVE",
                    "createdAt": "2024-12-19T10:30:00",
                    "updatedAt": "2024-12-19T10:30:00",
                    "createdBy": "user123"
                }
            ]
            """)))
    @GetMapping("/search")
    public ResponseEntity<List<Organization>> searchUserOrganizations(
            @Parameter(description = "Search query for organization name or domain", required = true, example = "acme") @RequestParam String q) {
        List<Organization> response = organizationService.searchOrganizations(q);
        return ResponseEntity.ok(response);
    }

}