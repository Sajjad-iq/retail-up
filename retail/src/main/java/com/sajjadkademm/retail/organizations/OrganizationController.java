package com.sajjadkademm.retail.organizations;

import com.sajjadkademm.retail.organizations.dto.CreateOrganizationRequest;
import com.sajjadkademm.retail.organizations.dto.UpdateOrganizationRequest;
import com.sajjadkademm.retail.organizations.dto.OrganizationResponse;

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
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.tags.Tag;

/**
 * Organization management controller providing CRUD operations for
 * organizations.
 * 
 * @author Sajjad Kadem
 * @version 1.0
 * @since 2024-12-19
 */
@Slf4j
@RestController
@RequestMapping("/api/organizations")
@RequiredArgsConstructor
@Tag(name = "Organizations", description = "Organization management endpoints")
public class OrganizationController {

    private final OrganizationService organizationService;

    /**
     * Create organization endpoint
     */
    @Operation(summary = "Create Organization", description = "Create a new organization with the provided details", operationId = "createOrganization")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Organization created successfully", content = @Content(mediaType = "application/json", schema = @Schema(implementation = OrganizationResponse.class), examples = @ExampleObject(name = "Created Organization", value = """
                    {
                        "id": "org123",
                        "name": "Acme Corporation",
                        "domain": "acme.com",
                        "status": "ACTIVE",
                        "createdAt": "2024-12-19T10:30:00",
                        "updatedAt": "2024-12-19T10:30:00"
                    }
                    """))),
            @ApiResponse(responseCode = "400", description = "Bad request - validation errors", content = @Content(mediaType = "application/json", schema = @Schema(implementation = Object.class))),
            @ApiResponse(responseCode = "409", description = "Conflict - organization with same domain already exists", content = @Content(mediaType = "application/json", schema = @Schema(implementation = Object.class)))
    })
    @PostMapping
    public ResponseEntity<OrganizationResponse> createOrganization(
            @Parameter(description = "Organization creation request", required = true, content = @Content(schema = @Schema(implementation = CreateOrganizationRequest.class), examples = @ExampleObject(name = "Create Organization Request", value = """
                    {
                        "name": "Acme Corporation",
                        "domain": "acme.com",
                        "description": "A leading retail company"
                    }
                    """))) @Valid @RequestBody CreateOrganizationRequest request,
            @Parameter(description = "User ID creating the organization", example = "user123") @RequestHeader(value = "User-ID", required = false) String userId) {
        OrganizationResponse response = organizationService.createOrganization(request, userId);
        return ResponseEntity.ok(response);
    }

    /**
     * Update organization endpoint
     */
    @Operation(summary = "Update Organization", description = "Update an existing organization's information", operationId = "updateOrganization")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Organization updated successfully", content = @Content(mediaType = "application/json", schema = @Schema(implementation = OrganizationResponse.class), examples = @ExampleObject(name = "Updated Organization", value = """
                    {
                        "id": "org123",
                        "name": "Acme Corporation Updated",
                        "domain": "acme.com",
                        "status": "ACTIVE",
                        "createdAt": "2024-12-19T10:30:00",
                        "updatedAt": "2024-12-19T11:30:00"
                    }
                    """))),
            @ApiResponse(responseCode = "404", description = "Organization not found", content = @Content(mediaType = "application/json", schema = @Schema(implementation = Object.class))),
            @ApiResponse(responseCode = "400", description = "Bad request - validation errors", content = @Content(mediaType = "application/json", schema = @Schema(implementation = Object.class)))
    })
    @PutMapping("/{id}")
    public ResponseEntity<OrganizationResponse> updateOrganization(
            @Parameter(description = "Organization ID", required = true, example = "org123") @PathVariable String id,
            @Parameter(description = "Organization update request", required = true, content = @Content(schema = @Schema(implementation = UpdateOrganizationRequest.class), examples = @ExampleObject(name = "Update Organization Request", value = """
                    {
                        "name": "Acme Corporation Updated",
                        "description": "Updated description"
                    }
                    """))) @Valid @RequestBody UpdateOrganizationRequest request) {
        OrganizationResponse response = organizationService.updateOrganization(id, request);
        return ResponseEntity.ok(response);
    }

    /**
     * Get organization by ID endpoint
     */
    @Operation(summary = "Get Organization by ID", description = "Retrieve organization details by its unique identifier", operationId = "getOrganizationById")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Organization found", content = @Content(mediaType = "application/json", schema = @Schema(implementation = OrganizationResponse.class), examples = @ExampleObject(name = "Organization Details", value = """
                    {
                        "id": "org123",
                        "name": "Acme Corporation",
                        "domain": "acme.com",
                        "status": "ACTIVE",
                        "createdAt": "2024-12-19T10:30:00",
                        "updatedAt": "2024-12-19T10:30:00"
                    }
                    """))),
            @ApiResponse(responseCode = "404", description = "Organization not found", content = @Content(mediaType = "application/json", schema = @Schema(implementation = Object.class)))
    })
    @GetMapping("/{id}")
    public ResponseEntity<OrganizationResponse> getOrganizationById(
            @Parameter(description = "Organization ID", required = true, example = "org123") @PathVariable String id) {
        OrganizationResponse response = organizationService.getOrganizationById(id);
        return ResponseEntity.ok(response);
    }

    /**
     * Get all organizations endpoint
     */
    @Operation(summary = "Get All Organizations", description = "Retrieve a list of all organizations", operationId = "getAllOrganizations")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "List of organizations retrieved successfully", content = @Content(mediaType = "application/json", schema = @Schema(implementation = OrganizationResponse.class, type = "array"), examples = @ExampleObject(name = "Organizations List", value = """
                    [
                        {
                            "id": "org123",
                            "name": "Acme Corporation",
                            "domain": "acme.com",
                            "status": "ACTIVE",
                            "createdAt": "2024-12-19T10:30:00",
                            "updatedAt": "2024-12-19T10:30:00"
                        },
                        {
                            "id": "org456",
                            "name": "Tech Solutions",
                            "domain": "techsolutions.com",
                            "status": "ACTIVE",
                            "createdAt": "2024-12-19T11:30:00",
                            "updatedAt": "2024-12-19T11:30:00"
                        }
                    ]
                    """)))
    })
    @GetMapping
    public ResponseEntity<List<OrganizationResponse>> getAllOrganizations() {
        List<OrganizationResponse> response = organizationService.getAllOrganizations();
        return ResponseEntity.ok(response);
    }

    /**
     * Search organizations endpoint
     */
    @Operation(summary = "Search Organizations", description = "Search organizations by name or domain", operationId = "searchOrganizations")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Search results retrieved successfully", content = @Content(mediaType = "application/json", schema = @Schema(implementation = OrganizationResponse.class, type = "array"), examples = @ExampleObject(name = "Search Results", value = """
                    [
                        {
                            "id": "org123",
                            "name": "Acme Corporation",
                            "domain": "acme.com",
                            "status": "ACTIVE",
                            "createdAt": "2024-12-19T10:30:00",
                            "updatedAt": "2024-12-19T10:30:00"
                        }
                    ]
                    """)))
    })
    @GetMapping("/search")
    public ResponseEntity<List<OrganizationResponse>> searchOrganizations(
            @Parameter(description = "Search query for organization name or domain", required = true, example = "acme") @RequestParam String q) {
        List<OrganizationResponse> response = organizationService.searchOrganizations(q);
        return ResponseEntity.ok(response);
    }

    /**
     * Check if organization exists by domain
     */
    @Operation(summary = "Check Organization Exists by Domain", description = "Check if an organization exists with the specified domain", operationId = "organizationExistsByDomain")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Organization existence check result", content = @Content(mediaType = "application/json", schema = @Schema(type = "boolean"), examples = {
                    @ExampleObject(name = "Organization Exists", value = "true"),
                    @ExampleObject(name = "Organization Not Found", value = "false")
            }))
    })
    @GetMapping("/exists/domain/{domain}")
    public ResponseEntity<Boolean> organizationExistsByDomain(
            @Parameter(description = "Domain to check", required = true, example = "acme.com") @PathVariable String domain) {
        boolean exists = organizationService.organizationExistsByDomain(domain);
        return ResponseEntity.ok(exists);
    }
}