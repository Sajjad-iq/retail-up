package com.sajjadkademm.retail.application.controllers.inventory;

import com.sajjadkademm.retail.domain.inventory.model.Inventory;
import com.sajjadkademm.retail.application.dto.inventory.CreateInventoryRequest;
import com.sajjadkademm.retail.application.dto.inventory.UpdateInventoryRequest;
import com.sajjadkademm.retail.application.services.inventory.InventoryService;

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
 * Inventory management controller providing CRUD operations for
 * inventories within organizations. All operations are scoped to the current
 * authenticated user.
 * 
 * @author Sajjad Kadem
 * @version 1.0
 * @since 2024-12-19
 */
@Slf4j
@RestController
@RequestMapping("/api/inventories")
@RequiredArgsConstructor
@Tag(name = "Inventories", description = "Inventory management endpoints (user-scoped)")
public class InventoryController {

    private final InventoryService inventoryService;

    /**
     * Create inventory endpoint
     * Creates a new inventory for an organization (only if the current user is the
     * organization creator)
     */
    @Operation(summary = "Create Inventory", description = "Create a new inventory for an organization (only if current user is the organization creator)", operationId = "createInventory")
    @ApiResponse(responseCode = "200", description = "Inventory created successfully", content = @Content(mediaType = "application/json", schema = @Schema(implementation = Inventory.class), examples = @ExampleObject(name = "Created Inventory", value = """
            {
                "id": "inv123",
                "name": "Main Warehouse",
                "description": "Primary storage facility for all products",
                "location": "123 Warehouse St, Industrial District",
                "isActive": true,
                "organizationId": "org123",
                "createdAt": "2024-12-19T10:30:00",
                "updatedAt": "2024-12-19T10:30:00",
                "createdBy": "user123"
            }
            """)))
    @PostMapping
    public ResponseEntity<Inventory> createInventory(
            @Parameter(description = "Inventory creation request", required = true, content = @Content(schema = @Schema(implementation = CreateInventoryRequest.class), examples = @ExampleObject(name = "Create Inventory Request", value = """
                    {
                        "organizationId": "org123",
                        "name": "Main Warehouse",
                        "description": "Primary storage facility for all products",
                        "location": "123 Warehouse St, Industrial District"
                    }
                    """))) @Valid @RequestBody CreateInventoryRequest request) throws Exception {
        
        Inventory response = inventoryService.createInventory(request);
        return ResponseEntity.ok(response);
    }

    /**
     * Update inventory endpoint
     * Updates an inventory (only if the current user is the organization creator)
     */
    @Operation(summary = "Update Inventory", description = "Update an existing inventory (only if current user is the organization creator)", operationId = "updateInventory")
    @ApiResponse(responseCode = "200", description = "Inventory updated successfully", content = @Content(mediaType = "application/json", schema = @Schema(implementation = Inventory.class), examples = @ExampleObject(name = "Updated Inventory", value = """
            {
                "id": "inv123",
                "name": "Main Warehouse Updated",
                "description": "Updated primary storage facility",
                "location": "456 Updated Warehouse Ave, Industrial District",
                "isActive": true,
                "organizationId": "org123",
                "createdAt": "2024-12-19T10:30:00",
                "updatedAt": "2024-12-19T11:30:00",
                "createdBy": "user123"
            }
            """)))
    @PutMapping("/{id}")
    public ResponseEntity<Inventory> updateInventory(
            @Parameter(description = "Inventory ID", required = true, example = "inv123") @PathVariable String id,
            @Parameter(description = "Inventory update request", required = true, content = @Content(schema = @Schema(implementation = UpdateInventoryRequest.class), examples = @ExampleObject(name = "Update Inventory Request", value = """
                    {
                        "name": "Main Warehouse Updated",
                        "description": "Updated primary storage facility",
                        "location": "456 Updated Warehouse Ave, Industrial District"
                    }
                    """))) @Valid @RequestBody UpdateInventoryRequest request) throws Exception {
        
        Inventory response = inventoryService.updateInventory(id, request);
        return ResponseEntity.ok(response);
    }

    /**
     * Get inventory by ID endpoint
     * Returns inventory details only if the current user is the organization
     * creator
     */
    @Operation(summary = "Get Inventory by ID", description = "Retrieve inventory details by ID (only if current user is the organization creator)", operationId = "getInventoryById")
    @ApiResponse(responseCode = "200", description = "Inventory found", content = @Content(mediaType = "application/json", schema = @Schema(implementation = Inventory.class), examples = @ExampleObject(name = "Inventory Details", value = """
            {
                "id": "inv123",
                "name": "Main Warehouse",
                "description": "Primary storage facility for all products",
                "location": "123 Warehouse St, Industrial District",
                "isActive": true,
                "organizationId": "org123",
                "createdAt": "2024-12-19T10:30:00",
                "updatedAt": "2024-12-19T10:30:00",
                "createdBy": "user123"
            }
            """)))
    @GetMapping("/{id}")
    public ResponseEntity<Inventory> getInventoryById(
            @Parameter(description = "Inventory ID", required = true, example = "inv123") @PathVariable String id) throws Exception {
        
        Inventory response = inventoryService.getInventoryById(id);
        return ResponseEntity.ok(response);
    }

    /**
     * Get all inventories for an organization endpoint
     * Returns inventories only if the current user is the organization creator
     */
    @Operation(summary = "Get Inventories by Organization", description = "Retrieve all inventories for a specific organization (only if current user is the organization creator)", operationId = "getInventoriesByOrganization")
    @ApiResponse(responseCode = "200", description = "List of inventories retrieved successfully", content = @Content(mediaType = "application/json", schema = @Schema(implementation = Inventory.class, type = "array"), examples = @ExampleObject(name = "Inventories List", value = """
            [
                {
                    "id": "inv123",
                    "name": "Main Warehouse",
                    "description": "Primary storage facility",
                    "location": "123 Warehouse St, Industrial District",
                    "isActive": true,
                    "organizationId": "org123",
                    "createdAt": "2024-12-19T10:30:00",
                    "updatedAt": "2024-12-19T10:30:00",
                    "createdBy": "user123"
                },
                {
                    "id": "inv456",
                    "name": "Secondary Storage",
                    "description": "Backup storage facility",
                    "location": "789 Backup Ave, Industrial District",
                    "isActive": true,
                    "organizationId": "org123",
                    "createdAt": "2024-12-19T11:30:00",
                    "updatedAt": "2024-12-19T11:30:00",
                    "createdBy": "user456"
                }
            ]
            """)))
    @GetMapping("/organization/{organizationId}")
    public ResponseEntity<List<Inventory>> getInventoriesByOrganization(
            @Parameter(description = "Organization ID", required = true, example = "org123") @PathVariable String organizationId) throws Exception {
        
        List<Inventory> response = inventoryService.getInventoriesByOrganization(organizationId);
        return ResponseEntity.ok(response);
    }

    /**
     * Get active inventories for an organization endpoint
     * Returns active inventories only if the current user is the organization
     * creator
     */
    @Operation(summary = "Get Active Inventories by Organization", description = "Retrieve all active inventories for a specific organization (only if current user is the organization creator)", operationId = "getActiveInventoriesByOrganization")
    @ApiResponse(responseCode = "200", description = "List of active inventories retrieved successfully", content = @Content(mediaType = "application/json", schema = @Schema(implementation = Inventory.class, type = "array")))
    @GetMapping("/organization/{organizationId}/active")
    public ResponseEntity<List<Inventory>> getActiveInventoriesByOrganization(
            @Parameter(description = "Organization ID", required = true, example = "org123") @PathVariable String organizationId) throws Exception {
        
        List<Inventory> response = inventoryService.getActiveInventoriesByOrganization(organizationId);
        return ResponseEntity.ok(response);
    }

    /**
     * Search inventories endpoint
     * Searches inventories only if the current user is the organization creator
     */
    @Operation(summary = "Search Inventories", description = "Search inventories by name within an organization (only if current user is the organization creator)", operationId = "searchInventories")
    @ApiResponse(responseCode = "200", description = "Search results retrieved successfully", content = @Content(mediaType = "application/json", schema = @Schema(implementation = Inventory.class, type = "array"), examples = @ExampleObject(name = "Search Results", value = """
            [
                {
                    "id": "inv123",
                    "name": "Main Warehouse",
                    "description": "Primary storage facility",
                    "location": "123 Warehouse St, Industrial District",
                    "isActive": true,
                    "organizationId": "org123",
                    "createdAt": "2024-12-19T10:30:00",
                    "updatedAt": "2024-12-19T10:30:00",
                    "createdBy": "user123"
                }
            ]
            """)))
    @GetMapping("/organization/{organizationId}/search")
    public ResponseEntity<List<Inventory>> searchInventories(
            @Parameter(description = "Organization ID", required = true, example = "org123") @PathVariable String organizationId,
            @Parameter(description = "Search query for inventory name", required = true, example = "warehouse") @RequestParam String q) throws Exception {
        
        List<Inventory> response = inventoryService.searchInventories(organizationId, q);
        return ResponseEntity.ok(response);
    }

    /**
     * Delete inventory endpoint
     * Deletes an inventory only if the current user is the organization creator
     */
    @Operation(summary = "Delete Inventory", description = "Soft delete an inventory by setting isActive to false (only if current user is the organization creator)", operationId = "deleteInventory")
    @ApiResponse(responseCode = "200", description = "Inventory deleted successfully")
    @DeleteMapping("/{id}")
    public ResponseEntity<Void> deleteInventory(
            @Parameter(description = "Inventory ID", required = true, example = "inv123") @PathVariable String id) throws Exception {
        
        inventoryService.deleteInventory(id);
        return ResponseEntity.ok().build();
    }

    /**
     * Check inventory exists by name endpoint
     */
    @Operation(summary = "Check Inventory Exists by Name", description = "Check if an inventory exists with the specified name within an organization", operationId = "inventoryExistsByName")
    @ApiResponse(responseCode = "200", description = "Inventory existence check result", content = @Content(mediaType = "application/json", schema = @Schema(type = "boolean"), examples = {
            @ExampleObject(name = "Inventory Exists", value = "true"),
            @ExampleObject(name = "Inventory Not Found", value = "false")
    }))
    @GetMapping("/exists/name/{name}/organization/{organizationId}")
    public ResponseEntity<Boolean> inventoryExistsByName(
            @Parameter(description = "Inventory name to check", required = true, example = "Main Warehouse") @PathVariable String name,
            @Parameter(description = "Organization ID", required = true, example = "org123") @PathVariable String organizationId) throws Exception {
        
        Boolean exists = inventoryService.inventoryExistsByName(name, organizationId);
        return ResponseEntity.ok(exists);
    }

    /**
     * Get inventory count for organization endpoint
     */
    @Operation(summary = "Get Inventory Count by Organization", description = "Get the total number of inventories for an organization", operationId = "getInventoryCountByOrganization")
    @ApiResponse(responseCode = "200", description = "Inventory count retrieved successfully", content = @Content(mediaType = "application/json", schema = @Schema(type = "integer"), examples = @ExampleObject(name = "Inventory Count", value = "5")))
    @GetMapping("/organization/{organizationId}/count")
    public ResponseEntity<Long> getInventoryCountByOrganization(
            @Parameter(description = "Organization ID", required = true, example = "org123") @PathVariable String organizationId) throws Exception {
        
        Long count = inventoryService.getInventoryCountByOrganization(organizationId);
        return ResponseEntity.ok(count);
    }
}