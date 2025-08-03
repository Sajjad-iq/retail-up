package com.sajjadkademm.retail.settings.inventory.controller;

import com.sajjadkademm.retail.auth.JwtUtil;
import com.sajjadkademm.retail.settings.inventory.dto.InventorySettingsRequest;
import com.sajjadkademm.retail.settings.inventory.dto.InventorySettingsResponse;
import com.sajjadkademm.retail.settings.inventory.service.InventorySettingsService;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.validation.Valid;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.ExampleObject;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.tags.Tag;
import io.swagger.v3.oas.annotations.security.SecurityRequirement;

/**
 * Inventory settings controller for managing inventory system configurations.
 * 
 * @author Sajjad Kadem
 * @version 1.0
 * @since 2024-12-19
 */
@Slf4j
@RestController
@RequestMapping("/api/settings/inventory")
@RequiredArgsConstructor
@Tag(name = "Inventory Settings", description = "Inventory settings management endpoints")
public class InventorySettingsController {

    private final InventorySettingsService inventorySettingsService;
    private final JwtUtil jwtUtil;

    /**
     * Get inventory settings for an organization
     */
    @Operation(summary = "Get Inventory Settings", description = "Retrieve inventory settings for a specific organization", operationId = "getInventorySettings")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Inventory settings retrieved successfully", content = @Content(mediaType = "application/json", schema = @Schema(implementation = InventorySettingsResponse.class), examples = @ExampleObject(name = "Inventory Settings", value = """
                    {
                        "id": "inv123",
                        "organizationId": "org123",
                        "negativeStockAllowed": false,
                        "barcodeRequired": true,
                        "skuRequired": true,
                        "requireCostPrice": true,
                        "lowStockAlertsEnabled": true,
                        "lowStockThreshold": 10,
                        "outOfStockAlertsEnabled": true,
                        "expiryAlertsEnabled": true,
                        "expiryAlertDays": 30,
                        "batchTrackingEnabled": true,
                        "expiryDateTrackingEnabled": true,
                        "updatedBy": "user123",
                        "createdAt": "2024-12-19T10:30:00",
                        "updatedAt": "2024-12-19T10:30:00"
                    }
                    """))),
            @ApiResponse(responseCode = "404", description = "Inventory settings not found for organization", content = @Content(mediaType = "application/json", schema = @Schema(implementation = Object.class)))
    })
    @GetMapping("/{organizationId}")
    public ResponseEntity<InventorySettingsResponse> getInventorySettings(
            @Parameter(description = "Organization ID", required = true, example = "org123") @PathVariable String organizationId) {
        InventorySettingsResponse response = inventorySettingsService.getInventorySettings(organizationId);
        return ResponseEntity.ok(response);
    }

    /**
     * Update inventory settings
     */
    @Operation(summary = "Update Inventory Settings", description = "Update inventory settings for a specific organization", operationId = "updateInventorySettings", security = @SecurityRequirement(name = "Bearer Authentication"))
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Inventory settings updated successfully", content = @Content(mediaType = "application/json", schema = @Schema(implementation = InventorySettingsResponse.class), examples = @ExampleObject(name = "Updated Inventory Settings", value = """
                    {
                        "id": "inv123",
                        "organizationId": "org123",
                        "negativeStockAllowed": true,
                        "barcodeRequired": false,
                        "skuRequired": true,
                        "requireCostPrice": false,
                        "lowStockAlertsEnabled": true,
                        "lowStockThreshold": 5,
                        "outOfStockAlertsEnabled": false,
                        "expiryAlertsEnabled": true,
                        "expiryAlertDays": 14,
                        "batchTrackingEnabled": false,
                        "expiryDateTrackingEnabled": true,
                        "updatedBy": "user123",
                        "createdAt": "2024-12-19T10:30:00",
                        "updatedAt": "2024-12-19T11:30:00"
                    }
                    """))),
            @ApiResponse(responseCode = "400", description = "Bad request - validation errors", content = @Content(mediaType = "application/json", schema = @Schema(implementation = Object.class))),
            @ApiResponse(responseCode = "404", description = "Organization not found", content = @Content(mediaType = "application/json", schema = @Schema(implementation = Object.class))),
            @ApiResponse(responseCode = "401", description = "Unauthorized - invalid or missing token", content = @Content(mediaType = "application/json", schema = @Schema(implementation = Object.class)))
    })
    @PutMapping("/{organizationId}")
    public ResponseEntity<InventorySettingsResponse> updateInventorySettings(
            @Parameter(description = "Organization ID", required = true, example = "org123") @PathVariable String organizationId,
            @Parameter(description = "Inventory settings update request", required = true, content = @Content(schema = @Schema(implementation = InventorySettingsRequest.class), examples = @ExampleObject(name = "Update Inventory Settings Request", value = """
                    {
                        "negativeStockAllowed": true,
                        "barcodeRequired": false,
                        "skuRequired": true,
                        "requireCostPrice": false,
                        "lowStockAlertsEnabled": true,
                        "lowStockThreshold": 5,
                        "outOfStockAlertsEnabled": false,
                        "expiryAlertsEnabled": true,
                        "expiryAlertDays": 14,
                        "batchTrackingEnabled": false,
                        "expiryDateTrackingEnabled": true,
                        "updatedBy": "user123"
                    }
                    """))) @Valid @RequestBody InventorySettingsRequest request,
            HttpServletRequest httpRequest) {

        // Extract user ID from JWT token
        String authHeader = httpRequest.getHeader("Authorization");
        String userId = null;

        if (authHeader != null && authHeader.startsWith("Bearer ")) {
            String token = authHeader.substring(7);
            userId = jwtUtil.extractUserId(token);
        }

        InventorySettingsResponse response = inventorySettingsService.updateInventorySettings(organizationId, request,
                userId);
        return ResponseEntity.ok(response);
    }

    /**
     * Reset inventory settings to defaults
     */
    @Operation(summary = "Reset Inventory Settings to Defaults", description = "Reset inventory settings to default values for a specific organization", operationId = "resetInventorySettingsToDefaults", security = @SecurityRequirement(name = "Bearer Authentication"))
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Inventory settings reset to defaults successfully", content = @Content(mediaType = "application/json", schema = @Schema(implementation = InventorySettingsResponse.class), examples = @ExampleObject(name = "Reset Inventory Settings", value = """
                    {
                        "id": "inv123",
                        "organizationId": "org123",
                        "negativeStockAllowed": false,
                        "barcodeRequired": true,
                        "skuRequired": true,
                        "requireCostPrice": true,
                        "lowStockAlertsEnabled": true,
                        "lowStockThreshold": 10,
                        "outOfStockAlertsEnabled": true,
                        "expiryAlertsEnabled": true,
                        "expiryAlertDays": 30,
                        "batchTrackingEnabled": true,
                        "expiryDateTrackingEnabled": true,
                        "updatedBy": "user123",
                        "createdAt": "2024-12-19T10:30:00",
                        "updatedAt": "2024-12-19T12:30:00"
                    }
                    """))),
            @ApiResponse(responseCode = "404", description = "Organization not found", content = @Content(mediaType = "application/json", schema = @Schema(implementation = Object.class))),
            @ApiResponse(responseCode = "401", description = "Unauthorized - invalid or missing token", content = @Content(mediaType = "application/json", schema = @Schema(implementation = Object.class)))
    })
    @PostMapping("/{organizationId}/reset")
    public ResponseEntity<InventorySettingsResponse> resetInventorySettingsToDefaults(
            @Parameter(description = "Organization ID", required = true, example = "org123") @PathVariable String organizationId) {
        InventorySettingsResponse response = inventorySettingsService.resetToDefaults(organizationId);
        return ResponseEntity.ok(response);
    }
}