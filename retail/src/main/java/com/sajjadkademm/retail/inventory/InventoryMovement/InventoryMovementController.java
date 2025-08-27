package com.sajjadkademm.retail.inventory.InventoryMovement;

import com.sajjadkademm.retail.inventory.InventoryMovement.enums.MovementType;
import com.sajjadkademm.retail.inventory.InventoryMovement.enums.ReferenceType;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.Page;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import java.time.LocalDateTime;
import java.util.List;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.tags.Tag;

/**
 * Inventory movement tracking controller for recording and querying
 * inventory stock changes and movements.
 * 
 * @author Sajjad Kadem
 * @version 1.0
 * @since 2024-12-19
 */
@Slf4j
@RestController
@RequestMapping("/api/inventory-movements")
@RequiredArgsConstructor
@Tag(name = "Inventory Movements", description = "Inventory movement tracking endpoints")
public class InventoryMovementController {

    private final InventoryMovementService movementService;

    // Movement creation is internal-only; no public POST endpoint

    /**
     * Get movement by ID endpoint
     */
    @Operation(summary = "Get Movement by ID", description = "Retrieve movement details by its unique identifier", operationId = "getMovementById")
    @ApiResponse(responseCode = "200", description = "Movement found", content = @Content(mediaType = "application/json", schema = @Schema(implementation = InventoryMovement.class)))
    @GetMapping("/{id}")
    public ResponseEntity<InventoryMovement> getMovementById(
            @Parameter(description = "Movement ID", required = true, example = "mov123") @PathVariable String id) {
        InventoryMovement response = movementService.getMovementById(id);
        return ResponseEntity.ok(response);
    }

    /**
     * Get movements by inventory item endpoint
     */
    @Operation(summary = "Get Movements by Item", description = "Retrieve all movements for a specific inventory item", operationId = "getMovementsByItem")
    @ApiResponse(responseCode = "200", description = "Movements retrieved successfully")
    @GetMapping("/item/{inventoryItemId}")
    public ResponseEntity<?> getMovementsByItem(
            @Parameter(description = "Inventory item ID", required = true, example = "item123") @PathVariable String inventoryItemId,
            @Parameter(description = "Page number (0-based)", example = "0") @RequestParam(required = false) Integer page,
            @Parameter(description = "Page size", example = "20") @RequestParam(required = false) Integer size) {

        if (page != null && size != null) {
            Page<InventoryMovement> response = movementService.getMovementsByItem(inventoryItemId, page, size);
            return ResponseEntity.ok(response);
        } else {
            List<InventoryMovement> response = movementService.getMovementsByItem(inventoryItemId);
            return ResponseEntity.ok(response);
        }
    }

    /**
     * Get movements by inventory endpoint
     */
    @Operation(summary = "Get Movements by Inventory", description = "Retrieve all movements for items in a specific inventory", operationId = "getMovementsByInventory")
    @ApiResponse(responseCode = "200", description = "Movements retrieved successfully")
    @GetMapping("/inventory/{inventoryId}")
    public ResponseEntity<?> getMovementsByInventory(
            @Parameter(description = "Inventory ID", required = true, example = "inv123") @PathVariable String inventoryId,
            @Parameter(description = "Page number (0-based)", example = "0") @RequestParam(required = false) Integer page,
            @Parameter(description = "Page size", example = "20") @RequestParam(required = false) Integer size) {

        if (page != null && size != null) {
            Page<InventoryMovement> response = movementService.getMovementsByInventory(inventoryId, page, size);
            return ResponseEntity.ok(response);
        } else {
            List<InventoryMovement> response = movementService.getMovementsByInventory(inventoryId);
            return ResponseEntity.ok(response);
        }
    }

    /**
     * Get movements by type endpoint
     */
    @Operation(summary = "Get Movements by Type", description = "Retrieve all movements of a specific type", operationId = "getMovementsByType")
    @ApiResponse(responseCode = "200", description = "Movements retrieved successfully")
    @GetMapping("/type/{movementType}")
    public ResponseEntity<List<InventoryMovement>> getMovementsByType(
            @Parameter(description = "Movement type", required = true, example = "SALE") @PathVariable MovementType movementType) {
        List<InventoryMovement> response = movementService.getMovementsByType(movementType);
        return ResponseEntity.ok(response);
    }

    /**
     * Get movements by user endpoint
     */
    @Operation(summary = "Get Movements by User", description = "Retrieve all movements created by a specific user", operationId = "getMovementsByUser")
    @ApiResponse(responseCode = "200", description = "Movements retrieved successfully")
    @GetMapping("/user/{userId}")
    public ResponseEntity<List<InventoryMovement>> getMovementsByUser(
            @Parameter(description = "User ID", required = true, example = "user123") @PathVariable String userId) {
        List<InventoryMovement> response = movementService.getMovementsByUser(userId);
        return ResponseEntity.ok(response);
    }

    /**
     * Get movements by date range endpoint
     */
    @Operation(summary = "Get Movements by Date Range", description = "Retrieve movements within a specific date range", operationId = "getMovementsByDateRange")
    @ApiResponse(responseCode = "200", description = "Movements retrieved successfully")
    @GetMapping("/date-range")
    public ResponseEntity<List<InventoryMovement>> getMovementsByDateRange(
            @Parameter(description = "Start date", required = true, example = "2024-01-01T00:00:00") @RequestParam @DateTimeFormat(iso = DateTimeFormat.ISO.DATE_TIME) LocalDateTime startDate,
            @Parameter(description = "End date", required = true, example = "2024-12-31T23:59:59") @RequestParam @DateTimeFormat(iso = DateTimeFormat.ISO.DATE_TIME) LocalDateTime endDate) {
        List<InventoryMovement> response = movementService.getMovementsByDateRange(startDate, endDate);
        return ResponseEntity.ok(response);
    }

    /**
     * Get movements by reference endpoint
     */
    @Operation(summary = "Get Movements by Reference", description = "Retrieve movements by reference type and ID", operationId = "getMovementsByReference")
    @ApiResponse(responseCode = "200", description = "Movements retrieved successfully")
    @GetMapping("/reference")
    public ResponseEntity<List<InventoryMovement>> getMovementsByReference(
            @Parameter(description = "Reference type", required = true, example = "SALE") @RequestParam ReferenceType referenceType,
            @Parameter(description = "Reference ID", required = true, example = "sale123") @RequestParam String referenceId) {
        List<InventoryMovement> response = movementService.getMovementsByReference(referenceType.name(), referenceId);
        return ResponseEntity.ok(response);
    }
}