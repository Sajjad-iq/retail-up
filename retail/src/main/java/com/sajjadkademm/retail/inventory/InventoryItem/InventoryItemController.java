package com.sajjadkademm.retail.inventory.InventoryItem;

import com.sajjadkademm.retail.inventory.InventoryItem.dto.*;
import com.sajjadkademm.retail.config.locales.LocalizedErrorService;

import com.sajjadkademm.retail.shared.enums.Unit;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import jakarta.validation.Valid;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.ExampleObject;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.tags.Tag;

/**
 * Inventory item management controller providing CRUD operations for
 * inventory items within inventories. All operations are scoped to the current
 * authenticated user.
 * 
 * @author Sajjad Kadem
 * @version 1.0
 * @since 2024-12-19
 */
@Slf4j
@RestController
@RequestMapping("/api/inventory-items")
@RequiredArgsConstructor
@Tag(name = "Inventory Items", description = "Inventory item management endpoints (user-scoped)")
public class InventoryItemController {

        private final InventoryItemService inventoryItemService;
        private final LocalizedErrorService localizedErrorService;

        /**
         * Create inventory item endpoint
         * Creates a new inventory item in an inventory (only if the current user is the
         * organization creator)
         */
        @Operation(summary = "Create Inventory Item", description = "Create a new inventory item in an inventory (only if current user is the organization creator)", operationId = "createInventoryItem")
        @ApiResponse(responseCode = "200", description = "Inventory item created successfully", content = @Content(mediaType = "application/json", schema = @Schema(implementation = InventoryItem.class), examples = @ExampleObject(name = "Created Inventory Item", value = "{\"id\": \"item123\", \"name\": \"Laptop Computer\", \"description\": \"High-performance laptop for business use\", \"barcode\": \"1234567890123\", \"category\": \"Electronics\", \"brand\": \"TechCorp\", \"unit\": \"PIECES\", \"currentStock\": 50, \"minimumStock\": 10, \"maximumStock\": 100, \"costPrice\": 800.00, \"sellingPrice\": 1200.00, \"isActive\": true, \"inventoryId\": \"inv123\", \"createdAt\": \"2024-12-19T10:30:00\", \"updatedAt\": \"2024-12-19T10:30:00\", \"createdBy\": \"user123\"}")))
        @PostMapping
        public ResponseEntity<InventoryItem> createInventoryItem(
                        @Parameter(description = "Inventory item creation request", required = true, content = @Content(schema = @Schema(implementation = CreateInventoryItemRequest.class), examples = @ExampleObject(name = "Create Inventory Item Request", value = "{\"inventoryId\": \"inv123\", \"name\": \"Laptop Computer\", \"description\": \"High-performance laptop for business use\", \"barcode\": \"1234567890123\", \"category\": \"Electronics\", \"brand\": \"TechCorp\", \"unit\": \"PIECES\", \"currentStock\": 50, \"minimumStock\": 10, \"maximumStock\": 100, \"costPrice\": 800.00, \"sellingPrice\": 1200.00}"))) @Valid @RequestBody CreateInventoryItemRequest request) {
                InventoryItem response = inventoryItemService.createInventoryItem(request);
                return ResponseEntity.ok(response);
        }

        /**
         * Update inventory item endpoint
         * Updates an inventory item (only if the current user is the organization
         * creator)
         */
        @Operation(summary = "Update Inventory Item", description = "Update an existing inventory item (only if current user is the organization creator)", operationId = "updateInventoryItem")
        @ApiResponse(responseCode = "200", description = "Inventory item updated successfully", content = @Content(mediaType = "application/json", schema = @Schema(implementation = InventoryItem.class)))
        @PutMapping("/{id}")
        public ResponseEntity<InventoryItem> updateInventoryItem(
                        @Parameter(description = "Inventory item ID", required = true, example = "item123") @PathVariable String id,
                        @Parameter(description = "Inventory item update request", required = true, content = @Content(schema = @Schema(implementation = UpdateInventoryItemRequest.class))) @Valid @RequestBody UpdateInventoryItemRequest request) {
                InventoryItem response = inventoryItemService.updateInventoryItem(id, request);
                return ResponseEntity.ok(response);
        }

        /**
         * Get inventory item by ID endpoint
         */
        @Operation(summary = "Get Inventory Item by ID", description = "Retrieve inventory item details by its unique identifier", operationId = "getInventoryItemById")
        @ApiResponse(responseCode = "200", description = "Inventory item found", content = @Content(mediaType = "application/json", schema = @Schema(implementation = InventoryItem.class)))
        @GetMapping("/{id}")
        public ResponseEntity<InventoryItem> getInventoryItemById(
                        @Parameter(description = "Inventory item ID", required = true, example = "item123") @PathVariable String id) {
                InventoryItem response = inventoryItemService.getInventoryItemById(id);
                return ResponseEntity.ok(response);
        }

        /**
         * Delete inventory item endpoint
         */
        @Operation(summary = "Delete Inventory Item", description = "Hard delete an inventory item from the database. Related movements are automatically deleted via database cascade.", operationId = "deleteInventoryItem")
        @ApiResponse(responseCode = "200", description = "Inventory item deleted successfully")
        @DeleteMapping("/{id}")
        public ResponseEntity<Void> deleteInventoryItem(
                        @Parameter(description = "Inventory item ID", required = true, example = "item123") @PathVariable String id) {
                inventoryItemService.deleteInventoryItem(id);
                return ResponseEntity.ok().build();
        }

        /**
         * Get inventory item by barcode endpoint
         */
        @Operation(summary = "Get Inventory Item by Barcode", description = "Retrieve inventory item details by barcode within an inventory", operationId = "getInventoryItemByBarcode")
        @ApiResponse(responseCode = "200", description = "Inventory item found", content = @Content(mediaType = "application/json", schema = @Schema(implementation = InventoryItem.class)))
        @GetMapping("/barcode/{barcode}/inventory/{inventoryId}")
        public ResponseEntity<InventoryItem> getInventoryItemByBarcode(
                        @Parameter(description = "Barcode", required = true, example = "1234567890123") @PathVariable String barcode,
                        @Parameter(description = "Inventory ID", required = true, example = "inv123") @PathVariable String inventoryId) {
                InventoryItem response = inventoryItemService.getInventoryItemByBarcode(barcode, inventoryId);
                return ResponseEntity.ok(response);
        }

        /**
         * Unified endpoint for getting inventory items with filtering and pagination
         */
        @Operation(summary = "Get Inventory Items", description = "Retrieve inventory items with comprehensive filtering, searching, and pagination support", operationId = "getInventoryItems")
        @ApiResponse(responseCode = "200", description = "Inventory items retrieved successfully", content = @Content(mediaType = "application/json", schema = @Schema(implementation = PagedResponse.class)))
        @GetMapping("/inventory/{inventoryId}")
        public ResponseEntity<PagedResponse<InventoryItem>> getInventoryItems(
                        @Parameter(description = "Inventory ID", required = true, example = "inv123") @PathVariable String inventoryId,

                        // Pagination parameters
                        @Parameter(description = "Page number (0-based)", example = "0") @RequestParam(defaultValue = "0") int page,
                        @Parameter(description = "Page size (max 100)", example = "20") @RequestParam(defaultValue = "20") int size,
                        @Parameter(description = "Sort field", example = "createdAt") @RequestParam(defaultValue = "createdAt") String sortBy,
                        @Parameter(description = "Sort direction (asc/desc)", example = "desc") @RequestParam(defaultValue = "desc") String sortDirection,

                        // Search parameter
                        @Parameter(description = "Search query for item name, barcode, or product code", example = "laptop") @RequestParam(required = false) String search,

                        // Basic filters
                        @Parameter(description = "Category filter", example = "Electronics") @RequestParam(required = false) String category,
                        @Parameter(description = "Brand filter", example = "Samsung") @RequestParam(required = false) String brand,
                        @Parameter(description = "Supplier filter", example = "TechSupplier") @RequestParam(required = false) String supplier,
                        @Parameter(description = "Color filter", example = "Black") @RequestParam(required = false) String color,
                        @Parameter(description = "Size filter", example = "Large") @RequestParam(required = false) String itemSize,

                        // Status filters
                        @Parameter(description = "Active items only", example = "true") @RequestParam(required = false) Boolean activeOnly,
                        @Parameter(description = "Perishable items filter", example = "true") @RequestParam(required = false) Boolean perishable,
                        @Parameter(description = "Low stock items only", example = "true") @RequestParam(required = false) Boolean lowStock,
                        @Parameter(description = "Out of stock items only", example = "true") @RequestParam(required = false) Boolean outOfStock,
                        @Parameter(description = "Items with active discounts only", example = "true") @RequestParam(required = false) Boolean hasDiscount,

                        // Stock range filters
                        @Parameter(description = "Minimum stock quantity", example = "10") @RequestParam(required = false) Integer minStock,
                        @Parameter(description = "Maximum stock quantity", example = "100") @RequestParam(required = false) Integer maxStock,

                        // Price range filters
                        @Parameter(description = "Minimum cost price", example = "10.00") @RequestParam(required = false) java.math.BigDecimal minCostPrice,
                        @Parameter(description = "Maximum cost price", example = "500.00") @RequestParam(required = false) java.math.BigDecimal maxCostPrice,
                        @Parameter(description = "Minimum selling price", example = "15.00") @RequestParam(required = false) java.math.BigDecimal minSellingPrice,
                        @Parameter(description = "Maximum selling price", example = "750.00") @RequestParam(required = false) java.math.BigDecimal maxSellingPrice,

                        // Date filters
                        @Parameter(description = "Created after date", example = "2024-01-01") @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) java.time.LocalDate createdAfter,
                        @Parameter(description = "Created before date", example = "2024-12-31") @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) java.time.LocalDate createdBefore,
                        @Parameter(description = "Expiry after date", example = "2024-06-01") @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) java.time.LocalDate expiryAfter,
                        @Parameter(description = "Expiry before date", example = "2024-12-31") @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) java.time.LocalDate expiryBefore,

                        // Expiry status filters
                        @Parameter(description = "Expiry status (expiring, expired, fresh)", example = "expiring") @RequestParam(required = false) String expiryStatus,
                        @Parameter(description = "Days for expiry calculation", example = "7") @RequestParam(defaultValue = "7") int expiryDays) {

                // Create filter request from parameters
                FilterRequest filterRequest = new FilterRequest();
                filterRequest.setCategory(category);
                filterRequest.setBrand(brand);
                filterRequest.setSupplierName(supplier);
                filterRequest.setColor(color);
                filterRequest.setSize(itemSize);
                filterRequest.setSearchTerm(search);
                filterRequest.setIsActive(activeOnly);
                filterRequest.setIsPerishable(perishable);
                filterRequest.setLowStock(lowStock);
                filterRequest.setOutOfStock(outOfStock);
                filterRequest.setHasDiscount(hasDiscount);
                filterRequest.setMinStock(minStock);
                filterRequest.setMaxStock(maxStock);
                filterRequest.setMinCostPrice(minCostPrice);
                filterRequest.setMaxCostPrice(maxCostPrice);
                filterRequest.setMinSellingPrice(minSellingPrice);
                filterRequest.setMaxSellingPrice(maxSellingPrice);
                filterRequest.setCreatedAfter(createdAfter);
                filterRequest.setCreatedBefore(createdBefore);
                filterRequest.setExpiryAfter(expiryAfter);
                filterRequest.setExpiryBefore(expiryBefore);
                filterRequest.setExpiryStatus(expiryStatus);
                filterRequest.setExpiryDays(expiryDays);
                filterRequest.setSortBy(sortBy);
                filterRequest.setSortDirection(sortDirection);

                PagedResponse<InventoryItem> response = inventoryItemService.filterItemsPaginated(inventoryId,
                                filterRequest, page, size, sortBy, sortDirection);
                return ResponseEntity.ok(response);
        }

        /**
         * Get item count in an inventory endpoint
         */
        @Operation(summary = "Get Item Count", description = "Get the total count of items in an inventory", operationId = "getItemCountByInventory")
        @ApiResponse(responseCode = "200", description = "Item count retrieved successfully", content = @Content(mediaType = "application/json", schema = @Schema(type = "integer", format = "int64", example = "150")))
        @GetMapping("/inventory/{inventoryId}/count")
        public ResponseEntity<Long> getItemCountByInventory(
                        @Parameter(description = "Inventory ID", required = true, example = "inv123") @PathVariable String inventoryId) {
                long count = inventoryItemService.getItemCountByInventory(inventoryId);
                return ResponseEntity.ok(count);
        }

        /**
         * Get active item count in an inventory endpoint
         */
        @Operation(summary = "Get Active Item Count", description = "Get the count of active items in an inventory", operationId = "getActiveItemCountByInventory")
        @ApiResponse(responseCode = "200", description = "Active item count retrieved successfully", content = @Content(mediaType = "application/json", schema = @Schema(type = "integer", format = "int64", example = "120")))
        @GetMapping("/inventory/{inventoryId}/count/active")
        public ResponseEntity<Long> getActiveItemCountByInventory(
                        @Parameter(description = "Inventory ID", required = true, example = "inv123") @PathVariable String inventoryId) {
                long count = inventoryItemService.getActiveItemCountByInventory(inventoryId);
                return ResponseEntity.ok(count);
        }

        /**
         * Get all available units endpoint
         */
        @Operation(summary = "Get available units", description = "Get all available units for inventory items")
        @ApiResponse(responseCode = "200", description = "Units retrieved successfully")
        @GetMapping("/units")
        public ResponseEntity<Unit[]> getAvailableUnits() {
                return ResponseEntity.ok(Unit.values());
        }

}