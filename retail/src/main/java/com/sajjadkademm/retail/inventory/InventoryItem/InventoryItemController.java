package com.sajjadkademm.retail.inventory.InventoryItem;

import com.sajjadkademm.retail.inventory.InventoryItem.dto.CreateInventoryItemRequest;
import com.sajjadkademm.retail.inventory.InventoryItem.dto.UpdateInventoryItemRequest;

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
 * Inventory item management controller providing CRUD operations for
 * inventory items within inventories.
 * 
 * @author Sajjad Kadem
 * @version 1.0
 * @since 2024-12-19
 */
@Slf4j
@RestController
@RequestMapping("/api/inventory-items")
@RequiredArgsConstructor
@Tag(name = "Inventory Items", description = "Inventory item management endpoints")
public class InventoryItemController {

        private final InventoryItemService inventoryItemService;

        /**
         * Create inventory item endpoint
         */
        @Operation(summary = "Create Inventory Item", description = "Create a new inventory item in an inventory", operationId = "createInventoryItem")
        @ApiResponses(value = {
                        @ApiResponse(responseCode = "200", description = "Inventory item created successfully", content = @Content(mediaType = "application/json", schema = @Schema(implementation = InventoryItem.class), examples = @ExampleObject(name = "Created Inventory Item", value = """
                                        {
                                            "id": "item123",
                                            "name": "Laptop Computer",
                                            "description": "High-performance laptop for business use",
                                            "sku": "LAPTOP001",
                                            "barcode": "1234567890123",
                                            "category": "Electronics",
                                            "brand": "TechCorp",
                                            "unit": "piece",
                                            "currentStock": 50,
                                            "minimumStock": 10,
                                            "maximumStock": 100,
                                            "costPrice": 800.00,
                                            "sellingPrice": 1200.00,
                                            "isActive": true,
                                            "inventoryId": "inv123",
                                            "createdAt": "2024-12-19T10:30:00",
                                            "updatedAt": "2024-12-19T10:30:00",
                                            "createdBy": "user123"
                                        }
                                        """))),
                        @ApiResponse(responseCode = "400", description = "Bad request - validation errors", content = @Content(mediaType = "application/json", schema = @Schema(implementation = Object.class))),
                        @ApiResponse(responseCode = "409", description = "Conflict - item with same SKU or barcode already exists in inventory", content = @Content(mediaType = "application/json", schema = @Schema(implementation = Object.class)))
        })
        @PostMapping
        public ResponseEntity<InventoryItem> createInventoryItem(
                        @Parameter(description = "Inventory item creation request", required = true, content = @Content(schema = @Schema(implementation = CreateInventoryItemRequest.class), examples = @ExampleObject(name = "Create Inventory Item Request", value = """
                                        {
                                            "userId": "user123",
                                            "inventoryId": "inv123",
                                            "name": "Laptop Computer",
                                            "description": "High-performance laptop for business use",
                                            "sku": "LAPTOP001",
                                            "barcode": "1234567890123",
                                            "category": "Electronics",
                                            "brand": "TechCorp",
                                            "unit": "piece",
                                            "currentStock": 50,
                                            "minimumStock": 10,
                                            "maximumStock": 100,
                                            "costPrice": 800.00,
                                            "sellingPrice": 1200.00
                                        }
                                        """))) @Valid @RequestBody CreateInventoryItemRequest request) {
                InventoryItem response = inventoryItemService.createInventoryItem(request);
                return ResponseEntity.ok(response);
        }

        /**
         * Update inventory item endpoint
         */
        @Operation(summary = "Update Inventory Item", description = "Update an existing inventory item's information", operationId = "updateInventoryItem")
        @ApiResponses(value = {
                        @ApiResponse(responseCode = "200", description = "Inventory item updated successfully", content = @Content(mediaType = "application/json", schema = @Schema(implementation = InventoryItem.class))),
                        @ApiResponse(responseCode = "404", description = "Inventory item not found", content = @Content(mediaType = "application/json", schema = @Schema(implementation = Object.class))),
                        @ApiResponse(responseCode = "400", description = "Bad request - validation errors", content = @Content(mediaType = "application/json", schema = @Schema(implementation = Object.class)))
        })
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
        @ApiResponses(value = {
                        @ApiResponse(responseCode = "200", description = "Inventory item found", content = @Content(mediaType = "application/json", schema = @Schema(implementation = InventoryItem.class))),
                        @ApiResponse(responseCode = "404", description = "Inventory item not found", content = @Content(mediaType = "application/json", schema = @Schema(implementation = Object.class)))
        })
        @GetMapping("/{id}")
        public ResponseEntity<InventoryItem> getInventoryItemById(
                        @Parameter(description = "Inventory item ID", required = true, example = "item123") @PathVariable String id) {
                InventoryItem response = inventoryItemService.getInventoryItemById(id);
                return ResponseEntity.ok(response);
        }

        /**
         * Get inventory item by SKU endpoint
         */
        @Operation(summary = "Get Inventory Item by SKU", description = "Retrieve inventory item details by SKU within an inventory", operationId = "getInventoryItemBySku")
        @ApiResponses(value = {
                        @ApiResponse(responseCode = "200", description = "Inventory item found", content = @Content(mediaType = "application/json", schema = @Schema(implementation = InventoryItem.class))),
                        @ApiResponse(responseCode = "404", description = "Inventory item not found", content = @Content(mediaType = "application/json", schema = @Schema(implementation = Object.class)))
        })
        @GetMapping("/sku/{sku}/inventory/{inventoryId}")
        public ResponseEntity<InventoryItem> getInventoryItemBySku(
                        @Parameter(description = "SKU", required = true, example = "LAPTOP001") @PathVariable String sku,
                        @Parameter(description = "Inventory ID", required = true, example = "inv123") @PathVariable String inventoryId) {
                InventoryItem response = inventoryItemService.getInventoryItemBySku(sku, inventoryId);
                return ResponseEntity.ok(response);
        }

        /**
         * Get inventory item by barcode endpoint
         */
        @Operation(summary = "Get Inventory Item by Barcode", description = "Retrieve inventory item details by barcode within an inventory", operationId = "getInventoryItemByBarcode")
        @ApiResponses(value = {
                        @ApiResponse(responseCode = "200", description = "Inventory item found", content = @Content(mediaType = "application/json", schema = @Schema(implementation = InventoryItem.class))),
                        @ApiResponse(responseCode = "404", description = "Inventory item not found", content = @Content(mediaType = "application/json", schema = @Schema(implementation = Object.class)))
        })
        @GetMapping("/barcode/{barcode}/inventory/{inventoryId}")
        public ResponseEntity<InventoryItem> getInventoryItemByBarcode(
                        @Parameter(description = "Barcode", required = true, example = "1234567890123") @PathVariable String barcode,
                        @Parameter(description = "Inventory ID", required = true, example = "inv123") @PathVariable String inventoryId) {
                InventoryItem response = inventoryItemService.getInventoryItemByBarcode(barcode, inventoryId);
                return ResponseEntity.ok(response);
        }

        /**
         * Get all items in an inventory endpoint
         */
        @Operation(summary = "Get Items by Inventory", description = "Retrieve all items in a specific inventory", operationId = "getItemsByInventory")
        @ApiResponses(value = {
                        @ApiResponse(responseCode = "200", description = "List of items retrieved successfully", content = @Content(mediaType = "application/json", schema = @Schema(implementation = InventoryItem.class, type = "array")))
        })
        @GetMapping("/inventory/{inventoryId}")
        public ResponseEntity<List<InventoryItem>> getItemsByInventory(
                        @Parameter(description = "Inventory ID", required = true, example = "inv123") @PathVariable String inventoryId) {
                List<InventoryItem> response = inventoryItemService.getItemsByInventory(inventoryId);
                return ResponseEntity.ok(response);
        }

        /**
         * Get active items in an inventory endpoint
         */
        @Operation(summary = "Get Active Items by Inventory", description = "Retrieve all active items in a specific inventory", operationId = "getActiveItemsByInventory")
        @ApiResponses(value = {
                        @ApiResponse(responseCode = "200", description = "List of active items retrieved successfully", content = @Content(mediaType = "application/json", schema = @Schema(implementation = InventoryItem.class, type = "array")))
        })
        @GetMapping("/inventory/{inventoryId}/active")
        public ResponseEntity<List<InventoryItem>> getActiveItemsByInventory(
                        @Parameter(description = "Inventory ID", required = true, example = "inv123") @PathVariable String inventoryId) {
                List<InventoryItem> response = inventoryItemService.getActiveItemsByInventory(inventoryId);
                return ResponseEntity.ok(response);
        }

        /**
         * Get items by category within an inventory endpoint
         */
        @Operation(summary = "Get Items by Category", description = "Retrieve items by category within an inventory", operationId = "getItemsByCategory")
        @ApiResponses(value = {
                        @ApiResponse(responseCode = "200", description = "List of items by category retrieved successfully", content = @Content(mediaType = "application/json", schema = @Schema(implementation = InventoryItem.class, type = "array")))
        })
        @GetMapping("/inventory/{inventoryId}/category/{category}")
        public ResponseEntity<List<InventoryItem>> getItemsByCategory(
                        @Parameter(description = "Inventory ID", required = true, example = "inv123") @PathVariable String inventoryId,
                        @Parameter(description = "Category", required = true, example = "Electronics") @PathVariable String category) {
                List<InventoryItem> response = inventoryItemService.getItemsByCategory(inventoryId, category);
                return ResponseEntity.ok(response);
        }

        /**
         * Get low stock items endpoint
         */
        @Operation(summary = "Get Low Stock Items", description = "Retrieve items with current stock at or below minimum stock level", operationId = "getLowStockItems")
        @ApiResponses(value = {
                        @ApiResponse(responseCode = "200", description = "List of low stock items retrieved successfully", content = @Content(mediaType = "application/json", schema = @Schema(implementation = InventoryItem.class, type = "array")))
        })
        @GetMapping("/inventory/{inventoryId}/low-stock")
        public ResponseEntity<List<InventoryItem>> getLowStockItems(
                        @Parameter(description = "Inventory ID", required = true, example = "inv123") @PathVariable String inventoryId) {
                List<InventoryItem> response = inventoryItemService.getLowStockItems(inventoryId);
                return ResponseEntity.ok(response);
        }

        /**
         * Get out of stock items endpoint
         */
        @Operation(summary = "Get Out of Stock Items", description = "Retrieve items with zero current stock", operationId = "getOutOfStockItems")
        @ApiResponses(value = {
                        @ApiResponse(responseCode = "200", description = "List of out of stock items retrieved successfully", content = @Content(mediaType = "application/json", schema = @Schema(implementation = InventoryItem.class, type = "array")))
        })
        @GetMapping("/inventory/{inventoryId}/out-of-stock")
        public ResponseEntity<List<InventoryItem>> getOutOfStockItems(
                        @Parameter(description = "Inventory ID", required = true, example = "inv123") @PathVariable String inventoryId) {
                List<InventoryItem> response = inventoryItemService.getOutOfStockItems(inventoryId);
                return ResponseEntity.ok(response);
        }

        /**
         * Search items endpoint
         */
        @Operation(summary = "Search Items", description = "Search items by name, SKU, or barcode within an inventory", operationId = "searchItems")
        @ApiResponses(value = {
                        @ApiResponse(responseCode = "200", description = "Search results retrieved successfully", content = @Content(mediaType = "application/json", schema = @Schema(implementation = InventoryItem.class, type = "array")))
        })
        @GetMapping("/inventory/{inventoryId}/search")
        public ResponseEntity<List<InventoryItem>> searchItems(
                        @Parameter(description = "Inventory ID", required = true, example = "inv123") @PathVariable String inventoryId,
                        @Parameter(description = "Search query for item name, SKU, or barcode", required = true, example = "laptop") @RequestParam String q) {
                List<InventoryItem> response = inventoryItemService.searchItems(inventoryId, q);
                return ResponseEntity.ok(response);
        }

        /**
         * Update stock endpoint
         */
        @Operation(summary = "Update Stock", description = "Update the current stock quantity for an inventory item", operationId = "updateStock")
        @ApiResponses(value = {
                        @ApiResponse(responseCode = "200", description = "Stock updated successfully", content = @Content(mediaType = "application/json", schema = @Schema(implementation = InventoryItem.class))),
                        @ApiResponse(responseCode = "404", description = "Inventory item not found", content = @Content(mediaType = "application/json", schema = @Schema(implementation = Object.class))),
                        @ApiResponse(responseCode = "400", description = "Bad request - stock cannot be negative", content = @Content(mediaType = "application/json", schema = @Schema(implementation = Object.class)))
        })
        @PutMapping("/{id}/stock")
        public ResponseEntity<InventoryItem> updateStock(
                        @Parameter(description = "Inventory item ID", required = true, example = "item123") @PathVariable String id,
                        @Parameter(description = "New stock quantity", required = true, example = "75") @RequestParam Integer stock) {
                InventoryItem response = inventoryItemService.updateStock(id, stock);
                return ResponseEntity.ok(response);
        }

        /**
         * Delete inventory item endpoint
         */
        @Operation(summary = "Delete Inventory Item", description = "Soft delete an inventory item by setting isActive to false", operationId = "deleteInventoryItem")
        @ApiResponses(value = {
                        @ApiResponse(responseCode = "200", description = "Inventory item deleted successfully"),
                        @ApiResponse(responseCode = "404", description = "Inventory item not found", content = @Content(mediaType = "application/json", schema = @Schema(implementation = Object.class)))
        })
        @DeleteMapping("/{id}")
        public ResponseEntity<Void> deleteInventoryItem(
                        @Parameter(description = "Inventory item ID", required = true, example = "item123") @PathVariable String id) {
                inventoryItemService.deleteInventoryItem(id);
                return ResponseEntity.ok().build();
        }

        /**
         * Check if SKU exists endpoint
         */
        @Operation(summary = "Check SKU Exists", description = "Check if an item with the specified SKU exists within an inventory", operationId = "itemExistsBySku")
        @ApiResponses(value = {
                        @ApiResponse(responseCode = "200", description = "SKU existence check result", content = @Content(mediaType = "application/json", schema = @Schema(type = "boolean"), examples = {
                                        @ExampleObject(name = "SKU Exists", value = "true"),
                                        @ExampleObject(name = "SKU Not Found", value = "false")
                        }))
        })
        @GetMapping("/exists/sku/{sku}/inventory/{inventoryId}")
        public ResponseEntity<Boolean> itemExistsBySku(
                        @Parameter(description = "SKU to check", required = true, example = "LAPTOP001") @PathVariable String sku,
                        @Parameter(description = "Inventory ID", required = true, example = "inv123") @PathVariable String inventoryId) {
                boolean exists = inventoryItemService.itemExistsBySku(sku, inventoryId);
                return ResponseEntity.ok(exists);
        }

        /**
         * Check if barcode exists endpoint
         */
        @Operation(summary = "Check Barcode Exists", description = "Check if an item with the specified barcode exists within an inventory", operationId = "itemExistsByBarcode")
        @ApiResponses(value = {
                        @ApiResponse(responseCode = "200", description = "Barcode existence check result", content = @Content(mediaType = "application/json", schema = @Schema(type = "boolean"), examples = {
                                        @ExampleObject(name = "Barcode Exists", value = "true"),
                                        @ExampleObject(name = "Barcode Not Found", value = "false")
                        }))
        })
        @GetMapping("/exists/barcode/{barcode}/inventory/{inventoryId}")
        public ResponseEntity<Boolean> itemExistsByBarcode(
                        @Parameter(description = "Barcode to check", required = true, example = "1234567890123") @PathVariable String barcode,
                        @Parameter(description = "Inventory ID", required = true, example = "inv123") @PathVariable String inventoryId) {
                boolean exists = inventoryItemService.itemExistsByBarcode(barcode, inventoryId);
                return ResponseEntity.ok(exists);
        }

        /**
         * Get item count in inventory endpoint
         */
        @Operation(summary = "Get Item Count by Inventory", description = "Get the total number of items in an inventory", operationId = "getItemCountByInventory")
        @ApiResponses(value = {
                        @ApiResponse(responseCode = "200", description = "Item count retrieved successfully", content = @Content(mediaType = "application/json", schema = @Schema(type = "integer"), examples = @ExampleObject(name = "Item Count", value = "150")))
        })
        @GetMapping("/inventory/{inventoryId}/count")
        public ResponseEntity<Long> getItemCountByInventory(
                        @Parameter(description = "Inventory ID", required = true, example = "inv123") @PathVariable String inventoryId) {
                long count = inventoryItemService.getItemCountByInventory(inventoryId);
                return ResponseEntity.ok(count);
        }

        /**
         * Get active item count in inventory endpoint
         */
        @Operation(summary = "Get Active Item Count by Inventory", description = "Get the total number of active items in an inventory", operationId = "getActiveItemCountByInventory")
        @ApiResponses(value = {
                        @ApiResponse(responseCode = "200", description = "Active item count retrieved successfully", content = @Content(mediaType = "application/json", schema = @Schema(type = "integer"), examples = @ExampleObject(name = "Active Item Count", value = "125")))
        })
        @GetMapping("/inventory/{inventoryId}/count/active")
        public ResponseEntity<Long> getActiveItemCountByInventory(
                        @Parameter(description = "Inventory ID", required = true, example = "inv123") @PathVariable String inventoryId) {
                long count = inventoryItemService.getActiveItemCountByInventory(inventoryId);
                return ResponseEntity.ok(count);
        }
}