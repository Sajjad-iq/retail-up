package com.sajjadkademm.retail.inventory.InventoryItem;

import com.sajjadkademm.retail.inventory.InventoryItem.dto.CreateInventoryItemRequest;
import com.sajjadkademm.retail.inventory.InventoryItem.dto.Unit;
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
        @ApiResponse(responseCode = "200", description = "Inventory item created successfully", content = @Content(mediaType = "application/json", schema = @Schema(implementation = InventoryItem.class), examples = @ExampleObject(name = "Created Inventory Item", value = """
                                        {
                                            "id": "item123",
                                            "name": "Laptop Computer",
                                            "description": "High-performance laptop for business use",
                                            "sku": "LAPTOP001",
                                            "barcode": "1234567890123",
                                            "category": "Electronics",
                                            "brand": "TechCorp",
                                            "unit": "PIECES",
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
                                        """)))
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
                                            "unit": "PIECES",
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
         * Get inventory item by SKU endpoint
         */
        @Operation(summary = "Get Inventory Item by SKU", description = "Retrieve inventory item details by SKU within an inventory", operationId = "getInventoryItemBySku")
        @ApiResponse(responseCode = "200", description = "Inventory item found", content = @Content(mediaType = "application/json", schema = @Schema(implementation = InventoryItem.class)))
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
        @ApiResponse(responseCode = "200", description = "Inventory item found", content = @Content(mediaType = "application/json", schema = @Schema(implementation = InventoryItem.class)))
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
        @ApiResponse(responseCode = "200", description = "List of items retrieved successfully", content = @Content(mediaType = "application/json", schema = @Schema(implementation = InventoryItem.class, type = "array")))
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
        @ApiResponse(responseCode = "200", description = "List of active items retrieved successfully", content = @Content(mediaType = "application/json", schema = @Schema(implementation = InventoryItem.class, type = "array")))
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
        @ApiResponse(responseCode = "200", description = "List of items by category retrieved successfully", content = @Content(mediaType = "application/json", schema = @Schema(implementation = InventoryItem.class, type = "array")))
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
        @ApiResponse(responseCode = "200", description = "List of low stock items retrieved successfully", content = @Content(mediaType = "application/json", schema = @Schema(implementation = InventoryItem.class, type = "array")))
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
        @ApiResponse(responseCode = "200", description = "List of out of stock items retrieved successfully", content = @Content(mediaType = "application/json", schema = @Schema(implementation = InventoryItem.class, type = "array")))
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
        @ApiResponse(responseCode = "200", description = "Search results retrieved successfully", content = @Content(mediaType = "application/json", schema = @Schema(implementation = InventoryItem.class, type = "array")))
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
        @ApiResponse(responseCode = "200", description = "Stock updated successfully", content = @Content(mediaType = "application/json", schema = @Schema(implementation = InventoryItem.class)))
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
        @ApiResponse(responseCode = "200", description = "Inventory item deleted successfully")
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
        @ApiResponse(responseCode = "200", description = "SKU existence check result", content = @Content(mediaType = "application/json", schema = @Schema(type = "boolean"), examples = {
                                        @ExampleObject(name = "SKU Exists", value = "true"),
                                        @ExampleObject(name = "SKU Not Found", value = "false")
                        }))
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
        @ApiResponse(responseCode = "200", description = "Barcode existence check result", content = @Content(mediaType = "application/json", schema = @Schema(type = "boolean"), examples = {
                                        @ExampleObject(name = "Barcode Exists", value = "true"),
                                        @ExampleObject(name = "Barcode Not Found", value = "false")
                        }))
        @GetMapping("/exists/barcode/{barcode}/inventory/{inventoryId}")
        public ResponseEntity<Boolean> itemExistsByBarcode(
                        @Parameter(description = "Barcode to check", required = true, example = "1234567890123") @PathVariable String barcode,
                        @Parameter(description = "Inventory ID", required = true, example = "inv123") @PathVariable String inventoryId) {
                boolean exists = inventoryItemService.itemExistsByBarcode(barcode, inventoryId);
                return ResponseEntity.ok(exists);
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

        // New endpoints for additional functionality

        @GetMapping("/product-code/{productCode}")
        @Operation(summary = "Get inventory item by product code", description = "Retrieve an inventory item by its product code")
        @ApiResponse(responseCode = "200", description = "Inventory item retrieved successfully")
        public ResponseEntity<InventoryItem> getInventoryItemByProductCode(
                @PathVariable String productCode,
                @RequestParam String inventoryId) {
            InventoryItem item = inventoryItemService.getInventoryItemByProductCode(productCode, inventoryId);
            return ResponseEntity.ok(item);
        }

        @GetMapping("/brand/{brand}")
        @Operation(summary = "Get items by brand", description = "Retrieve all items of a specific brand in an inventory")
        @ApiResponse(responseCode = "200", description = "Items retrieved successfully")
        public ResponseEntity<List<InventoryItem>> getItemsByBrand(
                @PathVariable String brand,
                @RequestParam String inventoryId) {
            List<InventoryItem> items = inventoryItemService.getItemsByBrand(inventoryId, brand);
            return ResponseEntity.ok(items);
        }

        @GetMapping("/supplier/{supplierName}")
        @Operation(summary = "Get items by supplier", description = "Retrieve all items from a specific supplier in an inventory")
        @ApiResponse(responseCode = "200", description = "Items retrieved successfully")
        public ResponseEntity<List<InventoryItem>> getItemsBySupplier(
                @PathVariable String supplierName,
                @RequestParam String inventoryId) {
            List<InventoryItem> items = inventoryItemService.getItemsBySupplier(inventoryId, supplierName);
            return ResponseEntity.ok(items);
        }

        @GetMapping("/location/{location}")
        @Operation(summary = "Get items by location", description = "Retrieve all items in a specific location within an inventory")
        @ApiResponse(responseCode = "200", description = "Items retrieved successfully")
        public ResponseEntity<List<InventoryItem>> getItemsByLocation(
                @PathVariable String location,
                @RequestParam String inventoryId) {
            List<InventoryItem> items = inventoryItemService.getItemsByLocation(inventoryId, location);
            return ResponseEntity.ok(items);
        }

        @GetMapping("/color/{color}")
        @Operation(summary = "Get items by color", description = "Retrieve all items of a specific color in an inventory")
        @ApiResponse(responseCode = "200", description = "Items retrieved successfully")
        public ResponseEntity<List<InventoryItem>> getItemsByColor(
                @PathVariable String color,
                @RequestParam String inventoryId) {
            List<InventoryItem> items = inventoryItemService.getItemsByColor(inventoryId, color);
            return ResponseEntity.ok(items);
        }

        @GetMapping("/size/{size}")
        @Operation(summary = "Get items by size", description = "Retrieve all items of a specific size in an inventory")
        @ApiResponse(responseCode = "200", description = "Items retrieved successfully")
        public ResponseEntity<List<InventoryItem>> getItemsBySize(
                @PathVariable String size,
                @RequestParam String inventoryId) {
            List<InventoryItem> items = inventoryItemService.getItemsBySize(inventoryId, size);
            return ResponseEntity.ok(items);
        }

        @GetMapping("/perishable")
        @Operation(summary = "Get perishable items", description = "Retrieve all perishable items in an inventory")
        @ApiResponse(responseCode = "200", description = "Perishable items retrieved successfully")
        public ResponseEntity<List<InventoryItem>> getPerishableItems(@RequestParam String inventoryId) {
            List<InventoryItem> items = inventoryItemService.getPerishableItems(inventoryId);
            return ResponseEntity.ok(items);
        }

        @GetMapping("/expiring-soon")
        @Operation(summary = "Get items expiring soon", description = "Retrieve items that will expire within specified days")
        @ApiResponse(responseCode = "200", description = "Items expiring soon retrieved successfully")
        public ResponseEntity<List<InventoryItem>> getItemsExpiringSoon(
                @RequestParam String inventoryId,
                @RequestParam(defaultValue = "7") int days) {
            List<InventoryItem> items = inventoryItemService.getItemsExpiringSoon(inventoryId, days);
            return ResponseEntity.ok(items);
        }

        @GetMapping("/expired")
        @Operation(summary = "Get expired items", description = "Retrieve all expired items in an inventory")
        @ApiResponse(responseCode = "200", description = "Expired items retrieved successfully")
        public ResponseEntity<List<InventoryItem>> getExpiredItems(@RequestParam String inventoryId) {
            List<InventoryItem> items = inventoryItemService.getExpiredItems(inventoryId);
            return ResponseEntity.ok(items);
        }

        @GetMapping("/discounted")
        @Operation(summary = "Get items with active discounts", description = "Retrieve all items with currently active discounts")
        @ApiResponse(responseCode = "200", description = "Discounted items retrieved successfully")
        public ResponseEntity<List<InventoryItem>> getItemsWithActiveDiscounts(@RequestParam String inventoryId) {
            List<InventoryItem> items = inventoryItemService.getItemsWithActiveDiscounts(inventoryId);
            return ResponseEntity.ok(items);
        }

        @PutMapping("/{id}/sales")
        @Operation(summary = "Update sales data", description = "Update sales-related data for an inventory item")
        @ApiResponse(responseCode = "200", description = "Sales data updated successfully")
        public ResponseEntity<InventoryItem> updateSalesData(
                @PathVariable String id,
                @RequestParam int quantitySold,
                @RequestParam java.math.BigDecimal revenue) {
            InventoryItem updatedItem = inventoryItemService.updateSalesData(id, quantitySold, revenue);
            return ResponseEntity.ok(updatedItem);
        }

        @GetMapping("/product-code/{productCode}/exists")
        @Operation(summary = "Check if product code exists", description = "Check if a product code already exists in the inventory")
        @ApiResponse(responseCode = "200", description = "Product code existence checked successfully")
        public ResponseEntity<Boolean> productCodeExists(
                @PathVariable String productCode,
                @RequestParam String inventoryId) {
            boolean exists = inventoryItemService.productCodeExists(productCode, inventoryId);
            return ResponseEntity.ok(exists);
        }

        @GetMapping("/value/total")
        @Operation(summary = "Get total inventory value", description = "Calculate the total value of all items in the inventory")
        @ApiResponse(responseCode = "200", description = "Total inventory value calculated successfully")
        public ResponseEntity<java.math.BigDecimal> getTotalInventoryValue(@RequestParam String inventoryId) {
            java.math.BigDecimal totalValue = inventoryItemService.getTotalInventoryValue(inventoryId);
            return ResponseEntity.ok(totalValue);
        }

        @GetMapping("/cost/total")
        @Operation(summary = "Get total inventory cost", description = "Calculate the total cost of all items in the inventory")
        @ApiResponse(responseCode = "200", description = "Total inventory cost calculated successfully")
        public ResponseEntity<java.math.BigDecimal> getTotalInventoryCost(@RequestParam String inventoryId) {
            java.math.BigDecimal totalCost = inventoryItemService.getTotalInventoryCost(inventoryId);
            return ResponseEntity.ok(totalCost);
        }

        @GetMapping("/reorder")
        @Operation(summary = "Get items needing reorder", description = "Retrieve items that need to be reordered (low stock)")
        @ApiResponse(responseCode = "200", description = "Items needing reorder retrieved successfully")
        public ResponseEntity<List<InventoryItem>> getItemsNeedingReorder(@RequestParam String inventoryId) {
            List<InventoryItem> items = inventoryItemService.getItemsNeedingReorder(inventoryId);
            return ResponseEntity.ok(items);
        }

        @GetMapping("/top-selling")
        @Operation(summary = "Get top selling items", description = "Retrieve the top selling items in the inventory")
        @ApiResponse(responseCode = "200", description = "Top selling items retrieved successfully")
        public ResponseEntity<List<InventoryItem>> getTopSellingItems(
                @RequestParam String inventoryId,
                @RequestParam(defaultValue = "10") int limit) {
            List<InventoryItem> items = inventoryItemService.getTopSellingItems(inventoryId, limit);
            return ResponseEntity.ok(items);
        }
}