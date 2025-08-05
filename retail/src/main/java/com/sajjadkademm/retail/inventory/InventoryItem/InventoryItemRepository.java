package com.sajjadkademm.retail.inventory.InventoryItem;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.List;
import java.util.Optional;

public interface InventoryItemRepository extends JpaRepository<InventoryItem, String> {

    // Find item by SKU within an inventory
    Optional<InventoryItem> findBySkuAndInventoryId(String sku, String inventoryId);

    // Find item by barcode within an inventory
    Optional<InventoryItem> findByBarcodeAndInventoryId(String barcode, String inventoryId);

    // Find all items in an inventory
    List<InventoryItem> findByInventoryId(String inventoryId);

    // Find active items in an inventory
    List<InventoryItem> findByInventoryIdAndIsActiveTrue(String inventoryId);

    // Find items by category within an inventory
    List<InventoryItem> findByInventoryIdAndCategory(String inventoryId, String category);

    // Find items with low stock (current stock <= minimum stock)
    @Query("SELECT i FROM InventoryItem i WHERE i.inventoryId = :inventoryId AND i.currentStock <= i.minimumStock AND i.isActive = true")
    List<InventoryItem> findLowStockItems(@Param("inventoryId") String inventoryId);

    // Find items with zero stock
    @Query("SELECT i FROM InventoryItem i WHERE i.inventoryId = :inventoryId AND i.currentStock = 0 AND i.isActive = true")
    List<InventoryItem> findOutOfStockItems(@Param("inventoryId") String inventoryId);

    // Search items by name, SKU, or barcode within an inventory
    @Query("SELECT i FROM InventoryItem i WHERE i.inventoryId = :inventoryId AND (i.name LIKE %:searchTerm% OR i.sku LIKE %:searchTerm% OR i.barcode LIKE %:searchTerm%)")
    List<InventoryItem> searchItems(@Param("inventoryId") String inventoryId, @Param("searchTerm") String searchTerm);

    // Check if SKU exists within an inventory
    boolean existsBySkuAndInventoryId(String sku, String inventoryId);

    // Check if barcode exists within an inventory
    boolean existsByBarcodeAndInventoryId(String barcode, String inventoryId);

    // Count items in an inventory
    long countByInventoryId(String inventoryId);

    // Count active items in an inventory
    long countByInventoryIdAndIsActiveTrue(String inventoryId);
}