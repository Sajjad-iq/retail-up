package com.sajjadkademm.retail.inventory.InventoryItem;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.math.BigDecimal;
import java.util.List;
import java.util.Optional;

public interface InventoryItemRepository extends JpaRepository<InventoryItem, String> {

    // Find item by SKU within an inventory
    Optional<InventoryItem> findBySkuAndInventoryId(String sku, String inventoryId);

    // Find item by barcode within an inventory
    Optional<InventoryItem> findByBarcodeAndInventoryId(String barcode, String inventoryId);

    // Find item by product code within an inventory
    Optional<InventoryItem> findByProductCodeAndInventoryId(String productCode, String inventoryId);

    // Find all items in an inventory
    List<InventoryItem> findByInventoryId(String inventoryId);

    // Find active items in an inventory
    List<InventoryItem> findByInventoryIdAndIsActiveTrue(String inventoryId);

    // Find items by category within an inventory
    List<InventoryItem> findByInventoryIdAndCategory(String inventoryId, String category);

    // Find items by brand within an inventory
    List<InventoryItem> findByInventoryIdAndBrand(String inventoryId, String brand);

    // Find items by supplier within an inventory
    List<InventoryItem> findByInventoryIdAndSupplierName(String inventoryId, String supplierName);

    // Find items by location within an inventory
    List<InventoryItem> findByInventoryIdAndLocation(String inventoryId, String location);

    // Find items by color within an inventory
    List<InventoryItem> findByInventoryIdAndColor(String inventoryId, String color);

    // Find items by size within an inventory
    List<InventoryItem> findByInventoryIdAndSize(String inventoryId, String size);

    // Find perishable items in an inventory
    List<InventoryItem> findByInventoryIdAndIsPerishableTrue(String inventoryId);

    // Find items with low stock (current stock <= minimum stock)
    @Query("SELECT i FROM InventoryItem i WHERE i.inventoryId = :inventoryId AND i.currentStock <= i.minimumStock AND i.isActive = true")
    List<InventoryItem> findLowStockItems(@Param("inventoryId") String inventoryId);

    // Find items with zero stock
    @Query("SELECT i FROM InventoryItem i WHERE i.inventoryId = :inventoryId AND i.currentStock = 0 AND i.isActive = true")
    List<InventoryItem> findOutOfStockItems(@Param("inventoryId") String inventoryId);

    // Find items expiring soon (within specified days)
    @Query("SELECT i FROM InventoryItem i WHERE i.inventoryId = :inventoryId AND i.isPerishable = true AND i.expiryDate IS NOT NULL AND i.expiryDate <= CURRENT_DATE + :days AND i.isActive = true")
    List<InventoryItem> findItemsExpiringSoon(@Param("inventoryId") String inventoryId, @Param("days") int days);

    // Find expired items
    @Query("SELECT i FROM InventoryItem i WHERE i.inventoryId = :inventoryId AND i.isPerishable = true AND i.expiryDate IS NOT NULL AND i.expiryDate < CURRENT_DATE AND i.isActive = true")
    List<InventoryItem> findExpiredItems(@Param("inventoryId") String inventoryId);

    // Find items with active discounts
    @Query("SELECT i FROM InventoryItem i WHERE i.inventoryId = :inventoryId AND i.discountPrice IS NOT NULL AND i.discountStartDate <= CURRENT_TIMESTAMP AND i.discountEndDate >= CURRENT_TIMESTAMP AND i.isActive = true")
    List<InventoryItem> findItemsWithActiveDiscounts(@Param("inventoryId") String inventoryId);

    // Search items by name, SKU, barcode, or product code within an inventory
    @Query("SELECT i FROM InventoryItem i WHERE i.inventoryId = :inventoryId AND (i.name LIKE %:searchTerm% OR i.sku LIKE %:searchTerm% OR i.barcode LIKE %:searchTerm% OR i.productCode LIKE %:searchTerm%)")
    List<InventoryItem> searchItems(@Param("inventoryId") String inventoryId, @Param("searchTerm") String searchTerm);

    // Find top selling items
    @Query("SELECT i FROM InventoryItem i WHERE i.inventoryId = :inventoryId AND i.isActive = true ORDER BY i.totalSold DESC LIMIT :limit")
    List<InventoryItem> findTopSellingItems(@Param("inventoryId") String inventoryId, @Param("limit") int limit);

    // Calculate total inventory value (selling price * current stock)
    @Query("SELECT COALESCE(SUM(i.sellingPrice * i.currentStock), 0) FROM InventoryItem i WHERE i.inventoryId = :inventoryId AND i.isActive = true")
    BigDecimal calculateTotalInventoryValue(@Param("inventoryId") String inventoryId);

    // Calculate total inventory cost (cost price * current stock)
    @Query("SELECT COALESCE(SUM(i.costPrice * i.currentStock), 0) FROM InventoryItem i WHERE i.inventoryId = :inventoryId AND i.costPrice IS NOT NULL AND i.isActive = true")
    BigDecimal calculateTotalInventoryCost(@Param("inventoryId") String inventoryId);

    // Check if SKU exists within an inventory
    boolean existsBySkuAndInventoryId(String sku, String inventoryId);

    // Check if barcode exists within an inventory
    boolean existsByBarcodeAndInventoryId(String barcode, String inventoryId);

    // Check if product code exists within an inventory
    boolean existsByProductCodeAndInventoryId(String productCode, String inventoryId);

    // Count items in an inventory
    long countByInventoryId(String inventoryId);

    // Count active items in an inventory
    long countByInventoryIdAndIsActiveTrue(String inventoryId);
}