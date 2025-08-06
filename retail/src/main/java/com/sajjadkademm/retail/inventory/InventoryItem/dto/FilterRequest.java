package com.sajjadkademm.retail.inventory.InventoryItem.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;
import java.time.LocalDate;

/**
 * Filter request DTO for inventory item filtering
 * 
 * @author Sajjad Kadem
 * @version 1.0
 * @since 2024-12-19
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class FilterRequest {
    
    // Basic filters
    private String category;
    private String brand;
    private String supplierName;
    private String location;
    private String color;
    private String size;
    private String searchTerm; // For name, SKU, or barcode search
    
    // Stock filters
    private Boolean lowStock;
    private Boolean outOfStock;
    private Integer minStock;
    private Integer maxStock;
    
    // Price filters
    private BigDecimal minCostPrice;
    private BigDecimal maxCostPrice;
    private BigDecimal minSellingPrice;
    private BigDecimal maxSellingPrice;
    
    // Date filters
    private LocalDate createdAfter;
    private LocalDate createdBefore;
    private LocalDate expiryAfter;
    private LocalDate expiryBefore;
    
    // Status filters
    private Boolean isActive;
    private Boolean isPerishable;
    private Boolean hasDiscount;
    
    // Expiry status
    private String expiryStatus; // "expiring", "expired", "fresh"
    private Integer expiryDays; // Days for expiry calculation
    
    // Sorting
    private String sortBy; // Field to sort by
    private String sortDirection; // "asc" or "desc"
}