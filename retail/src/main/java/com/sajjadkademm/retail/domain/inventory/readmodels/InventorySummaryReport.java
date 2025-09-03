package com.sajjadkademm.retail.domain.inventory.readmodels;

import lombok.Builder;
import lombok.Data;

import java.math.BigDecimal;
import java.time.LocalDateTime;

/**
 * Read model optimized for inventory summary reporting.
 * Contains pre-calculated metrics for dashboard/reporting purposes.
 */
@Data
@Builder
public class InventorySummaryReport {
    
    // Basic inventory info
    private final String inventoryId;
    private final String inventoryName;
    
    // Item counts
    private final Long totalItems;
    private final Long activeItems;
    private final Long lowStockItems;
    private final Long outOfStockItems;
    
    // Financial summary
    private final BigDecimal totalInventoryValue; // Current stock * selling price
    private final BigDecimal totalInventoryCost;  // Current stock * cost price
    
    // Stock summary
    private final Long totalCurrentStock;
    
    // Generated timestamp
    private final LocalDateTime reportGeneratedAt;
}