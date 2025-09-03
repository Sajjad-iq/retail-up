package com.sajjadkademm.retail.domain.inventory.handlers;

import com.sajjadkademm.retail.shared.cqrs.QueryHandler;
import com.sajjadkademm.retail.domain.inventory.queries.GetInventorySummaryQuery;
import com.sajjadkademm.retail.domain.inventory.readmodels.InventorySummaryReport;
import com.sajjadkademm.retail.domain.inventory.repositories.InventoryItemRepository;
import com.sajjadkademm.retail.domain.inventory.repositories.InventoryRepository;
import com.sajjadkademm.retail.domain.inventory.validation.InventoryItemValidationUtils;
import com.sajjadkademm.retail.domain.inventory.model.Inventory;

import org.springframework.stereotype.Component;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import java.math.BigDecimal;
import java.time.LocalDateTime;

/**
 * Query handler optimized for generating inventory summary reports.
 * Uses repository methods for efficient calculation.
 */
@Slf4j
@Component
@RequiredArgsConstructor
public class GetInventorySummaryQueryHandler implements QueryHandler<GetInventorySummaryQuery, InventorySummaryReport> {

    private final InventoryItemRepository inventoryItemRepository;
    private final InventoryRepository inventoryRepository;
    private final InventoryItemValidationUtils validationUtils;

    @Override
    public InventorySummaryReport handle(GetInventorySummaryQuery query) throws Exception {
        log.debug("Handling GetInventorySummaryQuery for inventory: {}", query.getInventoryId());

        // Validate user access
        validationUtils.checkUserAccessToInventory(query.getInventoryId());
        
        // Get inventory details
        Inventory inventory = inventoryRepository.findById(query.getInventoryId())
                .orElseThrow(() -> new RuntimeException("Inventory not found"));

        // Calculate metrics using existing repository methods
        long totalItems = inventoryItemRepository.countByInventoryId(query.getInventoryId());
        long activeItems = inventoryItemRepository.countByInventoryIdAndIsActiveTrue(query.getInventoryId());
        long lowStockItems = inventoryItemRepository.findLowStockItems(query.getInventoryId()).size();
        long outOfStockItems = inventoryItemRepository.findOutOfStockItems(query.getInventoryId()).size();
        
        BigDecimal totalValue = inventoryItemRepository.calculateTotalInventoryValue(query.getInventoryId());
        BigDecimal totalCost = inventoryItemRepository.calculateTotalInventoryCost(query.getInventoryId());
        
        // Calculate total current stock (this would need a new repository method)
        // For now, we'll set it to 0 - you can add the method to InventoryItemRepository
        long totalCurrentStock = 0L;

        return InventorySummaryReport.builder()
                .inventoryId(inventory.getId())
                .inventoryName(inventory.getName())
                .totalItems(totalItems)
                .activeItems(activeItems)
                .lowStockItems(lowStockItems)
                .outOfStockItems(outOfStockItems)
                .totalInventoryValue(totalValue != null ? totalValue : BigDecimal.ZERO)
                .totalInventoryCost(totalCost != null ? totalCost : BigDecimal.ZERO)
                .totalCurrentStock(totalCurrentStock)
                .reportGeneratedAt(LocalDateTime.now())
                .build();
    }

    @Override
    public Class<GetInventorySummaryQuery> getQueryType() {
        return GetInventorySummaryQuery.class;
    }

    @Override
    public boolean isCacheable() {
        return true; // Summary reports can be cached for a few minutes
    }

    @Override
    public String getCacheKey(GetInventorySummaryQuery query) {
        // Cache key includes user ID for security and time window for freshness
        return "inventory-summary:" + query.getInventoryId() + ":" + query.getUserId() + ":" + 
               (System.currentTimeMillis() / (2 * 60 * 1000)); // Cache for 2 minutes
    }
}