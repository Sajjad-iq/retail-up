package com.sajjadkademm.retail.domain.inventory.queries;

import com.sajjadkademm.retail.shared.cqrs.Query;
import com.sajjadkademm.retail.domain.inventory.readmodels.InventorySummaryReport;

import lombok.Builder;
import lombok.Data;

/**
 * Query to get inventory summary report with cached results
 */
@Data
@Builder
public class GetInventorySummaryQuery implements Query<InventorySummaryReport> {
    
    private final String userId;
    private final String inventoryId;

    @Override
    public String getUserId() {
        return userId;
    }

    @Override
    public void validate() {
        if (userId == null || userId.trim().isEmpty()) {
            throw new IllegalArgumentException("User ID is required");
        }
        if (inventoryId == null || inventoryId.trim().isEmpty()) {
            throw new IllegalArgumentException("Inventory ID is required");
        }
    }
}