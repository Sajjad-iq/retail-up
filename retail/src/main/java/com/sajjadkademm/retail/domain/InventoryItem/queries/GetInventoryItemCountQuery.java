package com.sajjadkademm.retail.domain.InventoryItem.queries;

import com.sajjadkademm.retail.shared.cqrs.Query;

import lombok.Builder;
import lombok.Data;

/**
 * Query to get inventory item count by inventory ID
 */
@Data
@Builder
public class GetInventoryItemCountQuery implements Query<Long> {
    
    private final String userId;
    private final String inventoryId;
    private final Boolean activeOnly; // true for active count, false/null for total count

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