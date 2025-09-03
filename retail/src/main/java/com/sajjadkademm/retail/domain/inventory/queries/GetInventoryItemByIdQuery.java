package com.sajjadkademm.retail.domain.inventory.queries;

import com.sajjadkademm.retail.shared.cqrs.Query;
import com.sajjadkademm.retail.domain.inventory.model.InventoryItem;

import lombok.Builder;
import lombok.Data;

/**
 * Query to get a single inventory item by ID
 */
@Data
@Builder
public class GetInventoryItemByIdQuery implements Query<InventoryItem> {
    
    private final String userId;
    private final String itemId;

    @Override
    public String getUserId() {
        return userId;
    }

    @Override
    public void validate() {
        if (userId == null || userId.trim().isEmpty()) {
            throw new IllegalArgumentException("User ID is required");
        }
        if (itemId == null || itemId.trim().isEmpty()) {
            throw new IllegalArgumentException("Item ID is required");
        }
    }
}