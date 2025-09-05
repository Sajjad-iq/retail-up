package com.sajjadkademm.retail.domain.inventory.queries;

import com.sajjadkademm.retail.shared.cqrs.Query;
import com.sajjadkademm.retail.application.dto.inventory.FilterRequest;
import com.sajjadkademm.retail.application.dto.inventory.PagedResponse;
import com.sajjadkademm.retail.domain.InventoryItem.model.InventoryItem;

import lombok.Builder;
import lombok.Data;

/**
 * Query to get filtered and paginated inventory items.
 * Wraps the existing filtering logic to maintain API compatibility.
 */
@Data
@Builder
public class GetInventoryItemsQuery implements Query<PagedResponse<InventoryItem>> {
    
    private final String userId;
    private final String inventoryId;
    private final FilterRequest filterRequest;
    private final int page;
    private final int size;
    private final String sortBy;
    private final String sortDirection;

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
        if (page < 0) {
            throw new IllegalArgumentException("Page must be >= 0");
        }
        if (size <= 0) {
            throw new IllegalArgumentException("Size must be > 0");
        }
    }
}