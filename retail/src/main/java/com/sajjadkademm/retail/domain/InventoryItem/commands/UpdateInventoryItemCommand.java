package com.sajjadkademm.retail.domain.inventory.commands;

import com.sajjadkademm.retail.shared.cqrs.Command;
import com.sajjadkademm.retail.application.dto.inventory.UpdateInventoryItemRequest;
import com.sajjadkademm.retail.domain.inventory.model.InventoryItem;

import lombok.Builder;
import lombok.Data;

/**
 * Command to update an existing inventory item.
 * Wraps the existing UpdateInventoryItemRequest to maintain API compatibility.
 */
@Data
@Builder
public class UpdateInventoryItemCommand implements Command<InventoryItem> {
    
    private final String userId;
    private final String itemId;
    private final UpdateInventoryItemRequest request;

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
        if (request == null) {
            throw new IllegalArgumentException("Request is required");
        }
    }
}