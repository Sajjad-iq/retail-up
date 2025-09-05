package com.sajjadkademm.retail.domain.InventoryItem.commands;

import com.sajjadkademm.retail.shared.cqrs.Command;

import lombok.Builder;
import lombok.Data;

/**
 * Command to delete an inventory item
 */
@Data
@Builder
public class DeleteInventoryItemCommand implements Command<Void> {
    
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