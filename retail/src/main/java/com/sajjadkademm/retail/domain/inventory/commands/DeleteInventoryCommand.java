package com.sajjadkademm.retail.domain.inventory.commands;

import com.sajjadkademm.retail.shared.cqrs.Command;

import lombok.Builder;
import lombok.Data;

/**
 * Command to delete an inventory
 */
@Data
@Builder
public class DeleteInventoryCommand implements Command<Void> {
    
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