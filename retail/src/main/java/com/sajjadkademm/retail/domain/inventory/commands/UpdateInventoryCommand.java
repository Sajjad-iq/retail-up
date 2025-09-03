package com.sajjadkademm.retail.domain.inventory.commands;

import com.sajjadkademm.retail.shared.cqrs.Command;
import com.sajjadkademm.retail.application.dto.inventory.UpdateInventoryRequest;
import com.sajjadkademm.retail.domain.inventory.model.Inventory;

import lombok.Builder;
import lombok.Data;

/**
 * Command to update an existing inventory
 */
@Data
@Builder
public class UpdateInventoryCommand implements Command<Inventory> {
    
    private final String userId;
    private final String inventoryId;
    private final UpdateInventoryRequest request;

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
        if (request == null) {
            throw new IllegalArgumentException("Request is required");
        }
    }
}