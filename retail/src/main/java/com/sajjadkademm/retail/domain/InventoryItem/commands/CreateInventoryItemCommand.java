package com.sajjadkademm.retail.domain.inventory.commands;

import com.sajjadkademm.retail.shared.cqrs.Command;
import com.sajjadkademm.retail.application.dto.inventory.CreateInventoryItemRequest;
import com.sajjadkademm.retail.domain.inventory.model.InventoryItem;

import lombok.Builder;
import lombok.Data;

/**
 * Command to create a new inventory item.
 * Wraps the existing CreateInventoryItemRequest to maintain API compatibility.
 */
@Data
@Builder
public class CreateInventoryItemCommand implements Command<InventoryItem> {
    
    private final String userId;
    private final CreateInventoryItemRequest request;

    @Override
    public String getUserId() {
        return userId;
    }

    @Override
    public void validate() {
        if (userId == null || userId.trim().isEmpty()) {
            throw new IllegalArgumentException("User ID is required");
        }
        if (request == null) {
            throw new IllegalArgumentException("Request is required");
        }
        if (request.getInventoryId() == null || request.getInventoryId().trim().isEmpty()) {
            throw new IllegalArgumentException("Inventory ID is required");
        }
        if (request.getName() == null || request.getName().trim().isEmpty()) {
            throw new IllegalArgumentException("Product name is required");
        }
    }
}