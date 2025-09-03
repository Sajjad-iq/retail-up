package com.sajjadkademm.retail.domain.inventory.commands;

import com.sajjadkademm.retail.shared.cqrs.Command;
import com.sajjadkademm.retail.application.dto.inventory.CreateInventoryRequest;
import com.sajjadkademm.retail.domain.inventory.model.Inventory;

import lombok.Builder;
import lombok.Data;

/**
 * Command to create a new inventory
 */
@Data
@Builder
public class CreateInventoryCommand implements Command<Inventory> {
    
    private final String userId;
    private final CreateInventoryRequest request;

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
        if (request.getName() == null || request.getName().trim().isEmpty()) {
            throw new IllegalArgumentException("Inventory name is required");
        }
        if (request.getOrganizationId() == null || request.getOrganizationId().trim().isEmpty()) {
            throw new IllegalArgumentException("Organization ID is required");
        }
    }
}