package com.sajjadkademm.retail.domain.inventory.queries;

import com.sajjadkademm.retail.shared.cqrs.Query;

import lombok.Builder;
import lombok.Data;

/**
 * Query to check if inventory exists by name within an organization
 */
@Data
@Builder
public class InventoryExistsByNameQuery implements Query<Boolean> {
    
    private final String userId;
    private final String name;
    private final String organizationId;

    @Override
    public String getUserId() {
        return userId;
    }

    @Override
    public void validate() {
        if (userId == null || userId.trim().isEmpty()) {
            throw new IllegalArgumentException("User ID is required");
        }
        if (name == null || name.trim().isEmpty()) {
            throw new IllegalArgumentException("Name is required");
        }
        if (organizationId == null || organizationId.trim().isEmpty()) {
            throw new IllegalArgumentException("Organization ID is required");
        }
    }
}