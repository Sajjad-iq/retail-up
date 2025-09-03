package com.sajjadkademm.retail.domain.inventory.queries;

import com.sajjadkademm.retail.shared.cqrs.Query;
import com.sajjadkademm.retail.domain.inventory.model.Inventory;

import lombok.Builder;
import lombok.Data;
import java.util.List;

/**
 * Query to get all inventories by organization
 */
@Data
@Builder
public class GetInventoriesByOrganizationQuery implements Query<List<Inventory>> {
    
    private final String userId;
    private final String organizationId;
    private final Boolean activeOnly; // true for active only, false/null for all

    @Override
    public String getUserId() {
        return userId;
    }

    @Override
    public void validate() {
        if (userId == null || userId.trim().isEmpty()) {
            throw new IllegalArgumentException("User ID is required");
        }
        if (organizationId == null || organizationId.trim().isEmpty()) {
            throw new IllegalArgumentException("Organization ID is required");
        }
    }
}