package com.sajjadkademm.retail.domain.inventory.queries;

import com.sajjadkademm.retail.shared.cqrs.Query;

import lombok.Builder;
import lombok.Data;

/**
 * Query to get count of inventories by organization
 */
@Data
@Builder
public class GetInventoryCountByOrganizationQuery implements Query<Long> {
    
    private final String userId;
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
        if (organizationId == null || organizationId.trim().isEmpty()) {
            throw new IllegalArgumentException("Organization ID is required");
        }
    }
}