package com.sajjadkademm.retail.domain.organization.queries;

import com.sajjadkademm.retail.shared.cqrs.Query;
import com.sajjadkademm.retail.domain.organization.model.Organization;

import lombok.Builder;
import lombok.Data;

/**
 * Query to get organization by ID
 */
@Data
@Builder
public class GetOrganizationByIdQuery implements Query<Organization> {
    
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