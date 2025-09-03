package com.sajjadkademm.retail.domain.organization.queries;

import com.sajjadkademm.retail.shared.cqrs.Query;
import com.sajjadkademm.retail.domain.organization.model.Organization;

import lombok.Builder;
import lombok.Data;
import java.util.List;

/**
 * Query to get all organizations for the current user
 */
@Data
@Builder
public class GetUserOrganizationsQuery implements Query<List<Organization>> {
    
    private final String userId;

    @Override
    public String getUserId() {
        return userId;
    }

    @Override
    public void validate() {
        if (userId == null || userId.trim().isEmpty()) {
            throw new IllegalArgumentException("User ID is required");
        }
    }
}