package com.sajjadkademm.retail.domain.organization.commands;

import com.sajjadkademm.retail.shared.cqrs.Command;
import com.sajjadkademm.retail.application.dto.organizations.UpdateOrganizationRequest;
import com.sajjadkademm.retail.domain.organization.model.Organization;

import lombok.Builder;
import lombok.Data;

/**
 * Command to update an existing organization
 */
@Data
@Builder
public class UpdateOrganizationCommand implements Command<Organization> {
    
    private final String userId;
    private final String organizationId;
    private final UpdateOrganizationRequest request;

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
        if (request == null) {
            throw new IllegalArgumentException("Request is required");
        }
    }
}