package com.sajjadkademm.retail.domain.organization.commands;

import com.sajjadkademm.retail.shared.cqrs.Command;
import com.sajjadkademm.retail.application.dto.organizations.CreateOrganizationRequest;
import com.sajjadkademm.retail.domain.organization.model.Organization;

import lombok.Builder;
import lombok.Data;

/**
 * Command to create a new organization
 */
@Data
@Builder
public class CreateOrganizationCommand implements Command<Organization> {
    
    private final String userId;
    private final CreateOrganizationRequest request;

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
            throw new IllegalArgumentException("Organization name is required");
        }
    }
}