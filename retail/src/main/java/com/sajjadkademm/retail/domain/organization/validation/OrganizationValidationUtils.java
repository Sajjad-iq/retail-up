package com.sajjadkademm.retail.domain.organization.validation;

import com.sajjadkademm.retail.application.config.security.SecurityUtils;
import com.sajjadkademm.retail.shared.localization.LocalizedErrorService;
import com.sajjadkademm.retail.shared.localization.errorCode.OrganizationErrorCode;
import com.sajjadkademm.retail.shared.localization.errorCode.UserErrorCode;
import com.sajjadkademm.retail.shared.common.exceptions.UnauthorizedException;
import com.sajjadkademm.retail.shared.common.exceptions.BadRequestException;
import com.sajjadkademm.retail.domain.organization.model.Organization;
import com.sajjadkademm.retail.domain.organization.repositories.OrganizationRepository;
import com.sajjadkademm.retail.domain.auth.model.User;
import com.sajjadkademm.retail.domain.auth.validation.UserValidator;
import com.sajjadkademm.retail.application.dto.organizations.CreateOrganizationRequest;
import com.sajjadkademm.retail.application.dto.organizations.UpdateOrganizationRequest;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

/**
 * Validation utilities for organization operations
 */
@Component
@RequiredArgsConstructor
public class OrganizationValidationUtils {

    private final OrganizationRepository organizationRepository;
    private final LocalizedErrorService localizedErrorService;
    private final UserValidator userValidator;

    /**
     * Validate create organization request and user permissions
     */
    public void validateCreateRequest(CreateOrganizationRequest request, String userId) {
        // Validate user is active
        User user = userValidator.validateUserActive(userId);
        
        // Validate unique name
        if (organizationRepository.existsByName(request.getName())) {
            throw new BadRequestException(localizedErrorService
                    .getLocalizedMessage(OrganizationErrorCode.ORGANIZATION_NAME_DUPLICATE.getMessage(), request.getName()));
        }
        
        // Validate unique domain if provided
        if (request.getDomain() != null && !request.getDomain().trim().isEmpty()) {
            if (organizationRepository.existsByDomain(request.getDomain())) {
                throw new BadRequestException(localizedErrorService
                        .getLocalizedMessage(OrganizationErrorCode.ORGANIZATION_DOMAIN_DUPLICATE.getMessage(), request.getDomain()));
            }
        }
    }

    /**
     * Validate update organization request
     */
    public void validateUpdateRequest(UpdateOrganizationRequest request, Organization existingOrganization) {
        // If name is being changed, validate uniqueness
        if (request.getName() != null && !request.getName().equals(existingOrganization.getName())) {
            if (organizationRepository.existsByName(request.getName())) {
                throw new BadRequestException(localizedErrorService
                        .getLocalizedMessage(OrganizationErrorCode.ORGANIZATION_NAME_DUPLICATE.getMessage(), request.getName()));
            }
        }
        
        // If domain is being changed, validate uniqueness
        if (request.getDomain() != null && !request.getDomain().equals(existingOrganization.getDomain())) {
            if (organizationRepository.existsByDomain(request.getDomain())) {
                throw new BadRequestException(localizedErrorService
                        .getLocalizedMessage(OrganizationErrorCode.ORGANIZATION_DOMAIN_DUPLICATE.getMessage(), request.getDomain()));
            }
        }
    }

    /**
     * Validate user has access to organization (must be creator)
     */
    public void validateUserAccess(Organization organization, String userId) {
        // Validate user is active
        User currentUser = userValidator.validateUserActive(userId);

        // Check if user is organization creator
        if (!currentUser.getId().equals(organization.getCreatedBy().getId())) {
            throw new UnauthorizedException(localizedErrorService
                    .getLocalizedMessage(UserErrorCode.USER_NOT_ORGANIZATION_CREATOR.getMessage()));
        }
    }

    /**
     * Validate user has access to organization by ID
     */
    public void validateUserAccessById(String organizationId, String userId) {
        // Find the organization first
        Organization organization = organizationRepository.findById(organizationId)
                .orElseThrow(() -> new BadRequestException(localizedErrorService
                        .getLocalizedMessage(OrganizationErrorCode.ORGANIZATION_NOT_FOUND.getMessage())));
        
        // Validate access
        validateUserAccess(organization, userId);
    }
}