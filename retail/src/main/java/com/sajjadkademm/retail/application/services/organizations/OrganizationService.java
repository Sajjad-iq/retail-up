package com.sajjadkademm.retail.application.services.organizations;

import com.sajjadkademm.retail.domain.organization.model.Organization;
import com.sajjadkademm.retail.domain.organization.repositories.OrganizationRepository;
import com.sajjadkademm.retail.shared.localization.errorCode.OrganizationErrorCode;
import com.sajjadkademm.retail.shared.localization.errorCode.UserErrorCode;
import com.sajjadkademm.retail.shared.common.exceptions.BadRequestException;
import com.sajjadkademm.retail.shared.common.exceptions.ConflictException;
import com.sajjadkademm.retail.shared.common.exceptions.NotFoundException;
import com.sajjadkademm.retail.application.dto.organizations.CreateOrganizationRequest;
import com.sajjadkademm.retail.application.dto.organizations.UpdateOrganizationRequest;
import com.sajjadkademm.retail.application.services.settings.SystemSettingsService;
import com.sajjadkademm.retail.domain.auth.model.User;
import com.sajjadkademm.retail.shared.localization.LocalizedErrorService;
import com.sajjadkademm.retail.application.config.security.SecurityUtils;
import com.sajjadkademm.retail.domain.auth.validation.UserValidator;
import com.sajjadkademm.retail.domain.organization.validation.OrganizationValidator;
import com.sajjadkademm.retail.shared.enums.AccountType;
import com.sajjadkademm.retail.application.validation.OrganizationValidationService;
import com.sajjadkademm.retail.application.services.users.UserService;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import lombok.RequiredArgsConstructor;

import java.util.List;

@Service
@RequiredArgsConstructor
public class OrganizationService {
    private final OrganizationRepository organizationRepository;
    private final SystemSettingsService systemSettingsService;
    private final UserService userService;
    private final LocalizedErrorService localizedErrorService;
    private final UserValidator userValidator;
    private final OrganizationValidationService organizationValidationService;

    /**
     * Create a new organization with default settings
     * If organization creation fails OR any settings creation fails, the entire
     * transaction will be rolled back
     */
    @Transactional(rollbackFor = { Exception.class })
    public Organization createOrganization(CreateOrganizationRequest request) {
        try {
            // Get current authenticated user
            User sessionUser = SecurityUtils.getCurrentUser();

            // check if the user is active
            User user = userValidator.validateUserActive(sessionUser.getId());

            // Validate current user permissions
            userValidator.assertUserAccountType(user, AccountType.USER);

            // Validate organization data
            organizationValidationService.validateOrganizationCreationData(request);

            Organization organization = Organization.builder()
                    .name(request.getName())
                    .domain(request.getDomain())
                    .description(request.getDescription())
                    .address(request.getAddress())
                    .phone(request.getPhone())
                    .createdBy(user)
                    .build();

            // Save the organization first
            Organization savedOrganization = organizationRepository.save(organization);

            // Create and save default settings for the organization
            // If this fails, the entire transaction will be rolled back
            systemSettingsService.createAndSaveDefaultSystemSettings(savedOrganization.getId(), user.getId());

            return savedOrganization;

        } catch (ConflictException e) {
            throw e;
        } catch (BadRequestException e) {
            throw e;
        } catch (Exception e) {
            throw new BadRequestException(localizedErrorService
                    .getLocalizedMessage(OrganizationErrorCode.ORGANIZATION_CREATION_FAILED.getMessage()) + ": "
                    + e.getMessage(), e);
        }
    }

    /**
     * Update an existing organization
     */
    public Organization updateOrganization(String id, UpdateOrganizationRequest request) {
        Organization organization = organizationRepository.findById(id)
                .orElseThrow(() -> new NotFoundException(localizedErrorService
                        .getLocalizedMessage(OrganizationErrorCode.ORGANIZATION_NOT_FOUND.getMessage())));

        // Get current authenticated user
        User currentUser = SecurityUtils.getCurrentUser();
        User user = userValidator.validateUserActive(currentUser.getId());

        // Validate current user permissions
        if (!user.getId().equals(organization.getCreatedBy().getId())) {
            throw new BadRequestException(localizedErrorService
                    .getLocalizedMessage(UserErrorCode.USER_NOT_ORGANIZATION_CREATOR.getMessage()));
        }

        // Validate organization can be updated and not disabled
        organizationValidationService.assertOrganizationIsNotDisabledBySystem(organization);

        // Validate update request data
        organizationValidationService.validateOrganizationUpdateData(request, organization);

        // Update organization fields
        if (request.getName() != null) {
            organization.setName(request.getName());
        }
        if (request.getDescription() != null) {
            organization.setDescription(request.getDescription());
        }
        if (request.getAddress() != null) {
            organization.setAddress(request.getAddress());
        }
        if (request.getPhone() != null) {
            organization.setPhone(request.getPhone());
        }
        if (request.getDomain() != null) {
            organization.setDomain(request.getDomain());
        }
        if (request.getStatus() != null) {
            organization.setStatus(request.getStatus());
        }

        return organizationRepository.save(organization);
    }

    /**
     * Get organization by ID (only for organization creators or admins)
     */
    public Organization getOrganizationById(String id) {
        Organization organization = organizationRepository.findById(id)
                .orElseThrow(() -> new NotFoundException(localizedErrorService
                        .getLocalizedMessage(OrganizationErrorCode.ORGANIZATION_NOT_FOUND.getMessage())));

        // Get current authenticated user
        User currentUser = SecurityUtils.getCurrentUser();

        // Check if user is the creator of the organization or has admin privileges
        if (!currentUser.getId().equals(organization.getCreatedBy().getId())) {
            throw new BadRequestException(localizedErrorService
                    .getLocalizedMessage(UserErrorCode.USER_NOT_ORGANIZATION_CREATOR.getMessage()));
        }

        return organization;
    }

    /**
     * Get all organizations for the current user
     */
    public List<Organization> getAllOrganizations() {
        User currentUser = SecurityUtils.getCurrentUser();
        User user = userService.getUserById(currentUser.getId());
        return organizationRepository.findByCreatedBy(user);
    }

    /**
     * Search organizations for the current user
     */
    public List<Organization> searchOrganizations(String searchTerm) {
        // Validate search term
        if (searchTerm == null || searchTerm.trim().isEmpty()) {
            throw new BadRequestException(localizedErrorService
                    .getLocalizedMessage(OrganizationErrorCode.SEARCH_TERM_EMPTY.getMessage()));
        }

        User currentUser = SecurityUtils.getCurrentUser();
        return organizationRepository.searchOrganizationsByUser(searchTerm.trim(), currentUser.getId());
    }
}