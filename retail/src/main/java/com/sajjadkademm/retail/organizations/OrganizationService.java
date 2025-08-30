package com.sajjadkademm.retail.organizations;

import com.sajjadkademm.retail.config.locales.errorCode.OrganizationErrorCode;
import com.sajjadkademm.retail.config.locales.errorCode.UserErrorCode;
import com.sajjadkademm.retail.exceptions.BadRequestException;
import com.sajjadkademm.retail.exceptions.ConflictException;
import com.sajjadkademm.retail.exceptions.NotFoundException;
import com.sajjadkademm.retail.organizations.dto.CreateOrganizationRequest;
import com.sajjadkademm.retail.organizations.dto.UpdateOrganizationRequest;
import com.sajjadkademm.retail.settings.system.service.SystemSettingsService;
import com.sajjadkademm.retail.users.User;
import com.sajjadkademm.retail.config.locales.LocalizedErrorService;
import com.sajjadkademm.retail.config.SecurityUtils;
import com.sajjadkademm.retail.shared.validators.UserValidator;
import com.sajjadkademm.retail.shared.validators.OrganizationValidator;
import com.sajjadkademm.retail.shared.enums.AccountType;
import com.sajjadkademm.retail.organizations.validator.internalValidator;
import com.sajjadkademm.retail.users.UserService;
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
    private final OrganizationValidator organizationStatusValidator;
    private final internalValidator internalValidator;

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
            internalValidator.validateOrganizationCreationData(request);

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

        // Validate organization can be updated
        organizationStatusValidator.assertOrganizationIsNotDisabledBySystem(organization);

        // Validate organization data
        internalValidator.validateOrganizationUpdateData(request, organization);

        // Get current authenticated user
        User currentUser = SecurityUtils.getCurrentUser();
        User user = userValidator.validateUserActive(currentUser.getId());

        // Validate current user permissions
        if (!user.getId().equals(organization.getCreatedBy().getId())) {
            throw new BadRequestException(localizedErrorService
                    .getLocalizedMessage(UserErrorCode.USER_NOT_ORGANIZATION_CREATOR.getMessage()));
        }

        // Update organization fields
        organization.setName(request.getName());
        organization.setDescription(request.getDescription());
        organization.setAddress(request.getAddress());
        organization.setPhone(request.getPhone());
        organization.setDomain(request.getDomain());
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