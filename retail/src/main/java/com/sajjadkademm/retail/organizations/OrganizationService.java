package com.sajjadkademm.retail.organizations;

import com.sajjadkademm.retail.config.locales.errorCode.OrganizationErrorCode;
import com.sajjadkademm.retail.exceptions.BadRequestException;
import com.sajjadkademm.retail.exceptions.ConflictException;
import com.sajjadkademm.retail.exceptions.NotFoundException;
import com.sajjadkademm.retail.organizations.dto.CreateOrganizationRequest;
import com.sajjadkademm.retail.organizations.dto.UpdateOrganizationRequest;
import com.sajjadkademm.retail.organizations.validator.OrganizationValidationUtils;
import com.sajjadkademm.retail.settings.system.service.SystemSettingsService;
import com.sajjadkademm.retail.users.User;
import com.sajjadkademm.retail.users.UserService;
import com.sajjadkademm.retail.config.locales.LocalizedErrorService;
import com.sajjadkademm.retail.config.SecurityUtils;

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
    private final OrganizationValidationUtils organizationValidationUtils;

    /**
     * Create a new organization with default settings
     * If organization creation fails OR any settings creation fails, the entire
     * transaction will be rolled back
     */
    @Transactional(rollbackFor = { Exception.class })
    public Organization createOrganization(CreateOrganizationRequest request) {
        try {
            // Get current authenticated user
            User currentUser = SecurityUtils.getCurrentUser();

            // Validate current user permissions
            organizationValidationUtils.validateUserCanCreateOrganization(currentUser);

            // Validate organization data
            organizationValidationUtils.validateOrganizationCreationData(
                    request.getName(),
                    request.getDomain(),
                    request.getDescription(),
                    request.getAddress(),
                    request.getPhone(),
                    request.getEmail());

            // Check if organization with same phone already exists
            organizationValidationUtils.validatePhoneUniqueness(request.getPhone());

            // Check if organization with same domain already exists
            organizationValidationUtils.validateDomainUniqueness(request.getDomain());

            Organization organization = Organization.builder()
                    .name(request.getName())
                    .domain(request.getDomain())
                    .description(request.getDescription())
                    .address(request.getAddress())
                    .phone(request.getPhone())
                    .createdBy(currentUser)
                    .build();

            // Save the organization first
            Organization savedOrganization = organizationRepository.save(organization);

            // Create and save default settings for the organization
            // If this fails, the entire transaction will be rolled back
            systemSettingsService.createAndSaveDefaultSystemSettings(savedOrganization.getId(), currentUser.getId());

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
        organizationValidationUtils.validateOrganizationCanBeUpdated(organization);

        // Validate organization data
        organizationValidationUtils.validateOrganizationUpdateData(
                request.getName(),
                request.getDomain(),
                request.getDescription(),
                request.getAddress(),
                request.getPhone());

        // Validate phone and domain uniqueness for updates
        organizationValidationUtils.validatePhoneUniquenessForUpdate(request.getPhone(), organization.getPhone());
        organizationValidationUtils.validateDomainUniquenessForUpdate(request.getDomain(), organization.getDomain());

        // Get current authenticated user
        User currentUser = SecurityUtils.getCurrentUser();

        // Validate current user permissions
        organizationValidationUtils.validateUserCanUpdateOrganization(currentUser, organization);

        // Validate status transition if status is being updated
        if (request.getStatus() != null && !request.getStatus().equals(organization.getStatus())) {
            organizationValidationUtils.validateStatusTransition(organization.getStatus(), request.getStatus());
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
        organizationValidationUtils.validateUserCanAccessOrganization(currentUser, organization);

        return organization;
    }

    /**
     * Get all organizations for the current user
     */
    public List<Organization> getAllOrganizations() {
        User currentUser = SecurityUtils.getCurrentUser();
        return organizationRepository.findByCreatedBy(currentUser);
    }

    /**
     * Search organizations for the current user
     */
    public List<Organization> searchOrganizations(String searchTerm) {
        // Validate search term
        organizationValidationUtils.validateSearchTerm(searchTerm);

        User currentUser = SecurityUtils.getCurrentUser();
        return organizationRepository.searchOrganizationsByUser(searchTerm.trim(), currentUser.getId());
    }

    /**
     * Check if organization exists by domain
     */
    public boolean organizationExistsByDomain(String domain) {
        return organizationRepository.existsByDomain(domain);
    }
}