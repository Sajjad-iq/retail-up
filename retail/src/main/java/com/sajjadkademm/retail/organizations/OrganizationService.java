package com.sajjadkademm.retail.organizations;

import com.sajjadkademm.retail.config.locales.errorCode.OrganizationErrorCode;
import com.sajjadkademm.retail.config.locales.errorCode.AuthErrorCode;
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
import com.sajjadkademm.retail.shared.validators.PhoneValidator;
import com.sajjadkademm.retail.shared.validators.DomainValidator;
import com.sajjadkademm.retail.shared.validators.EmailValidator;
import com.sajjadkademm.retail.shared.validators.OrganizationStatusValidator;
import com.sajjadkademm.retail.shared.enums.AccountType;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import lombok.RequiredArgsConstructor;

import java.util.List;

@Service
@RequiredArgsConstructor
public class OrganizationService {
    private final OrganizationRepository organizationRepository;
    private final SystemSettingsService systemSettingsService;
    private final LocalizedErrorService localizedErrorService;
    private final UserValidator userValidator;
    private final PhoneValidator phoneValidator;
    private final DomainValidator domainValidator;
    private final EmailValidator emailValidator;
    private final OrganizationStatusValidator organizationStatusValidator;

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

            User user = userValidator.validateUserActive(sessionUser.getId());

            // Validate current user permissions
            validateUserCanCreateOrganization(user);

            // Validate organization data
            validateOrganizationCreationData(
                    request.getName(),
                    request.getDomain(),
                    request.getDescription(),
                    request.getAddress(),
                    request.getPhone(),
                    request.getEmail());

            // Check if organization with same phone already exists
            validatePhoneUniqueness(request.getPhone());

            // Check if organization with same domain already exists
            validateDomainUniqueness(request.getDomain());

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
        validateOrganizationCanBeUpdated(organization);

        // Validate organization data
        validateOrganizationUpdateData(
                request.getName(),
                request.getDomain(),
                request.getDescription(),
                request.getAddress(),
                request.getPhone());

        // Validate phone and domain uniqueness for updates
        validatePhoneUniquenessForUpdate(request.getPhone(), organization.getPhone());
        validateDomainUniquenessForUpdate(request.getDomain(), organization.getDomain());

        // Get current authenticated user
        User currentUser = SecurityUtils.getCurrentUser();

        // Validate current user permissions
        validateUserCanUpdateOrganization(currentUser, organization);

        // Validate status transition if status is being updated
        if (request.getStatus() != null && !request.getStatus().equals(organization.getStatus())) {
            validateStatusTransition(organization.getStatus(), request.getStatus());
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
        validateUserCanAccessOrganization(currentUser, organization);

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
        validateSearchTerm(searchTerm);

        User currentUser = SecurityUtils.getCurrentUser();
        return organizationRepository.searchOrganizationsByUser(searchTerm.trim(), currentUser.getId());
    }

    /**
     * Check if organization exists by domain
     */
    public boolean organizationExistsByDomain(String domain) {
        return organizationRepository.existsByDomain(domain);
    }

    // Validation methods moved from OrganizationValidationUtils

    /**
     * Validates that the current user can create an organization.
     * Checks if user is of type USER and has ACTIVE status.
     */
    private void validateUserCanCreateOrganization(User currentUser) {
        userValidator.assertUserIsHasActiveStatus(currentUser);
        userValidator.assertUserAccountType(currentUser, AccountType.USER);
    }

    /**
     * Validates that the current user can update an organization.
     * Checks if user is of type USER, has ACTIVE status, and is the creator.
     */
    private void validateUserCanUpdateOrganization(User currentUser, Organization organization) {
        User user = userValidator.validateUserActive(currentUser.getId());

        if (!user.getId().equals(organization.getCreatedBy().getId())) {
            throw new BadRequestException(localizedErrorService
                    .getLocalizedMessage(UserErrorCode.USER_NOT_ORGANIZATION_CREATOR.getMessage()));
        }
    }

    /**
     * Validates that the current user can access an organization.
     * Checks if user is the creator of the organization.
     */
    private void validateUserCanAccessOrganization(User currentUser, Organization organization) {
        if (!currentUser.getId().equals(organization.getCreatedBy().getId())) {
            // TODO: Add admin role check here when role system is implemented
            throw new BadRequestException(localizedErrorService
                    .getLocalizedMessage(UserErrorCode.USER_NOT_ORGANIZATION_CREATOR.getMessage()));
        }
    }

    /**
     * Comprehensive validation for organization creation data.
     * Validates all organization fields including email for creation requests.
     */
    private void validateOrganizationCreationData(String name, String domain, String description,
            String address, String phone, String email) {
        // Validate organization name
        if (name == null || name.trim().isEmpty()) {
            throw new BadRequestException(localizedErrorService
                    .getLocalizedMessage(OrganizationErrorCode.ORGANIZATION_NAME_EMPTY.getMessage()));
        }
        if (name.trim().length() < 2 || name.trim().length() > 255) {
            throw new BadRequestException(localizedErrorService
                    .getLocalizedMessage(OrganizationErrorCode.ORGANIZATION_NAME_INVALID.getMessage()));
        }

        // Validate domain format
        domainValidator.validateDomainFormat(domain);

        // Validate description
        if (description != null && description.trim().length() > 500) {
            throw new BadRequestException(localizedErrorService
                    .getLocalizedMessage(OrganizationErrorCode.ORGANIZATION_DESCRIPTION_INVALID.getMessage()));
        }

        // Validate address
        if (address != null && address.trim().length() > 255) {
            throw new BadRequestException(localizedErrorService
                    .getLocalizedMessage(OrganizationErrorCode.ORGANIZATION_ADDRESS_INVALID.getMessage()));
        }

        // Validate phone format
        phoneValidator.validatePhoneFormat(phone);

        // Validate email format
        emailValidator.validateEmailFormat(email);
    }

    /**
     * Comprehensive validation for organization update data.
     * Validates all organization fields excluding email for update requests.
     */
    private void validateOrganizationUpdateData(String name, String domain, String description,
            String address, String phone) {
        // Validate organization name
        if (name == null || name.trim().isEmpty()) {
            throw new BadRequestException(localizedErrorService
                    .getLocalizedMessage(OrganizationErrorCode.ORGANIZATION_NAME_EMPTY.getMessage()));
        }
        if (name.trim().length() < 2 || name.trim().length() > 255) {
            throw new BadRequestException(localizedErrorService
                    .getLocalizedMessage(OrganizationErrorCode.ORGANIZATION_NAME_INVALID.getMessage()));
        }

        // Validate domain format
        domainValidator.validateDomainFormat(domain);

        // Validate description
        if (description != null && description.trim().length() > 500) {
            throw new BadRequestException(localizedErrorService
                    .getLocalizedMessage(OrganizationErrorCode.ORGANIZATION_DESCRIPTION_INVALID.getMessage()));
        }

        // Validate address
        if (address != null && address.trim().length() > 255) {
            throw new BadRequestException(localizedErrorService
                    .getLocalizedMessage(OrganizationErrorCode.ORGANIZATION_ADDRESS_INVALID.getMessage()));
        }

        // Validate phone format
        phoneValidator.validatePhoneFormat(phone);
    }

    /**
     * Validates phone uniqueness for organization creation.
     * Checks if organization with same phone already exists.
     */
    private void validatePhoneUniqueness(String phone) {
        if (organizationRepository.existsByPhone(phone)) {
            throw new ConflictException(localizedErrorService
                    .getLocalizedMessage(AuthErrorCode.AUTH_PHONE_ALREADY_EXISTS.getMessage(), phone));
        }
    }

    /**
     * Validates domain uniqueness for organization creation.
     * Checks if organization with same domain already exists.
     */
    private void validateDomainUniqueness(String domain) {
        if (organizationRepository.existsByDomain(domain)) {
            throw new ConflictException(localizedErrorService
                    .getLocalizedMessage(OrganizationErrorCode.DOMAIN_ALREADY_EXISTS.getMessage()));
        }
    }

    /**
     * Validates phone uniqueness when updating an organization.
     * Only checks if the new phone is different from the current one.
     */
    private void validatePhoneUniquenessForUpdate(String newPhone, String currentPhone) {
        if (!newPhone.equals(currentPhone)) {
            if (organizationRepository.existsByPhone(newPhone)) {
                throw new ConflictException(localizedErrorService
                        .getLocalizedMessage(AuthErrorCode.AUTH_PHONE_ALREADY_EXISTS.getMessage(), newPhone));
            }
        }
    }

    /**
     * Validates domain uniqueness when updating an organization.
     * Only checks if the new domain is different from the current one.
     */
    private void validateDomainUniquenessForUpdate(String newDomain, String currentDomain) {
        if (!newDomain.equals(currentDomain)) {
            if (organizationRepository.existsByDomain(newDomain)) {
                throw new ConflictException(localizedErrorService
                        .getLocalizedMessage(OrganizationErrorCode.DOMAIN_ALREADY_EXISTS.getMessage()));
            }
        }
    }

    /**
     * Validates that the search term is not empty or null.
     */
    private void validateSearchTerm(String searchTerm) {
        if (searchTerm == null || searchTerm.trim().isEmpty()) {
            throw new BadRequestException(localizedErrorService
                    .getLocalizedMessage(OrganizationErrorCode.SEARCH_TERM_EMPTY.getMessage()));
        }
    }

    /**
     * Validates that the organization can be updated.
     * Checks if the organization is in a state that allows updates.
     */
    private void validateOrganizationCanBeUpdated(Organization organization) {
        organizationStatusValidator.assertOrganizationCanBeUpdated(organization);
    }

    /**
     * Validates that the organization status transition is allowed.
     * Checks if the new status is valid for the current organization state.
     */
    private void validateStatusTransition(com.sajjadkademm.retail.shared.enums.OrganizationStatus currentStatus,
            com.sajjadkademm.retail.shared.enums.OrganizationStatus newStatus) {
        organizationStatusValidator.validateStatusTransition(currentStatus, newStatus);
    }
}