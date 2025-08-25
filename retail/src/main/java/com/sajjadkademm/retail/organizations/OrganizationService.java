package com.sajjadkademm.retail.organizations;

import com.sajjadkademm.retail.exceptions.BadRequestException;
import com.sajjadkademm.retail.exceptions.ConflictException;
import com.sajjadkademm.retail.exceptions.NotFoundException;
import com.sajjadkademm.retail.exceptions.UnauthorizedException;
import com.sajjadkademm.retail.organizations.dto.CreateOrganizationRequest;
import com.sajjadkademm.retail.organizations.dto.OrganizationStatus;
import com.sajjadkademm.retail.organizations.dto.UpdateOrganizationRequest;
import com.sajjadkademm.retail.organizations.utils.OrganizationValidationUtils;
import com.sajjadkademm.retail.settings.system.service.SystemSettingsService;
import com.sajjadkademm.retail.users.User;
import com.sajjadkademm.retail.users.UserService;
import com.sajjadkademm.retail.users.dto.AccountType;
import com.sajjadkademm.retail.users.dto.UserStatus;
import com.sajjadkademm.retail.config.locales.LocalizedErrorService;
import com.sajjadkademm.retail.organizations.OrganizationErrorCode;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

@Service
public class OrganizationService {
    private final OrganizationRepository organizationRepository;
    private final SystemSettingsService systemSettingsService;
    private final UserService userService;
    private final LocalizedErrorService localizedErrorService;
    private final OrganizationValidationUtils organizationValidationUtils;

    @Autowired
    public OrganizationService(OrganizationRepository organizationRepository,
            SystemSettingsService systemSettingsService,
            UserService userService,
            LocalizedErrorService localizedErrorService,
            OrganizationValidationUtils organizationValidationUtils) {
        this.organizationRepository = organizationRepository;
        this.systemSettingsService = systemSettingsService;
        this.userService = userService;
        this.localizedErrorService = localizedErrorService;
        this.organizationValidationUtils = organizationValidationUtils;
    }

    /**
     * Create a new organization with default settings
     * If organization creation fails OR any settings creation fails, the entire
     * transaction will be rolled back
     */
    @Transactional(rollbackFor = { Exception.class })
    public Organization createOrganization(CreateOrganizationRequest request) {
        try {
            // Check if organization with same phone already exists
            if (organizationRepository.existsByPhone(request.getPhone())) {
                throw new ConflictException(localizedErrorService
                        .getLocalizedMessage(OrganizationErrorCode.ORGANIZATION_ALREADY_EXISTS.getMessage(),
                                request.getPhone()));
            }

            // Check if organization with same domain already exists
            if (organizationRepository.existsByDomain(request.getDomain())) {
                throw new ConflictException(localizedErrorService
                        .getLocalizedMessage(OrganizationErrorCode.ORGANIZATION_ALREADY_EXISTS.getMessage(),
                                request.getDomain()));
            }

            // Check if user exists
            User user = userService.getUserById(request.getUserId());
            if (user == null) {
                throw new NotFoundException(localizedErrorService
                        .getLocalizedMessage(OrganizationErrorCode.USER_NOT_FOUND.getMessage()));
            }

            if (user.getAccountType() != AccountType.USER) {
                throw new UnauthorizedException(localizedErrorService
                        .getLocalizedMessage(OrganizationErrorCode.USER_CANNOT_CREATE_ORGANIZATION.getMessage()));
            }

            if (user.getStatus() != UserStatus.ACTIVE) {
                throw new UnauthorizedException(localizedErrorService
                        .getLocalizedMessage(OrganizationErrorCode.USER_NOT_ACTIVE.getMessage()));
            }

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
            systemSettingsService.createAndSaveDefaultSystemSettings(savedOrganization.getId(), request.getUserId());

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

        // assert organization is active before updates
        if (organization.getStatus() == OrganizationStatus.REJECTED
                || organization.getStatus() == OrganizationStatus.SUSPENDED
                || organization.getStatus() == OrganizationStatus.DELETED) {
            throw new BadRequestException(localizedErrorService
                    .getLocalizedMessage(OrganizationErrorCode.ORGANIZATION_INACTIVE.getMessage()));
        }

        if (request.getPhone() != null && !request.getPhone().equals(organization.getPhone())) {
            if (organizationRepository.existsByPhone(request.getPhone())) {
                throw new ConflictException(localizedErrorService
                        .getLocalizedMessage(OrganizationErrorCode.ORGANIZATION_ALREADY_EXISTS.getMessage(),
                                request.getPhone()));
            }
        }

        if (request.getDomain() != null && !request.getDomain().equals(organization.getDomain())) {
            if (organizationRepository.existsByDomain(request.getDomain())) {
                throw new ConflictException(localizedErrorService
                        .getLocalizedMessage(OrganizationErrorCode.ORGANIZATION_ALREADY_EXISTS.getMessage(),
                                request.getDomain()));
            }
        }

        // Check if user exists
        User user = userService.getUserById(request.getUserId());
        if (user == null) {
            throw new NotFoundException(localizedErrorService
                    .getLocalizedMessage(OrganizationErrorCode.USER_NOT_FOUND.getMessage()));
        }

        if (user.getAccountType() != AccountType.USER) {
            throw new UnauthorizedException(localizedErrorService
                    .getLocalizedMessage(OrganizationErrorCode.USER_CANNOT_UPDATE_ORGANIZATION.getMessage()));
        }

        if (user.getStatus() != UserStatus.ACTIVE) {
            throw new UnauthorizedException(localizedErrorService
                    .getLocalizedMessage(OrganizationErrorCode.USER_NOT_ACTIVE.getMessage()));
        }

        if (user.getId() != organization.getCreatedBy().getId()) {
            throw new UnauthorizedException(localizedErrorService
                    .getLocalizedMessage(OrganizationErrorCode.USER_NOT_ORGANIZATION_CREATOR.getMessage()));
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
     * Get organization by ID
     */
    public Organization getOrganizationById(String id) {
        return organizationRepository.findById(id)
                .orElseThrow(() -> new NotFoundException(localizedErrorService
                        .getLocalizedMessage(OrganizationErrorCode.ORGANIZATION_NOT_FOUND.getMessage())));
    }

    /**
     * Get all organizations
     */
    public List<Organization> getAllOrganizations() {
        return organizationRepository.findAll();
    }

    /**
     * Search organizations
     */
    public List<Organization> searchOrganizations(String searchTerm) {
        if (searchTerm == null || searchTerm.trim().isEmpty()) {
            throw new BadRequestException(localizedErrorService
                    .getLocalizedMessage(OrganizationErrorCode.SEARCH_TERM_EMPTY.getMessage()));
        }

        return organizationRepository.searchOrganizations(searchTerm.trim());
    }

    /**
     * Check if organization exists by domain
     */
    public boolean organizationExistsByDomain(String domain) {
        return organizationRepository.existsByDomain(domain);
    }
}