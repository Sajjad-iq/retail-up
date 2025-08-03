package com.sajjadkademm.retail.organizations;

import com.sajjadkademm.retail.exceptions.BadRequestException;
import com.sajjadkademm.retail.exceptions.ConflictException;
import com.sajjadkademm.retail.exceptions.NotFoundException;
import com.sajjadkademm.retail.organizations.dto.CreateOrganizationRequest;
import com.sajjadkademm.retail.organizations.dto.UpdateOrganizationRequest;
import com.sajjadkademm.retail.settings.inventory.service.InventorySettingsService;
import com.sajjadkademm.retail.settings.pos.service.POSSettingsService;
import com.sajjadkademm.retail.settings.system.service.SystemSettingsService;
import com.sajjadkademm.retail.users.User;
import com.sajjadkademm.retail.users.UserService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

@Service
public class OrganizationService {
    private final OrganizationRepository organizationRepository;
    private final InventorySettingsService inventorySettingsService;
    private final POSSettingsService posSettingsService;
    private final SystemSettingsService systemSettingsService;
    private final UserService userService;

    @Autowired
    public OrganizationService(OrganizationRepository organizationRepository,
            InventorySettingsService inventorySettingsService,
            POSSettingsService posSettingsService,
            SystemSettingsService systemSettingsService,
            UserService userService) {
        this.organizationRepository = organizationRepository;
        this.inventorySettingsService = inventorySettingsService;
        this.posSettingsService = posSettingsService;
        this.systemSettingsService = systemSettingsService;
        this.userService = userService;
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
                throw new ConflictException("Organization with phone " + request.getPhone() + " already exists");
            }

            // Check if organization with same domain already exists
            if (organizationRepository.existsByDomain(request.getDomain())) {
                throw new ConflictException("Organization with domain " + request.getDomain() + " already exists");
            }

            // Check if user exists
            User user = userService.getUserById(request.getUserId());
            if (user == null) {
                throw new NotFoundException("User not found with ID: " + request.getUserId());
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
            // If any of these fail, the entire transaction will be rolled back
            inventorySettingsService.createAndSaveDefaultInventorySettings(savedOrganization.getId(),
                    request.getUserId());
            posSettingsService.createAndSaveDefaultPOSSettings(savedOrganization.getId(), request.getUserId());
            systemSettingsService.createAndSaveDefaultSystemSettings(savedOrganization.getId(), request.getUserId());

            return savedOrganization;

        } catch (ConflictException e) {
            throw e;
        } catch (BadRequestException e) {
            throw e;
        } catch (Exception e) {
            throw new BadRequestException("Failed to create organization: " + e.getMessage(), e);
        }
    }

    /**
     * Update an existing organization
     */
    public Organization updateOrganization(String id, UpdateOrganizationRequest request) {
        Organization organization = organizationRepository.findById(id)
                .orElseThrow(() -> new NotFoundException("Organization not found with ID: " + id));

        // Update organization fields
        organization.setName(request.getName());
        organization.setDescription(request.getDescription());
        organization.setAddress(request.getAddress());

        return organizationRepository.save(organization);
    }

    /**
     * Get organization by ID
     */
    public Organization getOrganizationById(String id) {
        return organizationRepository.findById(id)
                .orElseThrow(() -> new NotFoundException("Organization not found with ID: " + id));
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
            throw new BadRequestException("Search term cannot be empty");
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