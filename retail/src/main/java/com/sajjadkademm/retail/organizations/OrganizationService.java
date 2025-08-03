package com.sajjadkademm.retail.organizations;

import com.sajjadkademm.retail.exceptions.BadRequestException;
import com.sajjadkademm.retail.exceptions.ConflictException;
import com.sajjadkademm.retail.exceptions.NotFoundException;
import com.sajjadkademm.retail.organizations.dto.CreateOrganizationRequest;
import com.sajjadkademm.retail.organizations.dto.UpdateOrganizationRequest;
import com.sajjadkademm.retail.organizations.dto.OrganizationResponse;
import com.sajjadkademm.retail.settings.inventory.service.InventorySettingsService;
import com.sajjadkademm.retail.settings.pos.service.POSSettingsService;
import com.sajjadkademm.retail.settings.system.service.SystemSettingsService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.stream.Collectors;

@Service
public class OrganizationService {
    private final OrganizationRepository organizationRepository;
    private final InventorySettingsService inventorySettingsService;
    private final POSSettingsService posSettingsService;
    private final SystemSettingsService systemSettingsService;

    @Autowired
    public OrganizationService(OrganizationRepository organizationRepository,
            InventorySettingsService inventorySettingsService,
            POSSettingsService posSettingsService,
            SystemSettingsService systemSettingsService) {
        this.organizationRepository = organizationRepository;
        this.inventorySettingsService = inventorySettingsService;
        this.posSettingsService = posSettingsService;
        this.systemSettingsService = systemSettingsService;
    }

    /**
     * Create a new organization with default settings
     * If organization creation fails OR any settings creation fails, the entire
     * transaction will be rolled back
     */
    @Transactional(rollbackFor = { Exception.class })
    public OrganizationResponse createOrganization(CreateOrganizationRequest request, String createdBy) {
        try {
            // Check if organization with same phone already exists
            if (organizationRepository.existsByPhone(request.getPhone())) {
                throw new ConflictException("Organization with phone " + request.getPhone() + " already exists");
            }

            // Check if organization with same domain already exists
            if (organizationRepository.existsByDomain(request.getDomain())) {
                throw new ConflictException("Organization with domain " + request.getDomain() + " already exists");
            }

            Organization organization = Organization.builder()
                    .name(request.getName())
                    .domain(request.getDomain())
                    .description(request.getDescription())
                    .address(request.getAddress())
                    .phone(request.getPhone())
                    .createdBy(createdBy)
                    .build();

            // Save the organization first
            Organization savedOrganization = organizationRepository.save(organization);

            // Create and save default settings for the organization
            // If any of these fail, the entire transaction will be rolled back
            inventorySettingsService.createAndSaveDefaultInventorySettings(savedOrganization.getId(), createdBy);
            posSettingsService.createAndSaveDefaultPOSSettings(savedOrganization.getId(), createdBy);
            systemSettingsService.createAndSaveDefaultSystemSettings(savedOrganization.getId(), createdBy);

            return mapToResponse(savedOrganization);

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
    public OrganizationResponse updateOrganization(String id, UpdateOrganizationRequest request) {
        Organization organization = organizationRepository.findById(id)
                .orElseThrow(() -> new NotFoundException("Organization not found with ID: " + id));

        // Update organization fields
        organization.setName(request.getName());
        organization.setDescription(request.getDescription());
        organization.setAddress(request.getAddress());

        Organization updatedOrganization = organizationRepository.save(organization);
        return mapToResponse(updatedOrganization);
    }

    /**
     * Get organization by ID
     */
    public OrganizationResponse getOrganizationById(String id) {
        Organization organization = organizationRepository.findById(id)
                .orElseThrow(() -> new NotFoundException("Organization not found with ID: " + id));

        return mapToResponse(organization);
    }

    /**
     * Get all organizations
     */
    public List<OrganizationResponse> getAllOrganizations() {
        return organizationRepository.findAll()
                .stream()
                .map(this::mapToResponse)
                .collect(Collectors.toList());
    }

    /**
     * Search organizations
     */
    public List<OrganizationResponse> searchOrganizations(String searchTerm) {
        if (searchTerm == null || searchTerm.trim().isEmpty()) {
            throw new BadRequestException("Search term cannot be empty");
        }

        return organizationRepository.searchOrganizations(searchTerm.trim())
                .stream()
                .map(this::mapToResponse)
                .collect(Collectors.toList());
    }

    /**
     * Check if organization exists by domain
     */
    public boolean organizationExistsByDomain(String domain) {
        return organizationRepository.existsByDomain(domain);
    }

    /**
     * Map Organization entity to OrganizationResponse DTO
     */
    private OrganizationResponse mapToResponse(Organization organization) {
        return OrganizationResponse.builder()
                .id(organization.getId())
                .name(organization.getName())
                .domain(organization.getDomain())
                .description(organization.getDescription())
                .address(organization.getAddress())
                .phone(organization.getPhone())
                .createdBy(organization.getCreatedBy())
                .build();
    }
}