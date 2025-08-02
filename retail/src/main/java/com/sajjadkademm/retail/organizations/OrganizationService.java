package com.sajjadkademm.retail.organizations;

import com.sajjadkademm.retail.exceptions.BadRequestException;
import com.sajjadkademm.retail.exceptions.ConflictException;
import com.sajjadkademm.retail.exceptions.NotFoundException;
import com.sajjadkademm.retail.organizations.dto.CreateOrganizationRequest;
import com.sajjadkademm.retail.organizations.dto.UpdateOrganizationRequest;
import com.sajjadkademm.retail.organizations.dto.OrganizationResponse;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.stream.Collectors;

@Service
public class OrganizationService {
    private final OrganizationRepository organizationRepository;

    @Autowired
    public OrganizationService(OrganizationRepository organizationRepository) {
        this.organizationRepository = organizationRepository;
    }

    /**
     * Create a new organization
     */
    public OrganizationResponse createOrganization(CreateOrganizationRequest request, String createdBy) {
        // Check if organization with same name already exists
        if (organizationRepository.existsByName(request.getName())) {
            throw new ConflictException("Organization with name " + request.getName() + " already exists");
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
                .email(request.getEmail())
                .createdBy(createdBy)
                .build();

        Organization savedOrganization = organizationRepository.save(organization);
        return mapToResponse(savedOrganization);
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
                .email(organization.getEmail())
                .createdAt(organization.getCreatedAt())
                .updatedAt(organization.getUpdatedAt())
                .createdBy(organization.getCreatedBy())
                .build();
    }

}