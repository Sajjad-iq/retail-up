package com.sajjadkademm.retail.domain.organization.services;

import com.sajjadkademm.retail.domain.organization.model.Organization;
import com.sajjadkademm.retail.domain.organization.repositories.OrganizationRepository;
import com.sajjadkademm.retail.shared.common.exceptions.NotFoundException;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

/**
 * Domain service for organization-specific business logic.
 * This service contains core business rules that don't belong in entities.
 * Unlike application services, domain services focus purely on business logic
 * without orchestration concerns.
 */
@Service
@RequiredArgsConstructor
public class OrganizationDomainService {

    private final OrganizationRepository organizationRepository;

    /**
     * Get organization by ID with domain validation
     */
    public Organization getOrganizationById(String id) {
        return organizationRepository.findById(id)
                .orElseThrow(() -> new NotFoundException("Organization not found with id: " + id));
    }
}