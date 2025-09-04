package com.sajjadkademm.retail.application.services.organizations;

import com.sajjadkademm.retail.domain.organization.model.Organization;
import com.sajjadkademm.retail.application.dto.organizations.CreateOrganizationRequest;
import com.sajjadkademm.retail.application.dto.organizations.UpdateOrganizationRequest;
import com.sajjadkademm.retail.domain.organization.commands.CreateOrganizationCommand;
import com.sajjadkademm.retail.domain.organization.commands.UpdateOrganizationCommand;
import com.sajjadkademm.retail.domain.organization.queries.GetOrganizationByIdQuery;
import com.sajjadkademm.retail.domain.organization.queries.GetUserOrganizationsQuery;
import com.sajjadkademm.retail.domain.organization.queries.SearchUserOrganizationsQuery;
import com.sajjadkademm.retail.shared.cqrs.CommandBus;
import com.sajjadkademm.retail.shared.cqrs.QueryBus;
import com.sajjadkademm.retail.application.config.security.SecurityUtils;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.util.List;

/**
 * Service class for organization operations.
 * Handles business logic for organization management.
 * 
 * @author Sajjad Kadem
 * @version 1.0
 * @since 2024-12-19
 */
@Slf4j
@Service
@RequiredArgsConstructor
public class OrganizationService {

    private final CommandBus commandBus;
    private final QueryBus queryBus;

    /**
     * Create a new organization for the current authenticated user
     * 
     * @param request Organization creation request
     * @return Created organization
     * @throws Exception if creation fails
     */
    public Organization createOrganization(CreateOrganizationRequest request) throws Exception {
        log.debug("Creating organization: {} for user: {}", request.getName(), SecurityUtils.getCurrentUserId());
        
        CreateOrganizationCommand command = CreateOrganizationCommand.builder()
                .userId(SecurityUtils.getCurrentUserId())
                .request(request)
                .build();
        
        return commandBus.execute(command);
    }

    /**
     * Update an existing organization
     * 
     * @param id Organization ID
     * @param request Organization update request
     * @return Updated organization
     * @throws Exception if update fails
     */
    public Organization updateOrganization(String id, UpdateOrganizationRequest request) throws Exception {
        log.debug("Updating organization: {} for user: {}", id, SecurityUtils.getCurrentUserId());
        
        UpdateOrganizationCommand command = UpdateOrganizationCommand.builder()
                .userId(SecurityUtils.getCurrentUserId())
                .organizationId(id)
                .request(request)
                .build();
        
        return commandBus.execute(command);
    }

    /**
     * Get organization by ID
     * 
     * @param id Organization ID
     * @return Organization details
     * @throws Exception if retrieval fails
     */
    public Organization getOrganizationById(String id) throws Exception {
        log.debug("Getting organization by ID: {} for user: {}", id, SecurityUtils.getCurrentUserId());
        
        GetOrganizationByIdQuery query = GetOrganizationByIdQuery.builder()
                .userId(SecurityUtils.getCurrentUserId())
                .organizationId(id)
                .build();
        
        return queryBus.execute(query);
    }

    /**
     * Get all organizations for the current authenticated user
     * 
     * @return List of user's organizations
     * @throws Exception if retrieval fails
     */
    public List<Organization> getUserOrganizations() throws Exception {
        log.debug("Getting organizations for user: {}", SecurityUtils.getCurrentUserId());
        
        GetUserOrganizationsQuery query = GetUserOrganizationsQuery.builder()
                .userId(SecurityUtils.getCurrentUserId())
                .build();
        
        return queryBus.execute(query);
    }

    /**
     * Search organizations for the current authenticated user
     * 
     * @param searchTerm Search query for organization name or domain
     * @return List of matching organizations
     * @throws Exception if search fails
     */
    public List<Organization> searchUserOrganizations(String searchTerm) throws Exception {
        log.debug("Searching organizations for user: {} with term: {}", SecurityUtils.getCurrentUserId(), searchTerm);
        
        SearchUserOrganizationsQuery query = SearchUserOrganizationsQuery.builder()
                .userId(SecurityUtils.getCurrentUserId())
                .searchTerm(searchTerm)
                .build();
        
        return queryBus.execute(query);
    }
}