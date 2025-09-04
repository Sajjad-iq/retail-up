package com.sajjadkademm.retail.application.services.inventory;

import com.sajjadkademm.retail.domain.inventory.model.Inventory;
import com.sajjadkademm.retail.application.dto.inventory.CreateInventoryRequest;
import com.sajjadkademm.retail.application.dto.inventory.UpdateInventoryRequest;
import com.sajjadkademm.retail.shared.cqrs.CommandBus;
import com.sajjadkademm.retail.shared.cqrs.QueryBus;
import com.sajjadkademm.retail.domain.inventory.commands.*;
import com.sajjadkademm.retail.domain.inventory.queries.*;
import com.sajjadkademm.retail.application.config.security.SecurityUtils;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.util.List;

/**
 * Service class for inventory operations.
 * Handles business logic for inventory management within organizations.
 * 
 * @author Sajjad Kadem
 * @version 1.0
 * @since 2024-12-19
 */
@Slf4j
@Service
@RequiredArgsConstructor
public class InventoryService {

    private final CommandBus commandBus;
    private final QueryBus queryBus;

    /**
     * Create a new inventory for an organization
     * 
     * @param request Inventory creation request
     * @return Created inventory
     * @throws Exception if creation fails
     */
    public Inventory createInventory(CreateInventoryRequest request) throws Exception {
        log.debug("Creating inventory: {} for organization: {}", request.getName(), request.getOrganizationId());
        
        CreateInventoryCommand command = CreateInventoryCommand.builder()
                .userId(SecurityUtils.getCurrentUserId())
                .request(request)
                .build();
        
        return commandBus.execute(command);
    }

    /**
     * Update an existing inventory
     * 
     * @param id Inventory ID
     * @param request Inventory update request
     * @return Updated inventory
     * @throws Exception if update fails
     */
    public Inventory updateInventory(String id, UpdateInventoryRequest request) throws Exception {
        log.debug("Updating inventory: {} for user: {}", id, SecurityUtils.getCurrentUserId());
        
        UpdateInventoryCommand command = UpdateInventoryCommand.builder()
                .userId(SecurityUtils.getCurrentUserId())
                .inventoryId(id)
                .request(request)
                .build();
        
        return commandBus.execute(command);
    }

    /**
     * Get inventory by ID
     * 
     * @param id Inventory ID
     * @return Inventory details
     * @throws Exception if retrieval fails
     */
    public Inventory getInventoryById(String id) throws Exception {
        log.debug("Getting inventory by ID: {} for user: {}", id, SecurityUtils.getCurrentUserId());
        
        GetInventoryByIdQuery query = GetInventoryByIdQuery.builder()
                .userId(SecurityUtils.getCurrentUserId())
                .inventoryId(id)
                .build();
        
        return queryBus.execute(query);
    }

    /**
     * Get all inventories for an organization
     * 
     * @param organizationId Organization ID
     * @return List of inventories
     * @throws Exception if retrieval fails
     */
    public List<Inventory> getInventoriesByOrganization(String organizationId) throws Exception {
        log.debug("Getting inventories for organization: {} by user: {}", organizationId, SecurityUtils.getCurrentUserId());
        
        GetInventoriesByOrganizationQuery query = GetInventoriesByOrganizationQuery.builder()
                .userId(SecurityUtils.getCurrentUserId())
                .organizationId(organizationId)
                .activeOnly(false) // Get all inventories
                .build();
        
        return queryBus.execute(query);
    }

    /**
     * Get active inventories for an organization
     * 
     * @param organizationId Organization ID
     * @return List of active inventories
     * @throws Exception if retrieval fails
     */
    public List<Inventory> getActiveInventoriesByOrganization(String organizationId) throws Exception {
        log.debug("Getting active inventories for organization: {} by user: {}", organizationId, SecurityUtils.getCurrentUserId());
        
        GetInventoriesByOrganizationQuery query = GetInventoriesByOrganizationQuery.builder()
                .userId(SecurityUtils.getCurrentUserId())
                .organizationId(organizationId)
                .activeOnly(true) // Get only active inventories
                .build();
        
        return queryBus.execute(query);
    }

    /**
     * Search inventories by name within an organization
     * 
     * @param organizationId Organization ID
     * @param searchTerm Search query for inventory name
     * @return List of matching inventories
     * @throws Exception if search fails
     */
    public List<Inventory> searchInventories(String organizationId, String searchTerm) throws Exception {
        log.debug("Searching inventories in organization: {} with term: {} by user: {}", 
                organizationId, searchTerm, SecurityUtils.getCurrentUserId());
        
        SearchInventoriesQuery query = SearchInventoriesQuery.builder()
                .userId(SecurityUtils.getCurrentUserId())
                .organizationId(organizationId)
                .searchTerm(searchTerm)
                .build();
        
        return queryBus.execute(query);
    }

    /**
     * Delete an inventory (soft delete)
     * 
     * @param id Inventory ID
     * @throws Exception if deletion fails
     */
    public void deleteInventory(String id) throws Exception {
        log.debug("Deleting inventory: {} by user: {}", id, SecurityUtils.getCurrentUserId());
        
        DeleteInventoryCommand command = DeleteInventoryCommand.builder()
                .userId(SecurityUtils.getCurrentUserId())
                .inventoryId(id)
                .build();
        
        commandBus.execute(command);
    }

    /**
     * Check if inventory exists by name within an organization
     * 
     * @param name Inventory name
     * @param organizationId Organization ID
     * @return true if inventory exists, false otherwise
     * @throws Exception if check fails
     */
    public Boolean inventoryExistsByName(String name, String organizationId) throws Exception {
        log.debug("Checking inventory existence by name: {} in organization: {} by user: {}", 
                name, organizationId, SecurityUtils.getCurrentUserId());
        
        InventoryExistsByNameQuery query = InventoryExistsByNameQuery.builder()
                .userId(SecurityUtils.getCurrentUserId())
                .name(name)
                .organizationId(organizationId)
                .build();
        
        return queryBus.execute(query);
    }

    /**
     * Get inventory count for an organization
     * 
     * @param organizationId Organization ID
     * @return Total number of inventories
     * @throws Exception if count fails
     */
    public Long getInventoryCountByOrganization(String organizationId) throws Exception {
        log.debug("Getting inventory count for organization: {} by user: {}", organizationId, SecurityUtils.getCurrentUserId());
        
        GetInventoryCountByOrganizationQuery query = GetInventoryCountByOrganizationQuery.builder()
                .userId(SecurityUtils.getCurrentUserId())
                .organizationId(organizationId)
                .build();
        
        return queryBus.execute(query);
    }
}