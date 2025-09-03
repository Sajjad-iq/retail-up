package com.sajjadkademm.retail.application.services.inventory;

import com.sajjadkademm.retail.application.dto.inventory.CreateInventoryRequest;
import com.sajjadkademm.retail.application.dto.inventory.UpdateInventoryRequest;
import com.sajjadkademm.retail.domain.inventory.model.Inventory;

import java.util.List;

/**
 * Interface for inventory service operations.
 * Provides contract for inventory management use cases.
 * This abstraction supports better testing and future implementation changes.
 */
public interface InventoryServiceInterface {
    
    /**
     * Create a new inventory
     */
    Inventory createInventory(CreateInventoryRequest request);
    
    /**
     * Update an existing inventory
     */
    Inventory updateInventory(String id, UpdateInventoryRequest request);
    
    /**
     * Get inventory by ID
     */
    Inventory getInventoryById(String id);
    
    /**
     * Delete inventory
     */
    void deleteInventory(String id);
    
    /**
     * Get all inventories for an organization
     */
    List<Inventory> getInventoriesByOrganization(String organizationId);
    
    /**
     * Get all inventories for current user
     */
    List<Inventory> getCurrentUserInventories();
}