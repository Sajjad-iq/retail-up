package com.sajjadkademm.retail.domain.inventory.services;

import com.sajjadkademm.retail.domain.inventory.model.Inventory;
import com.sajjadkademm.retail.domain.inventory.repositories.InventoryRepository;
import com.sajjadkademm.retail.shared.common.exceptions.NotFoundException;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

/**
 * Domain service for inventory-specific business logic.
 * This service contains core business rules that don't belong in entities.
 * Unlike application services, domain services focus purely on business logic
 * without orchestration concerns.
 */
@Service
@RequiredArgsConstructor
public class InventoryDomainService {

    private final InventoryRepository inventoryRepository;

    /**
     * Get inventory by ID with domain validation
     */
    public Inventory getInventoryById(String id) {
        return inventoryRepository.findById(id)
                .orElseThrow(() -> new NotFoundException("Inventory not found with id: " + id));
    }

    /**
     * Check if inventory exists and is active
     */
    public boolean isInventoryActiveById(String id) {
        return inventoryRepository.findById(id)
                .map(Inventory::getIsActive)
                .orElse(false);
    }

    /**
     * Get organization ID from inventory (domain logic)
     */
    public String getOrganizationIdByInventory(String inventoryId) {
        return getInventoryById(inventoryId).getOrganizationId();
    }
}