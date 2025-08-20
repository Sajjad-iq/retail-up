package com.sajjadkademm.retail.inventory;

import com.sajjadkademm.retail.exceptions.BadRequestException;
import com.sajjadkademm.retail.exceptions.ConflictException;
import com.sajjadkademm.retail.exceptions.NotFoundException;
import com.sajjadkademm.retail.inventory.dto.CreateInventoryRequest;
import com.sajjadkademm.retail.inventory.dto.UpdateInventoryRequest;
import com.sajjadkademm.retail.organizations.Organization;
import com.sajjadkademm.retail.organizations.OrganizationService;
import com.sajjadkademm.retail.organizations.OrganizationValidationUtils;
import com.sajjadkademm.retail.users.User;
import com.sajjadkademm.retail.users.UserService;
import com.sajjadkademm.retail.users.dto.UserStatus;
import com.sajjadkademm.retail.exceptions.UnauthorizedException;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

@Service
public class InventoryService {
    private final InventoryRepository inventoryRepository;
    private final OrganizationService organizationService;
    private final UserService userService;

    @Autowired
    public InventoryService(InventoryRepository inventoryRepository,
            OrganizationService organizationService,
            UserService userService) {
        this.inventoryRepository = inventoryRepository;
        this.organizationService = organizationService;
        this.userService = userService;
    }

    /**
     * Create a new inventory for an organization
     */
    @Transactional(rollbackFor = { Exception.class })
    public Inventory createInventory(CreateInventoryRequest request) {
        try {
            // Resolve organization and ensure it exists
            Organization organization = organizationService.getOrganizationById(request.getOrganizationId());
            if (organization == null) {
                throw new NotFoundException("Organization not found with ");
            }

            // Guard: only active organizations can create inventories
            OrganizationValidationUtils.assertOrganizationIsActive(organization);

            // Resolve creating user and ensure it exists
            User user = userService.getUserById(request.getUserId());
            if (user == null) {
                throw new NotFoundException("User not found");
            }

            if (user.getStatus() != UserStatus.ACTIVE) {
                throw new UnauthorizedException("Only Active Users Can Crate Inventories");
            }

            // Uniqueness: name must be unique within organization
            if (inventoryRepository.existsByNameAndOrganizationId(request.getName(), request.getOrganizationId())) {
                throw new ConflictException(
                        "Inventory with name '" + request.getName() + "' already exists in this organization");
            }

            // Persist
            Inventory inventory = Inventory.builder()
                    .name(request.getName())
                    .description(request.getDescription())
                    .location(request.getLocation())
                    .organizationId(request.getOrganizationId())
                    .isActive(true)
                    .createdBy(user)
                    .build();

            return inventoryRepository.save(inventory);

        } catch (ConflictException e) {
            throw e;
        } catch (BadRequestException e) {
            throw e;
        } catch (Exception e) {
            throw new BadRequestException("Failed to create inventory: " + e.getMessage(), e);
        }
    }

    /**
     * Update an existing inventory
     */
    public Inventory updateInventory(String id, UpdateInventoryRequest request) {
        Inventory inventory = inventoryRepository.findById(id)
                .orElseThrow(() -> new NotFoundException("Inventory not found with ID: " + id));

        // Uniqueness: name must be unique within organization when changed
        if (request.getName() != null && !request.getName().equals(inventory.getName())) {
            if (inventoryRepository.existsByNameAndOrganizationId(request.getName(), inventory.getOrganizationId())) {
                throw new ConflictException(
                        "Inventory with name '" + request.getName() + "' already exists in this organization");
            }
        }

        // Guard: disallow updates if parent organization is not active
        OrganizationValidationUtils.assertOrganizationIsActive(inventory.getOrganization());

        // Apply changes
        if (request.getName() != null) {
            inventory.setName(request.getName());
        }
        if (request.getDescription() != null) {
            inventory.setDescription(request.getDescription());
        }
        if (request.getLocation() != null) {
            inventory.setLocation(request.getLocation());
        }
        if (request.getIsActive() != null) {
            inventory.setIsActive(request.getIsActive());
        }
        return inventoryRepository.save(inventory);
    }

    /**
     * Get inventory by ID
     */
    public Inventory getInventoryById(String id) {
        return inventoryRepository.findById(id)
                .orElseThrow(() -> new NotFoundException("Inventory not found with ID: " + id));
    }

    /**
     * Get all inventories for an organization
     */
    public List<Inventory> getInventoriesByOrganization(String organizationId) {
        return inventoryRepository.findByOrganizationId(organizationId);
    }

    /**
     * Get active inventories for an organization
     */
    public List<Inventory> getActiveInventoriesByOrganization(String organizationId) {
        return inventoryRepository.findByOrganizationIdAndIsActiveTrue(organizationId);
    }

    /**
     * Search inventories by name within an organization
     */
    public List<Inventory> searchInventories(String organizationId, String searchTerm) {
        return inventoryRepository.searchInventoriesByOrganization(organizationId, searchTerm);
    }

    /**
     * Delete inventory (soft delete by setting isActive to false)
     */
    public void deleteInventory(String id) {
        Inventory inventory = inventoryRepository.findById(id)
                .orElseThrow(() -> new NotFoundException("Inventory not found"));

        inventory.setIsActive(false);
        inventoryRepository.save(inventory);
    }

    /**
     * Check if inventory exists by name within an organization
     */
    public boolean inventoryExistsByName(String name, String organizationId) {
        return inventoryRepository.existsByNameAndOrganizationId(name, organizationId);
    }

    /**
     * Get inventory count for an organization
     */
    public long getInventoryCountByOrganization(String organizationId) {
        return inventoryRepository.countByOrganizationId(organizationId);
    }
}