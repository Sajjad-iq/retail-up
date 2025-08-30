package com.sajjadkademm.retail.inventory;

import com.sajjadkademm.retail.config.locales.errorCode.OrganizationErrorCode;
import com.sajjadkademm.retail.config.locales.errorCode.UserErrorCode;
import com.sajjadkademm.retail.exceptions.BadRequestException;
import com.sajjadkademm.retail.exceptions.ConflictException;
import com.sajjadkademm.retail.exceptions.NotFoundException;
import com.sajjadkademm.retail.inventory.dto.CreateInventoryRequest;
import com.sajjadkademm.retail.inventory.dto.UpdateInventoryRequest;
import com.sajjadkademm.retail.organizations.Organization;
import com.sajjadkademm.retail.organizations.OrganizationService;
import com.sajjadkademm.retail.shared.validators.OrganizationValidator;
import com.sajjadkademm.retail.users.User;
import com.sajjadkademm.retail.shared.enums.UserStatus;
import com.sajjadkademm.retail.exceptions.UnauthorizedException;
import com.sajjadkademm.retail.config.locales.LocalizedErrorService;
import com.sajjadkademm.retail.config.locales.errorCode.InventoryErrorCode;
import com.sajjadkademm.retail.config.SecurityUtils;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

@Service
public class InventoryService {
    private final InventoryRepository inventoryRepository;
    private final OrganizationService organizationService;
    private final OrganizationValidator organizationValidationUtils;
    private final LocalizedErrorService localizedErrorService;

    @Autowired
    public InventoryService(InventoryRepository inventoryRepository,
            OrganizationService organizationService,
                            OrganizationValidator organizationValidationUtils,
            LocalizedErrorService localizedErrorService) {
        this.inventoryRepository = inventoryRepository;
        this.organizationService = organizationService;
        this.organizationValidationUtils = organizationValidationUtils;
        this.localizedErrorService = localizedErrorService;
    }

    /**
     * Create a new inventory for an organization
     */
    @Transactional(rollbackFor = { Exception.class })
    public Inventory createInventory(CreateInventoryRequest request) {
        try {
            // Get current authenticated user
            User currentUser = SecurityUtils.getCurrentUser();

            // Resolve organization and ensure it exists
            Organization organization = organizationService.getOrganizationById(request.getOrganizationId());
            if (organization == null) {
                throw new NotFoundException(localizedErrorService
                        .getLocalizedMessage(OrganizationErrorCode.ORGANIZATION_NOT_FOUND.getMessage()));
            }

            // Guard: only active organizations can create inventories
            organizationValidationUtils.assertOrganizationIsActive(organization);

            // Validate current user permissions
            if (currentUser.getStatus() != UserStatus.ACTIVE) {
                throw new UnauthorizedException(localizedErrorService
                        .getLocalizedMessage(UserErrorCode.USER_NOT_ACTIVE.getMessage()));
            }

            // Check if user has access to the organization (user must be the creator of the
            // organization)
            if (!currentUser.getId().equals(organization.getCreatedBy().getId())) {
                throw new UnauthorizedException(localizedErrorService
                        .getLocalizedMessage(UserErrorCode.USER_NOT_ORGANIZATION_CREATOR.getMessage()));
            }

            // Uniqueness: name must be unique within organization
            if (inventoryRepository.existsByNameAndOrganizationId(request.getName(), request.getOrganizationId())) {
                throw new ConflictException(localizedErrorService
                        .getLocalizedMessage(InventoryErrorCode.INVENTORY_NAME_DUPLICATE.getMessage(),
                                request.getName()));
            }

            // Persist
            Inventory inventory = Inventory.builder()
                    .name(request.getName())
                    .description(request.getDescription())
                    .location(request.getLocation())
                    .organizationId(request.getOrganizationId())
                    .isActive(true)
                    .createdBy(currentUser)
                    .build();

            return inventoryRepository.save(inventory);

        } catch (ConflictException e) {
            throw e;
        } catch (BadRequestException e) {
            throw e;
        } catch (Exception e) {
            throw new BadRequestException(localizedErrorService
                    .getLocalizedMessage(InventoryErrorCode.INVENTORY_CREATION_FAILED.getMessage()), e);
        }
    }

    /**
     * Update an existing inventory
     */
    public Inventory updateInventory(String id, UpdateInventoryRequest request) {
        Inventory inventory = inventoryRepository.findById(id)
                .orElseThrow(() -> new NotFoundException(localizedErrorService
                        .getLocalizedMessage(InventoryErrorCode.INVENTORY_NOT_FOUND.getMessage(), id)));

        // Get current authenticated user
        User currentUser = SecurityUtils.getCurrentUser();

        // Check if user has access to the inventory (user must be the creator of the
        // organization)
        if (!currentUser.getId().equals(inventory.getOrganization().getCreatedBy().getId())) {
            throw new UnauthorizedException(localizedErrorService
                    .getLocalizedMessage(UserErrorCode.USER_NOT_ORGANIZATION_CREATOR.getMessage()));
        }

        // Uniqueness: name must be unique within organization when changed
        if (request.getName() != null && !request.getName().equals(inventory.getName())) {
            if (inventoryRepository.existsByNameAndOrganizationId(request.getName(), inventory.getOrganizationId())) {
                throw new ConflictException(localizedErrorService
                        .getLocalizedMessage(InventoryErrorCode.INVENTORY_NAME_DUPLICATE.getMessage(),
                                request.getName()));
            }
        }

        // Guard: disallow updates if parent organization is not active
        organizationValidationUtils.assertOrganizationIsActive(inventory.getOrganization());

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
     * Get inventory by ID (only for users with access to the organization)
     */
    public Inventory getInventoryById(String id) {
        Inventory inventory = inventoryRepository.findById(id)
                .orElseThrow(() -> new NotFoundException(localizedErrorService
                        .getLocalizedMessage(InventoryErrorCode.INVENTORY_NOT_FOUND.getMessage(), id)));

        // Get current authenticated user
        User currentUser = SecurityUtils.getCurrentUser();

        // Check if user has access to the inventory (user must be the creator of the
        // organization)
        if (!currentUser.getId().equals(inventory.getOrganization().getCreatedBy().getId())) {
            throw new UnauthorizedException(localizedErrorService
                    .getLocalizedMessage(UserErrorCode.USER_NOT_ORGANIZATION_CREATOR.getMessage()));
        }

        return inventory;
    }

    /**
     * Get all inventories for an organization (only for users with access to the
     * organization)
     */
    public List<Inventory> getInventoriesByOrganization(String organizationId) {
        // Get current authenticated user
        User currentUser = SecurityUtils.getCurrentUser();

        // Verify user has access to the organization
        Organization organization = organizationService.getOrganizationById(organizationId);
        if (!currentUser.getId().equals(organization.getCreatedBy().getId())) {
            throw new UnauthorizedException(localizedErrorService
                    .getLocalizedMessage(UserErrorCode.USER_NOT_ORGANIZATION_CREATOR.getMessage()));
        }

        return inventoryRepository.findByOrganizationId(organizationId);
    }

    /**
     * Get active inventories for an organization (only for users with access to the
     * organization)
     */
    public List<Inventory> getActiveInventoriesByOrganization(String organizationId) {
        // Get current authenticated user
        User currentUser = SecurityUtils.getCurrentUser();

        // Verify user has access to the organization
        Organization organization = organizationService.getOrganizationById(organizationId);
        if (!currentUser.getId().equals(organization.getCreatedBy().getId())) {
            throw new UnauthorizedException(localizedErrorService
                    .getLocalizedMessage(UserErrorCode.USER_NOT_ORGANIZATION_CREATOR.getMessage()));
        }

        return inventoryRepository.findByOrganizationIdAndIsActiveTrue(organizationId);
    }

    /**
     * Search inventories by name within an organization (only for users with access
     * to the organization)
     */
    public List<Inventory> searchInventories(String organizationId, String searchTerm) {
        // Get current authenticated user
        User currentUser = SecurityUtils.getCurrentUser();

        // Verify user has access to the organization
        Organization organization = organizationService.getOrganizationById(organizationId);
        if (!currentUser.getId().equals(organization.getCreatedBy().getId())) {
            throw new UnauthorizedException(localizedErrorService
                    .getLocalizedMessage(UserErrorCode.USER_NOT_ORGANIZATION_CREATOR.getMessage()));
        }

        return inventoryRepository.searchInventoriesByOrganization(organizationId, searchTerm);
    }

    /**
     * Delete inventory (soft delete by setting isActive to false)
     */
    public void deleteInventory(String id) {
        Inventory inventory = inventoryRepository.findById(id)
                .orElseThrow(() -> new NotFoundException(localizedErrorService
                        .getLocalizedMessage(InventoryErrorCode.INVENTORY_NOT_FOUND.getMessage(), id)));

        // Get current authenticated user
        User currentUser = SecurityUtils.getCurrentUser();

        // Check if user has access to the inventory (user must be the creator of the
        // organization)
        if (!currentUser.getId().equals(inventory.getOrganization().getCreatedBy().getId())) {
            throw new UnauthorizedException(localizedErrorService
                    .getLocalizedMessage(UserErrorCode.USER_NOT_ORGANIZATION_CREATOR.getMessage()));
        }

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