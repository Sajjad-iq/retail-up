package com.sajjadkademm.retail.application.services.inventory;

import com.sajjadkademm.retail.domain.inventory.model.Inventory;
import com.sajjadkademm.retail.domain.inventory.repositories.InventoryRepository;
import com.sajjadkademm.retail.shared.localization.errorCode.OrganizationErrorCode;
import com.sajjadkademm.retail.shared.localization.errorCode.UserErrorCode;
import com.sajjadkademm.retail.shared.common.exceptions.BadRequestException;
import com.sajjadkademm.retail.shared.common.exceptions.ConflictException;
import com.sajjadkademm.retail.shared.common.exceptions.NotFoundException;
import com.sajjadkademm.retail.application.dto.inventory.CreateInventoryRequest;
import com.sajjadkademm.retail.application.dto.inventory.UpdateInventoryRequest;
import com.sajjadkademm.retail.domain.organization.model.Organization;
import com.sajjadkademm.retail.application.services.organizations.OrganizationService;
import com.sajjadkademm.retail.domain.organization.validation.OrganizationValidator;
import com.sajjadkademm.retail.domain.auth.validation.UserValidator;
import com.sajjadkademm.retail.domain.auth.model.User;
import com.sajjadkademm.retail.shared.common.exceptions.UnauthorizedException;
import com.sajjadkademm.retail.shared.localization.LocalizedErrorService;
import com.sajjadkademm.retail.shared.localization.errorCode.InventoryErrorCode;
import com.sajjadkademm.retail.application.config.security.SecurityUtils;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import lombok.RequiredArgsConstructor;
import java.util.List;

@Service
@RequiredArgsConstructor
public class InventoryService {
    private final InventoryRepository inventoryRepository;
    private final OrganizationService organizationService;
    private final OrganizationValidator organizationValidationUtils;
    private final LocalizedErrorService localizedErrorService;
    private final UserValidator userValidator;

    /**
     * Create a new inventory for an organization
     */
    @Transactional(rollbackFor = { Exception.class })
    public Inventory createInventory(CreateInventoryRequest request) {
        try {
            // Get current authenticated user
            User currentUser = SecurityUtils.getCurrentUser();
            // check if the user is active
            User user = userValidator.validateUserActive(currentUser.getId());

            // Resolve organization and ensure it exists
            Organization organization = organizationService.getOrganizationById(request.getOrganizationId());
            if (organization == null) {
                throw new NotFoundException(localizedErrorService
                        .getLocalizedMessage(OrganizationErrorCode.ORGANIZATION_NOT_FOUND.getMessage()));
            }

            // Guard: only active organizations can create inventories
            organizationValidationUtils.assertOrganizationIsActive(organization);

            // Check if user has access to the organization (user must be the creator of the
            // organization)
            if (!user.getId().equals(organization.getCreatedBy().getId())) {
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
                    .createdBy(user)
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
        User user = userValidator.validateUserActive(currentUser.getId());
        // Check if user has access to the inventory (user must be the creator of the
        // organization)
        if (!user.getId().equals(inventory.getOrganization().getCreatedBy().getId())) {
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