package com.sajjadkademm.retail.domain.inventory.validation;

import com.sajjadkademm.retail.application.config.security.SecurityUtils;
import com.sajjadkademm.retail.shared.localization.LocalizedErrorService;
import com.sajjadkademm.retail.shared.localization.errorCode.InventoryErrorCode;
import com.sajjadkademm.retail.shared.localization.errorCode.OrganizationErrorCode;
import com.sajjadkademm.retail.shared.localization.errorCode.UserErrorCode;
import com.sajjadkademm.retail.shared.common.exceptions.UnauthorizedException;
import com.sajjadkademm.retail.shared.common.exceptions.BadRequestException;
import com.sajjadkademm.retail.domain.inventory.model.Inventory;
import com.sajjadkademm.retail.domain.inventory.repositories.InventoryRepository;
import com.sajjadkademm.retail.domain.organization.model.Organization;
import com.sajjadkademm.retail.domain.organization.services.OrganizationDomainService;
import com.sajjadkademm.retail.domain.user.model.User;
import com.sajjadkademm.retail.domain.user.validation.UserValidator;
import com.sajjadkademm.retail.application.dto.inventory.CreateInventoryRequest;
import com.sajjadkademm.retail.application.dto.inventory.UpdateInventoryRequest;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

/**
 * Validation utilities for inventory operations
 */
@Component
@RequiredArgsConstructor
public class InventoryValidationUtils {

    private final OrganizationDomainService organizationDomainService;
    private final InventoryRepository inventoryRepository;
    private final LocalizedErrorService localizedErrorService;
    private final UserValidator userValidator;

    /**
     * Validate create inventory request and user permissions
     */
    public void validateCreateRequest(CreateInventoryRequest request, String userId) {
        // Validate user is active
        User user = userValidator.validateUserActive(userId);
        
        // Validate organization exists and user has access
        Organization organization = validateOrganizationAccess(request.getOrganizationId(), userId);
        
        // Validate unique name within organization
        if (inventoryRepository.existsByNameAndOrganizationId(request.getName(), request.getOrganizationId())) {
            throw new BadRequestException(localizedErrorService
                    .getLocalizedMessage(InventoryErrorCode.INVENTORY_NAME_DUPLICATE.getMessage(), request.getName()));
        }
    }

    /**
     * Validate update inventory request
     */
    public void validateUpdateRequest(UpdateInventoryRequest request, Inventory existingInventory) {
        // If name is being changed, validate uniqueness
        if (request.getName() != null && !request.getName().equals(existingInventory.getName())) {
            if (inventoryRepository.existsByNameAndOrganizationId(request.getName(), existingInventory.getOrganizationId())) {
                throw new BadRequestException(localizedErrorService
                        .getLocalizedMessage(InventoryErrorCode.INVENTORY_NAME_DUPLICATE.getMessage(), request.getName()));
            }
        }
    }

    /**
     * Validate user has access to update inventory
     */
    public void validateUpdateAccess(Inventory inventory, String userId) {
        validateReadAccess(inventory, userId);
    }

    /**
     * Validate user has access to delete inventory
     */
    public void validateDeleteAccess(Inventory inventory, String userId) {
        validateReadAccess(inventory, userId);
        
        // Additional validation: Check if inventory has items (could prevent deletion)
        // This would require checking if inventory has associated inventory items
        // For now, we allow deletion - cascade rules in database will handle cleanup
    }

    /**
     * Validate user has read access to inventory
     */
    public void validateReadAccess(Inventory inventory, String userId) {
        // Validate user access through organization
        validateOrganizationAccess(inventory.getOrganizationId(), userId);
    }

    /**
     * Validate user has access to organization and return the organization
     */
    public Organization validateOrganizationAccess(String organizationId, String userId) {
        try {
            // Get current authenticated user and validate active
            User currentUser = userValidator.validateUserActive(userId);

            // Get organization and check if user has access to it
            Organization organization = organizationDomainService.getOrganizationById(organizationId);
            if (organization == null) {
                throw new UnauthorizedException(localizedErrorService
                        .getLocalizedMessage(OrganizationErrorCode.ORGANIZATION_NOT_FOUND.getMessage()));
            }

            // Check if user is organization creator
            if (!currentUser.getId().equals(organization.getCreatedBy().getId())) {
                throw new UnauthorizedException(localizedErrorService
                        .getLocalizedMessage(UserErrorCode.USER_NOT_ORGANIZATION_CREATOR.getMessage()));
            }

            return organization;
        } catch (UnauthorizedException e) {
            throw e; // Re-throw authorization exceptions
        } catch (Exception e) {
            throw new UnauthorizedException(localizedErrorService
                    .getLocalizedMessage(UserErrorCode.USER_NOT_ORGANIZATION_CREATOR.getMessage()) + ": " + e.getMessage());
        }
    }
}