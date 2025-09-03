package com.sajjadkademm.retail.domain.inventory.handlers;

import com.sajjadkademm.retail.shared.cqrs.CommandHandler;
import com.sajjadkademm.retail.domain.inventory.commands.UpdateInventoryCommand;
import com.sajjadkademm.retail.domain.inventory.model.Inventory;
import com.sajjadkademm.retail.domain.inventory.repositories.InventoryRepository;
import com.sajjadkademm.retail.domain.inventory.validation.InventoryValidationUtils;
import com.sajjadkademm.retail.application.services.audit.GlobalAuditService;
import com.sajjadkademm.retail.domain.audit.enums.AuditAction;
import com.sajjadkademm.retail.domain.audit.enums.EntityType;
import com.sajjadkademm.retail.application.dto.inventory.UpdateInventoryRequest;
import com.sajjadkademm.retail.shared.common.exceptions.NotFoundException;
import com.sajjadkademm.retail.shared.localization.LocalizedErrorService;
import com.sajjadkademm.retail.shared.localization.errorCode.InventoryErrorCode;

import org.springframework.stereotype.Component;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

/**
 * Command handler for updating inventories
 */
@Slf4j
@Component
@RequiredArgsConstructor
public class UpdateInventoryCommandHandler implements CommandHandler<UpdateInventoryCommand, Inventory> {

    private final InventoryRepository inventoryRepository;
    private final InventoryValidationUtils validationUtils;
    private final GlobalAuditService globalAuditService;
    private final LocalizedErrorService localizedErrorService;

    @Override
    public Inventory handle(UpdateInventoryCommand command) throws Exception {
        log.debug("Handling UpdateInventoryCommand for inventory: {}", command.getInventoryId());

        // Find existing inventory
        Inventory existingInventory = inventoryRepository.findById(command.getInventoryId())
                .orElseThrow(() -> new NotFoundException(localizedErrorService
                        .getLocalizedMessage(InventoryErrorCode.INVENTORY_NOT_FOUND.getMessage(), command.getInventoryId())));

        // Validate user access and update request
        validationUtils.validateUpdateAccess(existingInventory, command.getUserId());
        validationUtils.validateUpdateRequest(command.getRequest(), existingInventory);
        
        // Apply updates
        UpdateInventoryRequest request = command.getRequest();
        applyUpdates(existingInventory, request);
        
        // Save updated inventory
        Inventory savedInventory = inventoryRepository.save(existingInventory);
        
        // Log audit trail
        globalAuditService.auditEntityChange(
                savedInventory.getOrganizationId(),
                EntityType.INVENTORY,
                savedInventory.getId(),
                savedInventory.getName(),
                AuditAction.UPDATE,
                "Updated inventory: " + savedInventory.getName(),
                null, // fieldName
                null, // oldValue
                null, // newValue
                savedInventory.getCreatedBy()
        );
        
        log.info("Successfully updated inventory: {} for user: {}", 
                savedInventory.getId(), command.getUserId());
        
        return savedInventory;
    }
    
    /**
     * Apply updates from request to existing inventory
     */
    private void applyUpdates(Inventory existingInventory, UpdateInventoryRequest request) {
        if (request.getName() != null) {
            existingInventory.setName(request.getName());
        }
        if (request.getDescription() != null) {
            existingInventory.setDescription(request.getDescription());
        }
        if (request.getLocation() != null) {
            existingInventory.setLocation(request.getLocation());
        }
        if (request.getIsActive() != null) {
            existingInventory.setIsActive(request.getIsActive());
        }
    }

    @Override
    public Class<UpdateInventoryCommand> getCommandType() {
        return UpdateInventoryCommand.class;
    }

    @Override
    public boolean requiresTransaction() {
        return true;
    }
}