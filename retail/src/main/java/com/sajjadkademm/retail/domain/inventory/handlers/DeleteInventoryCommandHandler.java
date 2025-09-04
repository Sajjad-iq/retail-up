package com.sajjadkademm.retail.domain.inventory.handlers;

import com.sajjadkademm.retail.shared.cqrs.CommandHandler;
import com.sajjadkademm.retail.domain.inventory.commands.DeleteInventoryCommand;
import com.sajjadkademm.retail.domain.inventory.model.Inventory;
import com.sajjadkademm.retail.domain.inventory.repositories.InventoryRepository;
import com.sajjadkademm.retail.domain.inventory.validation.InventoryValidationUtils;
import com.sajjadkademm.retail.application.services.audit.GlobalAuditService;
import com.sajjadkademm.retail.domain.audit.enums.AuditAction;
import com.sajjadkademm.retail.domain.audit.enums.EntityType;
import com.sajjadkademm.retail.shared.common.exceptions.NotFoundException;
import com.sajjadkademm.retail.shared.localization.LocalizedErrorService;
import com.sajjadkademm.retail.shared.localization.errorCode.InventoryErrorCode;
import com.sajjadkademm.retail.shared.cache.CacheInvalidationService;

import org.springframework.stereotype.Component;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

/**
 * Command handler for deleting inventories
 */
@Slf4j
@Component
@RequiredArgsConstructor
public class DeleteInventoryCommandHandler implements CommandHandler<DeleteInventoryCommand, Void> {

    private final InventoryRepository inventoryRepository;
    private final InventoryValidationUtils validationUtils;
    private final GlobalAuditService globalAuditService;
    private final LocalizedErrorService localizedErrorService;
    private final CacheInvalidationService cacheInvalidationService;

    @Override
    public Void handle(DeleteInventoryCommand command) throws Exception {
        log.debug("Handling DeleteInventoryCommand for inventory: {}", command.getInventoryId());

        // Find existing inventory
        Inventory existingInventory = inventoryRepository.findById(command.getInventoryId())
                .orElseThrow(() -> new NotFoundException(localizedErrorService
                        .getLocalizedMessage(InventoryErrorCode.INVENTORY_NOT_FOUND.getMessage(), command.getInventoryId())));

        // Validate user access and delete permissions
        validationUtils.validateDeleteAccess(existingInventory, command.getUserId());
        
        String inventoryName = existingInventory.getName();
        String organizationId = existingInventory.getOrganizationId();
        
        // Delete inventory
        inventoryRepository.delete(existingInventory);
        
        // Invalidate caches
        cacheInvalidationService.invalidateInventoryCaches(command.getInventoryId(), organizationId);
        
        // Log audit trail
        globalAuditService.auditEntityChange(
                organizationId,
                EntityType.INVENTORY,
                command.getInventoryId(),
                inventoryName,
                AuditAction.DELETE,
                "Deleted inventory: " + inventoryName,
                null, // fieldName
                null, // oldValue
                null, // newValue
                existingInventory.getCreatedBy()
        );
        
        log.info("Successfully deleted inventory: {} for user: {}", 
                command.getInventoryId(), command.getUserId());
        
        return null;
    }

    @Override
    public Class<DeleteInventoryCommand> getCommandType() {
        return DeleteInventoryCommand.class;
    }

    @Override
    public boolean requiresTransaction() {
        return true;
    }
}