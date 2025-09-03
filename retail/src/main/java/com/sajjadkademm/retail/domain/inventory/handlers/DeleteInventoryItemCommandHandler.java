package com.sajjadkademm.retail.domain.inventory.handlers;

import com.sajjadkademm.retail.shared.cqrs.CommandHandler;
import com.sajjadkademm.retail.domain.inventory.commands.DeleteInventoryItemCommand;
import com.sajjadkademm.retail.domain.inventory.repositories.InventoryItemRepository;
import com.sajjadkademm.retail.domain.inventory.validation.InventoryItemValidationUtils;
import com.sajjadkademm.retail.domain.inventory.model.InventoryItem;
import com.sajjadkademm.retail.application.services.audit.GlobalAuditService;
import com.sajjadkademm.retail.domain.audit.enums.AuditAction;
import com.sajjadkademm.retail.domain.audit.enums.EntityType;
import com.sajjadkademm.retail.domain.inventory.services.InventoryDomainService;
import com.sajjadkademm.retail.shared.common.exceptions.NotFoundException;
import com.sajjadkademm.retail.shared.localization.LocalizedErrorService;
import com.sajjadkademm.retail.shared.localization.errorCode.InventoryItemErrorCode;

import org.springframework.stereotype.Component;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

/**
 * Command handler for deleting inventory items.
 * Pure CQRS implementation using repositories and domain services directly.
 */
@Slf4j
@Component
@RequiredArgsConstructor
public class DeleteInventoryItemCommandHandler implements CommandHandler<DeleteInventoryItemCommand, Void> {

    private final InventoryItemRepository inventoryItemRepository;
    private final InventoryItemValidationUtils validationUtils;
    private final GlobalAuditService globalAuditService;
    private final InventoryDomainService inventoryDomainService;
    private final LocalizedErrorService localizedErrorService;

    @Override
    public Void handle(DeleteInventoryItemCommand command) throws Exception {
        log.debug("Handling DeleteInventoryItemCommand for item: {}", command.getItemId());
        
        // Find existing inventory item
        InventoryItem existingItem = inventoryItemRepository.findById(command.getItemId())
                .orElseThrow(() -> new NotFoundException(localizedErrorService
                        .getLocalizedMessage(InventoryItemErrorCode.INVENTORY_ITEM_NOT_FOUND.getMessage(), command.getItemId())));

        // Validate user access
        validationUtils.checkUserAccessToInventoryItem(existingItem);
        
        // Get organization ID for audit (before deletion)
        String organizationId = getOrganizationId(existingItem);
        String itemName = existingItem.getName();
        
        // Delete inventory item
        inventoryItemRepository.delete(existingItem);
        
        // Log audit trail
        globalAuditService.auditEntityChange(
                organizationId,
                EntityType.INVENTORY_ITEM,
                command.getItemId(),
                itemName,
                AuditAction.DELETE,
                "Deleted inventory item: " + itemName,
                null, // fieldName
                null, // oldValue
                null, // newValue
                existingItem.getCreatedBy()
        );
        
        log.info("Successfully deleted inventory item: {} for user: {}", 
                command.getItemId(), command.getUserId());
        
        return null;
    }
    
    /**
     * Get organization ID from inventory item
     */
    private String getOrganizationId(InventoryItem item) {
        try {
            return inventoryDomainService.getOrganizationIdByInventory(item.getInventoryId());
        } catch (Exception e) {
            return item.getCreatedBy().getId(); // Fallback to user ID
        }
    }

    @Override
    public Class<DeleteInventoryItemCommand> getCommandType() {
        return DeleteInventoryItemCommand.class;
    }

    @Override
    public boolean requiresTransaction() {
        return true; // Delete operations need transactions
    }
}