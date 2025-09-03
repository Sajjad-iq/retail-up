package com.sajjadkademm.retail.domain.inventory.handlers;

import com.sajjadkademm.retail.shared.cqrs.CommandHandler;
import com.sajjadkademm.retail.domain.inventory.commands.DeleteInventoryItemCommand;
import com.sajjadkademm.retail.application.services.inventory.InventoryItemService;

import org.springframework.stereotype.Component;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

/**
 * Command handler for deleting inventory items.
 * Delegates to existing InventoryItemService to maintain compatibility.
 */
@Slf4j
@Component
@RequiredArgsConstructor
public class DeleteInventoryItemCommandHandler implements CommandHandler<DeleteInventoryItemCommand, Void> {

    private final InventoryItemService inventoryItemService;

    @Override
    public Void handle(DeleteInventoryItemCommand command) throws Exception {
        log.debug("Handling DeleteInventoryItemCommand for item: {}", command.getItemId());
        
        // Delegate to existing service - maintains all existing validation and business logic
        inventoryItemService.deleteInventoryItem(command.getItemId());
        return null;
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