package com.sajjadkademm.retail.domain.inventory.handlers;

import com.sajjadkademm.retail.shared.cqrs.CommandHandler;
import com.sajjadkademm.retail.domain.inventory.commands.UpdateInventoryItemCommand;
import com.sajjadkademm.retail.domain.inventory.model.InventoryItem;
import com.sajjadkademm.retail.application.services.inventory.InventoryItemService;

import org.springframework.stereotype.Component;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

/**
 * Command handler for updating inventory items.
 * Delegates to existing InventoryItemService to maintain compatibility.
 */
@Slf4j
@Component
@RequiredArgsConstructor
public class UpdateInventoryItemCommandHandler implements CommandHandler<UpdateInventoryItemCommand, InventoryItem> {

    private final InventoryItemService inventoryItemService;

    @Override
    public InventoryItem handle(UpdateInventoryItemCommand command) throws Exception {
        log.debug("Handling UpdateInventoryItemCommand for item: {}", command.getItemId());
        
        // Delegate to existing service - maintains all existing validation and business logic
        return inventoryItemService.updateInventoryItem(command.getItemId(), command.getRequest());
    }

    @Override
    public Class<UpdateInventoryItemCommand> getCommandType() {
        return UpdateInventoryItemCommand.class;
    }

    @Override
    public boolean requiresTransaction() {
        return true; // Update operations need transactions
    }
}