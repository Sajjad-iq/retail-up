package com.sajjadkademm.retail.domain.inventory.handlers;

import com.sajjadkademm.retail.shared.cqrs.CommandHandler;
import com.sajjadkademm.retail.domain.inventory.commands.CreateInventoryItemCommand;
import com.sajjadkademm.retail.domain.inventory.model.InventoryItem;
import com.sajjadkademm.retail.application.services.inventory.InventoryItemService;

import org.springframework.stereotype.Component;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

/**
 * Command handler for creating inventory items.
 * Delegates to existing InventoryItemService to maintain compatibility.
 */
@Slf4j
@Component
@RequiredArgsConstructor
public class CreateInventoryItemCommandHandler implements CommandHandler<CreateInventoryItemCommand, InventoryItem> {

    private final InventoryItemService inventoryItemService;

    @Override
    public InventoryItem handle(CreateInventoryItemCommand command) throws Exception {
        log.debug("Handling CreateInventoryItemCommand for user: {}", command.getUserId());
        
        // Delegate to existing service - maintains all existing validation and business logic
        return inventoryItemService.createInventoryItem(command.getRequest());
    }

    @Override
    public Class<CreateInventoryItemCommand> getCommandType() {
        return CreateInventoryItemCommand.class;
    }

    @Override
    public boolean requiresTransaction() {
        return true; // Creation operations need transactions
    }
}