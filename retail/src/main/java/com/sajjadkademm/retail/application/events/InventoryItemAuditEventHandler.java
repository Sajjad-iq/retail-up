package com.sajjadkademm.retail.application.events;

import com.sajjadkademm.retail.shared.cqrs.CommandBus;
import com.sajjadkademm.retail.domain.audit.commands.LogEntityChangeCommand;
import com.sajjadkademm.retail.domain.audit.commands.LogInventoryChangeCommand;
import com.sajjadkademm.retail.domain.audit.enums.AuditAction;
import com.sajjadkademm.retail.domain.audit.enums.EntityType;
import com.sajjadkademm.retail.domain.inventory.model.Inventory;
import com.sajjadkademm.retail.shared.cqrs.QueryBus;
import com.sajjadkademm.retail.domain.inventory.queries.GetInventoryByIdQuery;
import com.sajjadkademm.retail.application.config.security.SecurityUtils;
import com.sajjadkademm.retail.domain.InventoryItem.model.InventoryItem;
import com.sajjadkademm.retail.domain.InventoryItem.events.InventoryItemCreatedEvent;
import com.sajjadkademm.retail.domain.user.model.User;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;

/**
 * Application layer event handler for inventory item audit logging.
 * Located in application layer because it orchestrates multiple services
 * for cross-cutting concerns like auditing.
 */
@Component
@RequiredArgsConstructor
@Slf4j
public class InventoryItemAuditEventHandler {

    private final CommandBus commandBus;
    private final QueryBus queryBus;

    /**
     * Get organization ID from inventory item
     */
    private String getOrganizationId(InventoryItem item) {
        try {
            GetInventoryByIdQuery query = GetInventoryByIdQuery.builder()
                    .userId(SecurityUtils.getCurrentUserId())
                    .inventoryId(item.getInventoryId())
                    .build();
            
            Inventory inventory = queryBus.execute(query);
            return inventory.getOrganizationId();
        } catch (Exception e) {
            log.error("Failed to get organization ID for inventory item: {}", item.getId(), e);
            return null; // Fallback to user ID in audit calls
        }
    }

    @EventListener
    public void handleInventoryItemCreated(InventoryItemCreatedEvent event) {
        try {
            InventoryItem item = event.getInventoryItem();
            User user = event.getCreatedBy();
            
            log.debug("Processing audit logging for created inventory item: {}", item.getName());

            // Get organization ID from inventory
            String organizationId = getOrganizationId(item);
            if (organizationId == null) {
                organizationId = user.getId(); // Fallback to user ID if organization not found
                log.warn("Using user ID as organization ID fallback for audit logging: {}", item.getId());
            }

            // AUDIT: Log item creation
            LogEntityChangeCommand entityChangeCommand = LogEntityChangeCommand.builder()
                    .organizationId(organizationId)
                    .entityType(EntityType.INVENTORY_ITEM)
                    .entityId(item.getId())
                    .entityName(item.getName())
                    .action(AuditAction.CREATE)
                    .description("Inventory item created")
                    .fieldName(null) // No field name for creation
                    .oldValue(null) // No old value for creation
                    .newValue(item.getName()) // New value is the item name
                    .user(user)
                    .userId(user.getId())
                    .build();
            try {
                commandBus.execute(entityChangeCommand);
            } catch (Exception e) {
                log.error("Failed to log entity change audit for item {}: {}", item.getName(), e.getMessage(), e);
                // Don't fail the event processing due to audit logging failure
            }

            // AUDIT: Log initial stock if positive
            Integer initialStock = item.getCurrentStock();
            if (initialStock != null && initialStock > 0) {
                LogInventoryChangeCommand inventoryChangeCommand = LogInventoryChangeCommand.builder()
                        .organizationId(organizationId)
                        .itemId(item.getId())
                        .itemName(item.getName())
                        .movementType("INITIAL_STOCK") // Movement type for initial stock
                        .quantityChange(initialStock) // Quantity change
                        .stockBefore(0) // Stock before (new item starts with 0)
                        .stockAfter(initialStock) // Stock after
                        .reason("Initial stock set during item creation")
                        .referenceType("INVENTORY_ITEM") // Reference type
                        .referenceId(item.getId()) // Reference ID
                        .user(user)
                        .userId(user.getId())
                        .build();
                try {
                    commandBus.execute(inventoryChangeCommand);
                } catch (Exception e) {
                    log.error("Failed to log inventory change audit for item {}: {}", item.getName(), e.getMessage(), e);
                    // Don't fail the event processing due to audit logging failure
                }
            }

            log.debug("Audit logging completed for inventory item: {}", item.getName());

        } catch (Exception e) {
            // Log error but don't let audit failures affect business logic
            log.error("Failed to audit inventory item creation for item: {}", 
                    event.getInventoryItem().getName(), e);
        }
    }
}