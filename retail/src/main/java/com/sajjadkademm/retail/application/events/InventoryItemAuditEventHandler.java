package com.sajjadkademm.retail.application.events;

import com.sajjadkademm.retail.application.services.audit.GlobalAuditService;
import com.sajjadkademm.retail.domain.audit.enums.AuditAction;
import com.sajjadkademm.retail.domain.audit.enums.EntityType;
import com.sajjadkademm.retail.domain.inventory.model.Inventory;
import com.sajjadkademm.retail.shared.cqrs.QueryBus;
import com.sajjadkademm.retail.domain.inventory.queries.GetInventoryByIdQuery;
import com.sajjadkademm.retail.application.config.security.SecurityUtils;
import com.sajjadkademm.retail.domain.inventory.model.InventoryItem;
import com.sajjadkademm.retail.domain.inventory.events.InventoryItemCreatedEvent;
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

    private final GlobalAuditService globalAuditService;
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
            globalAuditService.auditEntityChange(
                    organizationId,
                    EntityType.INVENTORY_ITEM,
                    item.getId(),
                    item.getName(),
                    AuditAction.CREATE,
                    "Inventory item created",
                    null, // No field name for creation
                    null, // No old value for creation
                    item.getName(), // New value is the item name
                    user);

            // AUDIT: Log initial stock if positive
            Integer initialStock = item.getCurrentStock();
            if (initialStock != null && initialStock > 0) {
                globalAuditService.auditInventoryChange(
                        organizationId,
                        item.getId(),
                        item.getName(),
                        "INITIAL_STOCK", // Movement type for initial stock
                        initialStock, // Quantity change
                        0, // Stock before (new item starts with 0)
                        initialStock, // Stock after
                        "Initial stock set during item creation",
                        "INVENTORY_ITEM", // Reference type
                        item.getId(), // Reference ID
                        user);
            }

            log.debug("Audit logging completed for inventory item: {}", item.getName());

        } catch (Exception e) {
            // Log error but don't let audit failures affect business logic
            log.error("Failed to audit inventory item creation for item: {}", 
                    event.getInventoryItem().getName(), e);
        }
    }
}