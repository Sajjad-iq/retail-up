package com.sajjadkademm.retail.inventory.InventoryItem.events;

import com.sajjadkademm.retail.inventory.InventoryItem.InventoryItem;
import com.sajjadkademm.retail.users.User;
import org.springframework.context.ApplicationEvent;

/**
 * Event published when an inventory item is successfully created.
 * This allows for decoupled audit logging and other side effects.
 */
public class InventoryItemCreatedEvent extends ApplicationEvent {
    
    private final InventoryItem inventoryItem;
    private final User createdBy;
    
    public InventoryItemCreatedEvent(Object source, InventoryItem inventoryItem, User createdBy) {
        super(source);
        this.inventoryItem = inventoryItem;
        this.createdBy = createdBy;
    }
    
    public InventoryItem getInventoryItem() {
        return inventoryItem;
    }
    
    public User getCreatedBy() {
        return createdBy;
    }
}