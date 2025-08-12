package com.sajjadkademm.retail.inventory.InventoryItem.utils;

import com.sajjadkademm.retail.inventory.Inventory;
import com.sajjadkademm.retail.users.User;

/**
 * Holder for validated dependencies needed to create an InventoryItem
 */
public class ValidatedCreateInventoryItemContext {
    private final Inventory inventory;
    private final User user;

    public ValidatedCreateInventoryItemContext(Inventory inventory, User user) {
        this.inventory = inventory;
        this.user = user;
    }

    public Inventory getInventory() {
        return inventory;
    }

    public User getUser() {
        return user;
    }
}
