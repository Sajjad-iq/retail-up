package com.sajjadkademm.retail.domain.inventory.validation;

import com.sajjadkademm.retail.domain.inventory.model.Inventory;
import com.sajjadkademm.retail.domain.auth.model.User;

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
