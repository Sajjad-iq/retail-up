package com.sajjadkademm.retail.inventory.InventoryMovement.enums;

/**
 * Enumeration of inventory movement types
 */
public enum MovementType {
    // Stock increases
    STOCK_IN("Stock In - Initial stock or restocking"),
    PURCHASE("Purchase - Stock received from supplier"),
    RETURN("Return - Customer returned items"),
    ADJUSTMENT_IN("Adjustment In - Positive stock correction"),
    TRANSFER_IN("Transfer In - Stock received from another location"),
    
    // Stock decreases
    STOCK_OUT("Stock Out - General stock removal"),
    SALE("Sale - Items sold to customer"),
    DAMAGE("Damage - Items damaged or spoiled"),
    THEFT("Theft - Items stolen or missing"),
    EXPIRED("Expired - Items past expiry date"),
    ADJUSTMENT_OUT("Adjustment Out - Negative stock correction"),
    TRANSFER_OUT("Transfer Out - Stock sent to another location"),
    
    // Other movements
    MANUAL_ADJUSTMENT("Manual Adjustment - Manual stock correction");

    private final String description;

    MovementType(String description) {
        this.description = description;
    }

    public String getDescription() {
        return description;
    }
}