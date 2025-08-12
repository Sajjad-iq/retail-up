package com.sajjadkademm.retail.inventory.InventoryMovement.dto;

/**
 * Reference types for inventory movements (originating business process)
 */
public enum ReferenceType {
    SALE,
    PURCHASE,
    ADJUSTMENT,
    TRANSFER,
    RETURN,
    DAMAGE,
    THEFT,
    STOCK_TAKE,
    OTHER
}
