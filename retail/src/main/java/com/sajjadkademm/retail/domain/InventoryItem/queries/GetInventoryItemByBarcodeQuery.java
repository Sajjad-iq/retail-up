package com.sajjadkademm.retail.domain.inventory.queries;

import com.sajjadkademm.retail.shared.cqrs.Query;
import com.sajjadkademm.retail.domain.InventoryItem.model.InventoryItem;

import lombok.Builder;
import lombok.Data;

/**
 * Query to get inventory item by barcode and inventory ID
 */
@Data
@Builder
public class GetInventoryItemByBarcodeQuery implements Query<InventoryItem> {
    
    private final String userId;
    private final String barcode;
    private final String inventoryId;

    @Override
    public String getUserId() {
        return userId;
    }

    @Override
    public void validate() {
        if (userId == null || userId.trim().isEmpty()) {
            throw new IllegalArgumentException("User ID is required");
        }
        if (barcode == null || barcode.trim().isEmpty()) {
            throw new IllegalArgumentException("Barcode is required");
        }
        if (inventoryId == null || inventoryId.trim().isEmpty()) {
            throw new IllegalArgumentException("Inventory ID is required");
        }
    }
}