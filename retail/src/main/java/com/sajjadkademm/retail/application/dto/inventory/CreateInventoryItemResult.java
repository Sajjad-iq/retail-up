package com.sajjadkademm.retail.application.dto.inventory;

import com.sajjadkademm.retail.domain.inventory.model.InventoryItem;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Result object for inventory item creation operations
 * Used to handle success/failure scenarios without throwing exceptions
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class CreateInventoryItemResult {
    private boolean success;
    private InventoryItem item;
    private String errorMessage;

    /**
     * Create a successful result
     */
    public static CreateInventoryItemResult success(InventoryItem item) {
        return CreateInventoryItemResult.builder()
                .success(true)
                .item(item)
                .build();
    }

    /**
     * Create a failure result
     */
    public static CreateInventoryItemResult failure(String errorMessage) {
        return CreateInventoryItemResult.builder()
                .success(false)
                .errorMessage(errorMessage)
                .build();
    }

    /**
     * Check if the operation was successful
     */
    public boolean isSuccess() {
        return success;
    }

    /**
     * Get the created item (only available on success)
     */
    public InventoryItem getItem() {
        return item;
    }

    /**
     * Get the error message (only available on failure)
     */
    public String getErrorMessage() {
        return errorMessage;
    }
}
