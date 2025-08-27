package com.sajjadkademm.retail.inventory.InventoryMovement.dto;

import com.sajjadkademm.retail.inventory.InventoryItem.InventoryItem;
import com.sajjadkademm.retail.inventory.InventoryMovement.enums.MovementType;
import com.sajjadkademm.retail.inventory.InventoryMovement.enums.ReferenceType;
import com.sajjadkademm.retail.users.User;

import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Size;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class CreateMovementRequest {

    @NotNull(message = "User is required")
    private User user;

    @NotNull(message = "Inventory item is required")
    private InventoryItem inventoryItem;

    @NotNull(message = "Movement type is required")
    private MovementType movementType;

    @NotNull(message = "Quantity is required")
    @Min(value = 1, message = "Quantity must be at least 1")
    private Integer quantity;

    @Size(max = 500, message = "Reason cannot exceed 500 characters")
    private String reason;

    private ReferenceType referenceType;

    @Size(max = 100, message = "Reference ID cannot exceed 100 characters")
    private String referenceId;
}