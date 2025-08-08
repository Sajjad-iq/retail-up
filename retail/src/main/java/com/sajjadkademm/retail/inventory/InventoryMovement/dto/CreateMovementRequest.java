package com.sajjadkademm.retail.inventory.InventoryMovement.dto;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Size;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class CreateMovementRequest {
    
    @NotBlank(message = "User ID is required")
    private String userId;
    
    @NotBlank(message = "Inventory item ID is required")
    private String inventoryItemId;
    
    @NotNull(message = "Movement type is required")
    private MovementType movementType;
    
    @NotNull(message = "Quantity is required")
    private Integer quantity;
    
    @Size(max = 500, message = "Reason cannot exceed 500 characters")
    private String reason;
    
    @Size(max = 50, message = "Reference type cannot exceed 50 characters")
    private String referenceType;
    
    @Size(max = 100, message = "Reference ID cannot exceed 100 characters")
    private String referenceId;
}