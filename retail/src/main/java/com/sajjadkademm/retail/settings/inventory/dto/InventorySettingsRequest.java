package com.sajjadkademm.retail.settings.inventory.dto;

import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.Max;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Pattern;
import jakarta.validation.constraints.Size;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class InventorySettingsRequest {

    @NotBlank(message = "User ID is required")
    @Pattern(regexp = "^[a-zA-Z0-9_-]+$", message = "User ID must contain only alphanumeric characters, hyphens, and underscores")
    @Size(min = 1, max = 50, message = "User ID must be between 1 and 50 characters")
    private String userId;

    // Stock Management Settings
    @NotNull(message = "Negative stock allowed is required")
    private Boolean negativeStockAllowed;

    @NotNull(message = "Barcode required is required")
    private Boolean barcodeRequired;

    @NotNull(message = "SKU required is required")
    private Boolean skuRequired;

    @NotNull(message = "Require cost price is required")
    private Boolean requireCostPrice;

    // Alert Settings
    @NotNull(message = "Low stock alerts enabled is required")
    private Boolean lowStockAlertsEnabled;

    @Min(value = 1, message = "Low stock threshold must be at least 1")
    @Max(value = 1000, message = "Low stock threshold cannot exceed 1000")
    private Integer lowStockThreshold;

    @NotNull(message = "Out of stock alerts enabled is required")
    private Boolean outOfStockAlertsEnabled;

    @NotNull(message = "Expiry alerts enabled is required")
    private Boolean expiryAlertsEnabled;

    @Min(value = 1, message = "Expiry alert days must be at least 1")
    @Max(value = 365, message = "Expiry alert days cannot exceed 365")
    private Integer expiryAlertDays;

    // Tracking Settings
    @NotNull(message = "Batch tracking enabled is required")
    private Boolean batchTrackingEnabled;

    @NotNull(message = "Expiry date tracking enabled is required")
    private Boolean expiryDateTrackingEnabled;

    // Audit Fields
    private String updatedBy;
}