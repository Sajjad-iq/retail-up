package com.sajjadkademm.retail.application.dto.settings;

import com.sajjadkademm.retail.shared.constants.ValidationConstants;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.Max;
import jakarta.validation.constraints.NotNull;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class InventorySettingsRequest {

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

    @Min(value = ValidationConstants.MIN_LOW_STOCK_THRESHOLD, message = "Low stock threshold must be at least 1")
    @Max(value = ValidationConstants.MAX_LOW_STOCK_THRESHOLD, message = "Low stock threshold cannot exceed 1000")
    private Integer lowStockThreshold;

    @NotNull(message = "Out of stock alerts enabled is required")
    private Boolean outOfStockAlertsEnabled;

    @NotNull(message = "Expiry alerts enabled is required")
    private Boolean expiryAlertsEnabled;

    @Min(value = ValidationConstants.MIN_EXPIRY_ALERT_DAYS, message = "Expiry alert days must be at least 1")
    @Max(value = ValidationConstants.MAX_EXPIRY_ALERT_DAYS, message = "Expiry alert days cannot exceed 365")
    private Integer expiryAlertDays;

    // Tracking Settings
    @NotNull(message = "Batch tracking enabled is required")
    private Boolean batchTrackingEnabled;

    @NotNull(message = "Expiry date tracking enabled is required")
    private Boolean expiryDateTrackingEnabled;
}