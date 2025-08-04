package com.sajjadkademm.retail.settings.inventory.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class InventorySettingsResponse {

    private String id;
    private String organizationId;

    // Stock Management Settings
    private Boolean negativeStockAllowed;
    private Boolean barcodeRequired;
    private Boolean skuRequired;
    private Boolean requireCostPrice;

    // Alert Settings
    private Boolean lowStockAlertsEnabled;
    private Integer lowStockThreshold;
    private Boolean outOfStockAlertsEnabled;
    private Boolean expiryAlertsEnabled;
    private Integer expiryAlertDays;

    // Tracking Settings
    private Boolean batchTrackingEnabled;
    private Boolean expiryDateTrackingEnabled;

    // Audit Fields
    private LocalDateTime createdAt;
    private LocalDateTime updatedAt;
}