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
    private Boolean autoStockAdjustment;
    private Boolean barcodeRequired;
    private Boolean skuRequired;
    private Boolean allowZeroPricing;
    private Boolean requireCostPrice;
    private Boolean autoCalculateMarkup;
    private Double defaultMarkupPercentage;

    // Alert Settings
    private Boolean lowStockAlertsEnabled;
    private Integer lowStockThresholdPercentage;
    private Boolean outOfStockAlertsEnabled;
    private Boolean expiryAlertsEnabled;
    private Integer expiryAlertDays;
    private Boolean overstockAlertsEnabled;
    private Integer overstockThresholdPercentage;
    private Boolean alertEmailEnabled;
    private Boolean alertSmsEnabled;

    // Category Settings
    private Boolean autoCategorizationEnabled;
    private Integer maxCategoriesPerItem;
    private Boolean requireCategoryAssignment;
    private Boolean allowSubcategories;
    private Integer maxSubcategoryLevels;

    // Reorder Settings
    private Boolean autoReorderEnabled;
    private String reorderPointCalculation;
    private Integer safetyStockPercentage;
    private Integer leadTimeDays;
    private String reorderQuantityCalculation;
    private Boolean economicOrderQuantityEnabled;
    private Integer minOrderQuantity;
    private Integer maxOrderQuantity;

    // Tracking Settings
    private String inventoryTrackingMethod;
    private Boolean batchTrackingEnabled;
    private Boolean serialNumberTrackingEnabled;
    private Boolean lotNumberRequired;
    private Boolean expiryDateTrackingEnabled;
    private Boolean manufacturingDateTrackingEnabled;

    // Supplier Settings
    private Boolean supplierManagementEnabled;
    private Boolean requireSupplierAssignment;
    private Boolean multipleSuppliersPerItem;
    private Boolean preferredSupplierEnabled;

    // Location Settings
    private Boolean multiLocationEnabled;
    private Boolean warehouseManagementEnabled;
    private Boolean binLocationTrackingEnabled;
    private Boolean requireLocationAssignment;

    private LocalDateTime createdAt;
    private LocalDateTime updatedAt;
}