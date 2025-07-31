package com.sajjadkademm.retail.settings.inventory.dto;

import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.Max;
import jakarta.validation.constraints.Pattern;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class InventorySettingsRequest {

    // Stock Management Settings
    private Boolean negativeStockAllowed;
    private Boolean autoStockAdjustment;
    private Boolean barcodeRequired;
    private Boolean skuRequired;
    private Boolean allowZeroPricing;
    private Boolean requireCostPrice;
    private Boolean autoCalculateMarkup;

    @Min(value = 0, message = "Default markup percentage must be at least 0")
    @Max(value = 1000, message = "Default markup percentage cannot exceed 1000")
    private Double defaultMarkupPercentage;

    // Alert Settings
    private Boolean lowStockAlertsEnabled;

    @Min(value = 1, message = "Low stock threshold must be at least 1%")
    @Max(value = 100, message = "Low stock threshold cannot exceed 100%")
    private Integer lowStockThresholdPercentage;

    private Boolean outOfStockAlertsEnabled;
    private Boolean expiryAlertsEnabled;

    @Min(value = 1, message = "Expiry alert days must be at least 1")
    @Max(value = 365, message = "Expiry alert days cannot exceed 365")
    private Integer expiryAlertDays;

    private Boolean overstockAlertsEnabled;

    @Min(value = 100, message = "Overstock threshold must be at least 100%")
    @Max(value = 1000, message = "Overstock threshold cannot exceed 1000%")
    private Integer overstockThresholdPercentage;

    private Boolean alertEmailEnabled;
    private Boolean alertSmsEnabled;

    // Category Settings
    private Boolean autoCategorizationEnabled;

    @Min(value = 1, message = "Max categories per item must be at least 1")
    @Max(value = 10, message = "Max categories per item cannot exceed 10")
    private Integer maxCategoriesPerItem;

    private Boolean requireCategoryAssignment;
    private Boolean allowSubcategories;

    @Min(value = 1, message = "Max subcategory levels must be at least 1")
    @Max(value = 5, message = "Max subcategory levels cannot exceed 5")
    private Integer maxSubcategoryLevels;

    // Reorder Settings
    private Boolean autoReorderEnabled;

    @Pattern(regexp = "^(lead_time_demand|safety_stock|manual)$", message = "Reorder point calculation must be lead_time_demand, safety_stock, or manual")
    private String reorderPointCalculation;

    @Min(value = 0, message = "Safety stock percentage must be at least 0")
    @Max(value = 100, message = "Safety stock percentage cannot exceed 100")
    private Integer safetyStockPercentage;

    @Min(value = 1, message = "Lead time days must be at least 1")
    @Max(value = 365, message = "Lead time days cannot exceed 365")
    private Integer leadTimeDays;

    @Pattern(regexp = "^(eoq|fixed|demand_based)$", message = "Reorder quantity calculation must be eoq, fixed, or demand_based")
    private String reorderQuantityCalculation;

    private Boolean economicOrderQuantityEnabled;

    @Min(value = 1, message = "Min order quantity must be at least 1")
    private Integer minOrderQuantity;

    @Min(value = 1, message = "Max order quantity must be at least 1")
    private Integer maxOrderQuantity;

    // Tracking Settings
    @Pattern(regexp = "^(fifo|lifo|average)$", message = "Inventory tracking method must be fifo, lifo, or average")
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
}