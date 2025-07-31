package com.sajjadkademm.retail.settings.inventory.entity;

import jakarta.persistence.Entity;
import jakarta.persistence.Table;
import jakarta.persistence.Id;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Column;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.FetchType;
import jakarta.persistence.ForeignKey;
import jakarta.persistence.Index;
import java.time.LocalDateTime;

import com.sajjadkademm.retail.organizations.Organization;

import jakarta.validation.constraints.NotNull;

import org.hibernate.annotations.CreationTimestamp;
import org.hibernate.annotations.UpdateTimestamp;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.Builder;

@Entity
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Table(name = "inventory_settings", indexes = {
        @Index(name = "idx_inventory_settings_organization_id", columnList = "organization_id")
})
public class InventorySetting {
    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    private String id;

    @Column(name = "organization_id", nullable = false)
    @NotNull(message = "Organization ID is required")
    private String organizationId;

    // Stock Management Settings
    @Column(name = "negative_stock_allowed", nullable = false)
    private Boolean negativeStockAllowed = false;

    @Column(name = "auto_stock_adjustment", nullable = false)
    private Boolean autoStockAdjustment = true;

    @Column(name = "barcode_required", nullable = false)
    private Boolean barcodeRequired = true;

    @Column(name = "sku_required", nullable = false)
    private Boolean skuRequired = true;

    @Column(name = "allow_zero_pricing", nullable = false)
    private Boolean allowZeroPricing = false;

    @Column(name = "require_cost_price", nullable = false)
    private Boolean requireCostPrice = true;

    @Column(name = "auto_calculate_markup", nullable = false)
    private Boolean autoCalculateMarkup = false;

    @Column(name = "default_markup_percentage", nullable = false)
    private Double defaultMarkupPercentage = 30.0;

    // Alert Settings
    @Column(name = "low_stock_alerts_enabled", nullable = false)
    private Boolean lowStockAlertsEnabled = true;

    @Column(name = "low_stock_threshold_percentage", nullable = false)
    private Integer lowStockThresholdPercentage = 10;

    @Column(name = "out_of_stock_alerts_enabled", nullable = false)
    private Boolean outOfStockAlertsEnabled = true;

    @Column(name = "expiry_alerts_enabled", nullable = false)
    private Boolean expiryAlertsEnabled = true;

    @Column(name = "expiry_alert_days", nullable = false)
    private Integer expiryAlertDays = 30;

    @Column(name = "overstock_alerts_enabled", nullable = false)
    private Boolean overstockAlertsEnabled = false;

    @Column(name = "overstock_threshold_percentage", nullable = false)
    private Integer overstockThresholdPercentage = 200;

    @Column(name = "alert_email_enabled", nullable = false)
    private Boolean alertEmailEnabled = true;

    @Column(name = "alert_sms_enabled", nullable = false)
    private Boolean alertSmsEnabled = false;

    // Category Settings
    @Column(name = "auto_categorization_enabled", nullable = false)
    private Boolean autoCategorizationEnabled = false;

    @Column(name = "max_categories_per_item", nullable = false)
    private Integer maxCategoriesPerItem = 3;

    @Column(name = "require_category_assignment", nullable = false)
    private Boolean requireCategoryAssignment = true;

    @Column(name = "allow_subcategories", nullable = false)
    private Boolean allowSubcategories = true;

    @Column(name = "max_subcategory_levels", nullable = false)
    private Integer maxSubcategoryLevels = 3;

    // Reorder Settings
    @Column(name = "auto_reorder_enabled", nullable = false)
    private Boolean autoReorderEnabled = false;

    @Column(name = "reorder_point_calculation", nullable = false)
    private String reorderPointCalculation = "lead_time_demand"; // lead_time_demand, safety_stock, manual

    @Column(name = "safety_stock_percentage", nullable = false)
    private Integer safetyStockPercentage = 20;

    @Column(name = "lead_time_days", nullable = false)
    private Integer leadTimeDays = 7;

    @Column(name = "reorder_quantity_calculation", nullable = false)
    private String reorderQuantityCalculation = "eoq"; // eoq, fixed, demand_based

    @Column(name = "economic_order_quantity_enabled", nullable = false)
    private Boolean economicOrderQuantityEnabled = true;

    @Column(name = "min_order_quantity", nullable = false)
    private Integer minOrderQuantity = 1;

    @Column(name = "max_order_quantity", nullable = false)
    private Integer maxOrderQuantity = 10000;

    // Tracking Settings
    @Column(name = "inventory_tracking_method", nullable = false)
    private String inventoryTrackingMethod = "fifo"; // fifo, lifo, average

    @Column(name = "batch_tracking_enabled", nullable = false)
    private Boolean batchTrackingEnabled = true;

    @Column(name = "serial_number_tracking_enabled", nullable = false)
    private Boolean serialNumberTrackingEnabled = false;

    @Column(name = "lot_number_required", nullable = false)
    private Boolean lotNumberRequired = false;

    @Column(name = "expiry_date_tracking_enabled", nullable = false)
    private Boolean expiryDateTrackingEnabled = true;

    @Column(name = "manufacturing_date_tracking_enabled", nullable = false)
    private Boolean manufacturingDateTrackingEnabled = false;

    // Supplier Settings
    @Column(name = "supplier_management_enabled", nullable = false)
    private Boolean supplierManagementEnabled = true;

    @Column(name = "require_supplier_assignment", nullable = false)
    private Boolean requireSupplierAssignment = false;

    @Column(name = "multiple_suppliers_per_item", nullable = false)
    private Boolean multipleSuppliersPerItem = true;

    @Column(name = "preferred_supplier_enabled", nullable = false)
    private Boolean preferredSupplierEnabled = true;

    // Location Settings
    @Column(name = "multi_location_enabled", nullable = false)
    private Boolean multiLocationEnabled = false;

    @Column(name = "warehouse_management_enabled", nullable = false)
    private Boolean warehouseManagementEnabled = false;

    @Column(name = "bin_location_tracking_enabled", nullable = false)
    private Boolean binLocationTrackingEnabled = false;

    @Column(name = "require_location_assignment", nullable = false)
    private Boolean requireLocationAssignment = false;

    @CreationTimestamp
    @Column(name = "created_at", nullable = false)
    private LocalDateTime createdAt;

    @UpdateTimestamp
    @Column(name = "updated_at", nullable = false)
    private LocalDateTime updatedAt;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "organization_id", insertable = false, updatable = false, foreignKey = @ForeignKey(name = "fk_inventory_settings_organization"))
    private Organization organization;
}