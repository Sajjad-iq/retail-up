package com.sajjadkademm.retail.settings.inventory.entity;

import jakarta.persistence.Entity;
import jakarta.persistence.Table;
import jakarta.persistence.Id;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Column;
import jakarta.persistence.OneToOne;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.FetchType;
import jakarta.persistence.ForeignKey;
import jakarta.persistence.Index;
import java.time.LocalDateTime;

import com.sajjadkademm.retail.organizations.Organization;

import jakarta.validation.constraints.Max;
import jakarta.validation.constraints.Min;
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
    @NotNull(message = "Negative stock allowed is required")
    private Boolean negativeStockAllowed = false;

    @Column(name = "barcode_required", nullable = false)
    @NotNull(message = "Barcode required is required")
    private Boolean barcodeRequired = true;

    @Column(name = "sku_required", nullable = false)
    @NotNull(message = "SKU required is required")
    private Boolean skuRequired = true;

    @Column(name = "require_cost_price", nullable = false)
    @NotNull(message = "Require cost price is required")
    private Boolean requireCostPrice = true;

    // Alert Settings
    @Column(name = "low_stock_alerts_enabled", nullable = false)
    @NotNull(message = "Low stock alerts enabled is required")
    private Boolean lowStockAlertsEnabled = true;

    @Column(name = "low_stock_threshold", nullable = false)
    @NotNull(message = "Low stock threshold is required")
    @Min(value = 1, message = "Low stock threshold must be at least 1")
    @Max(value = 1000, message = "Low stock threshold cannot exceed 1000")
    private Integer lowStockThreshold = 10;

    @Column(name = "out_of_stock_alerts_enabled", nullable = false)
    @NotNull(message = "Out of stock alerts enabled is required")
    private Boolean outOfStockAlertsEnabled = true;

    @Column(name = "expiry_alerts_enabled", nullable = false)
    @NotNull(message = "Expiry alerts enabled is required")
    private Boolean expiryAlertsEnabled = true;

    @Column(name = "expiry_alert_days", nullable = false)
    @NotNull(message = "Expiry alert days is required")
    @Min(value = 1, message = "Expiry alert days must be at least 1")
    @Max(value = 365, message = "Expiry alert days cannot exceed 365")
    private Integer expiryAlertDays = 30;

    // Tracking Settings
    @Column(name = "batch_tracking_enabled", nullable = false)
    @NotNull(message = "Batch tracking enabled is required")
    private Boolean batchTrackingEnabled = true;

    @Column(name = "expiry_date_tracking_enabled", nullable = false)
    @NotNull(message = "Expiry date tracking enabled is required")
    private Boolean expiryDateTrackingEnabled = true;

    // Audit Fields
    @CreationTimestamp
    @Column(name = "created_at", nullable = false)
    private LocalDateTime createdAt;

    @UpdateTimestamp
    @Column(name = "updated_at", nullable = false)
    private LocalDateTime updatedAt;

    @OneToOne(fetch = FetchType.EAGER)
    @NotNull(message = "Organization is required")
    @JoinColumn(name = "organization_id", insertable = false, updatable = false, foreignKey = @ForeignKey(name = "fk_inventory_settings_organization"))
    private Organization organization;
}