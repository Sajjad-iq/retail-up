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

    @Column(name = "barcode_required", nullable = false)
    private Boolean barcodeRequired = true;

    @Column(name = "sku_required", nullable = false)
    private Boolean skuRequired = true;

    @Column(name = "require_cost_price", nullable = false)
    private Boolean requireCostPrice = true;

    // Alert Settings
    @Column(name = "low_stock_alerts_enabled", nullable = false)
    private Boolean lowStockAlertsEnabled = true;

    @Column(name = "low_stock_threshold", nullable = false)
    private Integer lowStockThreshold = 10;

    @Column(name = "out_of_stock_alerts_enabled", nullable = false)
    private Boolean outOfStockAlertsEnabled = true;

    @Column(name = "expiry_alerts_enabled", nullable = false)
    private Boolean expiryAlertsEnabled = true;

    @Column(name = "expiry_alert_days", nullable = false)
    private Integer expiryAlertDays = 30;

    // Tracking Settings
    @Column(name = "batch_tracking_enabled", nullable = false)
    private Boolean batchTrackingEnabled = true;

    @Column(name = "expiry_date_tracking_enabled", nullable = false)
    private Boolean expiryDateTrackingEnabled = true;

    // Audit Fields
    @Column(name = "updated_by", nullable = true)
    private String updatedBy;

    @CreationTimestamp
    @Column(name = "created_at", nullable = false)
    private LocalDateTime createdAt;

    @UpdateTimestamp
    @Column(name = "updated_at", nullable = false)
    private LocalDateTime updatedAt;

    @OneToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "organization_id", insertable = false, updatable = false, foreignKey = @ForeignKey(name = "fk_inventory_settings_organization"))
    private Organization organization;
}