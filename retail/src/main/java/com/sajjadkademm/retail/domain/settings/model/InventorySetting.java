package com.sajjadkademm.retail.domain.settings.model;

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

import com.sajjadkademm.retail.domain.organization.model.Organization;
import com.sajjadkademm.retail.shared.constants.ValidationConstants;

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
    @NotNull(message = "{inventory.id.required}")
    private String organizationId;

    // Stock Management Settings
    @Column(name = "negative_stock_allowed", nullable = false)
    @NotNull(message = "{invalid.inventory.data}")
    private Boolean negativeStockAllowed = false;

    @Column(name = "barcode_required", nullable = false)
    @NotNull(message = "{invalid.inventory.data}")
    private Boolean barcodeRequired = true;

    @Column(name = "sku_required", nullable = false)
    @NotNull(message = "{invalid.inventory.data}")
    private Boolean skuRequired = true;

    @Column(name = "require_cost_price", nullable = false)
    @NotNull(message = "{invalid.inventory.data}")
    private Boolean requireCostPrice = true;

    // Alert Settings
    @Column(name = "low_stock_alerts_enabled", nullable = false)
    @NotNull(message = "{invalid.inventory.data}")
    private Boolean lowStockAlertsEnabled = true;

    @Column(name = "low_stock_threshold", nullable = false)
    @NotNull(message = "{invalid.inventory.data}")
    @Min(value = ValidationConstants.MIN_STOCK_VALUE, message = "{stock.cannot.be.negative}")
    @Max(value = ValidationConstants.MAX_LOW_STOCK_THRESHOLD, message = "{invalid.inventory.data}")
    private Integer lowStockThreshold = 10;

    @Column(name = "out_of_stock_alerts_enabled", nullable = false)
    @NotNull(message = "{invalid.inventory.data}")
    private Boolean outOfStockAlertsEnabled = true;

    @Column(name = "expiry_alerts_enabled", nullable = false)
    @NotNull(message = "{invalid.inventory.data}")
    private Boolean expiryAlertsEnabled = true;

    @Column(name = "expiry_alert_days", nullable = false)
    @NotNull(message = "{invalid.inventory.data}")
    @Min(value = ValidationConstants.MIN_EXPIRY_ALERT_DAYS, message = "{invalid.inventory.data}")
    @Max(value = ValidationConstants.MAX_EXPIRY_ALERT_DAYS, message = "{invalid.inventory.data}")
    private Integer expiryAlertDays = 30;

    // Tracking Settings
    @Column(name = "batch_tracking_enabled", nullable = false)
    @NotNull(message = "{invalid.inventory.data}")
    private Boolean batchTrackingEnabled = true;

    @Column(name = "expiry_date_tracking_enabled", nullable = false)
    @NotNull(message = "{invalid.inventory.data}")
    private Boolean expiryDateTrackingEnabled = true;

    // Audit Fields
    @CreationTimestamp
    @Column(name = "created_at", nullable = false)
    private LocalDateTime createdAt;

    @UpdateTimestamp
    @Column(name = "updated_at", nullable = false)
    private LocalDateTime updatedAt;

    @OneToOne(fetch = FetchType.EAGER)
    @JoinColumn(name = "organization_id", insertable = false, updatable = false, foreignKey = @ForeignKey(name = "fk_inventory_settings_organization"))
    private Organization organization;
}