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

import jakarta.validation.constraints.NotBlank;
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
        @Index(name = "idx_inventory_settings_organization_id", columnList = "organization_id"),
        @Index(name = "idx_inventory_settings_key", columnList = "key_name"),
        @Index(name = "idx_inventory_settings_organization_key", columnList = "organization_id,key_name")
})
public class InventorySetting {
    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    private String id;

    @Column(name = "organization_id", nullable = false)
    @NotNull(message = "Organization ID is required")
    private String organizationId;

    @Column(name = "key_name", nullable = false)
    @NotBlank(message = "Setting key is required")
    private String key;

    @Column(name = "value", nullable = true, columnDefinition = "TEXT")
    private String value;

    @Column(name = "description", nullable = true)
    private String description;

    @Column(name = "setting_type", nullable = false)
    @NotBlank(message = "Setting type is required")
    private String settingType; // e.g., "stock", "alerts", "categories", "reorder"

    @Column(name = "is_default", nullable = false)
    private Boolean isDefault = false;

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