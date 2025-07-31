package com.sajjadkademm.retail.settings.system.entity;

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
@Table(name = "system_settings", indexes = {
        @Index(name = "idx_system_settings_organization_id", columnList = "organization_id")
})
public class SystemSetting {
    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    private String id;

    @Column(name = "organization_id", nullable = false)
    @NotNull(message = "Organization ID is required")
    private String organizationId;

    @Column(name = "two_factor_auth_enabled", nullable = false)
    private Boolean twoFactorAuthEnabled = false;
    // Backup Settings
    @Column(name = "auto_backup_enabled", nullable = false)
    private Boolean autoBackupEnabled = true;

    @Column(name = "backup_retention_days", nullable = false)
    private Integer backupRetentionDays = 30;

    // General Settings
    @Column(name = "timezone", nullable = false)
    private String timezone = "UTC";

    @Column(name = "language", nullable = false)
    private String language = "en";

    @Column(name = "currency", nullable = false)
    private String currency = "USD";

    // Notification Settings
    @Column(name = "email_notifications_enabled", nullable = false)
    private Boolean emailNotificationsEnabled = true;

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
    @JoinColumn(name = "organization_id", insertable = false, updatable = false, foreignKey = @ForeignKey(name = "fk_system_settings_organization"))
    private Organization organization;
}