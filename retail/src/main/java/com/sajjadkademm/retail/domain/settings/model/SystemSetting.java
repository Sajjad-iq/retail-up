package com.sajjadkademm.retail.domain.settings.model;

import com.sajjadkademm.retail.shared.constants.ValidationConstants;
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
import jakarta.persistence.Enumerated;
import jakarta.persistence.EnumType;

import java.time.LocalDateTime;

import com.sajjadkademm.retail.domain.organization.model.Organization;
import com.sajjadkademm.retail.shared.enums.Currency;

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

    // Backup Settings
    @Column(name = "auto_backup_enabled", nullable = false)
    @NotNull(message = "Auto backup enabled is required")
    private Boolean autoBackupEnabled = true;

    @Column(name = "backup_retention_days", nullable = false)
    @Min(value = ValidationConstants.MIN_BACKUP_RETENTION_DAYS, message = "Backup retention days must be at least 1")
    @Max(value = ValidationConstants.MAX_BACKUP_RETENTION_DAYS, message = "Backup retention days cannot exceed 365")
    private Integer backupRetentionDays = 30;

    // General Settings
    @Column(name = "timezone", nullable = false)
    @NotNull(message = "Timezone is required")
    private String timezone = "UTC";

    @Column(name = "language", nullable = false)
    @NotNull(message = "Language is required")
    private String language = "en";

    @Column(name = "currency", nullable = false)
    @NotNull(message = "Currency is required")
    @Enumerated(EnumType.STRING)
    private Currency currency;

    // Notification Settings
    @Column(name = "email_notifications_enabled", nullable = false)
    @NotNull(message = "Email notifications enabled is required")
    private Boolean emailNotificationsEnabled = true;

    // Audit Fields
    @CreationTimestamp
    @Column(name = "created_at", nullable = false)
    private LocalDateTime createdAt;

    @UpdateTimestamp
    @Column(name = "updated_at", nullable = false)
    private LocalDateTime updatedAt;

    @OneToOne(fetch = FetchType.EAGER)
    @JoinColumn(name = "organization_id", insertable = false, updatable = false, foreignKey = @ForeignKey(name = "fk_system_settings_organization"))
    private Organization organization;
}