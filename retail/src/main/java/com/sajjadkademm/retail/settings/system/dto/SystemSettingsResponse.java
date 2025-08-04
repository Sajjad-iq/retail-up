package com.sajjadkademm.retail.settings.system.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class SystemSettingsResponse {

    private String id;
    private String organizationId;

    private Boolean twoFactorAuthEnabled;

    // Backup Settings
    private Boolean autoBackupEnabled;
    private Integer backupRetentionDays;
    private Integer backupFrequencyHours;
    private Boolean backupCompressionEnabled;

    // General Settings
    private String timezone;
    private String language;
    private String currency;

    // Notification Settings
    private Boolean emailNotificationsEnabled;

    // Audit Fields
    private LocalDateTime createdAt;
    private LocalDateTime updatedAt;
}