package com.sajjadkademm.retail.settings.system.dto;

import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.Max;
import jakarta.validation.constraints.Pattern;
import jakarta.validation.constraints.NotNull;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class SystemSettingsRequest {

    @NotNull(message = "Two factor auth enabled is required")
    private Boolean twoFactorAuthEnabled;

    // Backup Settings
    @NotNull(message = "Auto backup enabled is required")
    private Boolean autoBackupEnabled;

    @Min(value = 1, message = "Backup retention days must be at least 1")
    @Max(value = 365, message = "Backup retention days cannot exceed 365")
    private Integer backupRetentionDays;

    // General Settings
    @Pattern(regexp = "^[A-Za-z_]+/[A-Za-z_]+$", message = "Invalid timezone format")
    @NotNull(message = "Timezone is required")
    private String timezone;

    @Pattern(regexp = "^[a-z]{2}$", message = "Language must be a 2-letter code")
    @NotNull(message = "Language is required")
    private String language;

    @Pattern(regexp = "^[A-Z]{3}$", message = "Currency must be a 3-letter code")
    @NotNull(message = "Currency is required")
    private String currency;

    // Notification Settings
    @NotNull(message = "Email notifications enabled is required")
    private Boolean emailNotificationsEnabled;

    // Audit Fields
    private String updatedBy;
}