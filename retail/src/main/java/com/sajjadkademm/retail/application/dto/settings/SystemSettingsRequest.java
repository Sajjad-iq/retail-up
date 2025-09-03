package com.sajjadkademm.retail.application.dto.settings;

import com.sajjadkademm.retail.shared.constants.ValidationConstants;
import jakarta.validation.constraints.Min;

import com.sajjadkademm.retail.shared.enums.Currency;

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

    @Min(value = ValidationConstants.MIN_BACKUP_RETENTION_DAYS, message = "Backup retention days must be at least 1")
    @Max(value = ValidationConstants.MAX_BACKUP_RETENTION_DAYS, message = "Backup retention days cannot exceed 365")
    private Integer backupRetentionDays;

    // General Settings
    @Pattern(regexp = ValidationConstants.TIMEZONE_PATTERN, message = "Invalid timezone format")
    @NotNull(message = "Timezone is required")
    private String timezone;

    @Pattern(regexp = ValidationConstants.LANGUAGE_PATTERN, message = "Language must be a 2-letter code")
    @NotNull(message = "Language is required")
    private String language;

    @Pattern(regexp = ValidationConstants.CURRENCY_PATTERN, message = "Currency must be a 3-letter code")
    @NotNull(message = "Currency is required")
    private Currency currency;

    // Notification Settings
    @NotNull(message = "Email notifications enabled is required")
    private Boolean emailNotificationsEnabled;
}