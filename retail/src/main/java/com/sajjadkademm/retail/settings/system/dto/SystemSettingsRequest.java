package com.sajjadkademm.retail.settings.system.dto;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Size;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class SystemSettingsRequest {

    @NotBlank(message = "Settings key is required")
    @Size(min = 1, max = 100, message = "Key must be between 1 and 100 characters")
    private String key;

    private String value;

    @Size(max = 500, message = "Description must not exceed 500 characters")
    private String description;

    @NotBlank(message = "Setting type is required")
    @Size(min = 2, max = 50, message = "Setting type must be between 2 and 50 characters")
    private String settingType; // e.g., "security", "performance", "backup", "general"

    private Boolean isDefault = false;
}