package com.sajjadkademm.retail.settings.dto;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Size;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class CreateSettingsRequest {

    @NotNull(message = "Organization ID is required")
    private String organizationId;

    @NotBlank(message = "Settings category is required")
    @Size(min = 2, max = 50, message = "Category must be between 2 and 50 characters")
    private String category;

    @NotBlank(message = "Settings key is required")
    @Size(min = 1, max = 100, message = "Key must be between 1 and 100 characters")
    private String key;

    private String value;

    @Size(max = 500, message = "Description must not exceed 500 characters")
    private String description;
}