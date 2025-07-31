package com.sajjadkademm.retail.settings.dto;

import jakarta.validation.constraints.Size;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class UpdateSettingsRequest {

    private String value;

    @Size(max = 500, message = "Description must not exceed 500 characters")
    private String description;
}