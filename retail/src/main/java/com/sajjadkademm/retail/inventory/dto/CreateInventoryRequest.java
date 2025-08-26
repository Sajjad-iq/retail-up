package com.sajjadkademm.retail.inventory.dto;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Size;
import jakarta.validation.constraints.Pattern;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class CreateInventoryRequest {

    @NotBlank(message = "Organization ID is required")
    @Pattern(regexp = "^[a-zA-Z0-9_-]+$", message = "Organization ID must contain only alphanumeric characters, hyphens, and underscores")
    @Size(min = 20, max = 255, message = "Organization ID must be between 20 and 255 characters")
    private String organizationId;

    @NotBlank(message = "Inventory name is required")
    @Size(min = 2, max = 100, message = "Inventory name must be between 2 and 100 characters")
    private String name;

    @Size(max = 500, message = "Description must not exceed 500 characters")
    private String description;

    @Size(max = 255, message = "Location must not exceed 255 characters")
    private String location;
}