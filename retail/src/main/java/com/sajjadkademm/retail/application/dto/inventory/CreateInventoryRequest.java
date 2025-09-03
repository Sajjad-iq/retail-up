package com.sajjadkademm.retail.application.dto.inventory;

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

    @NotBlank(message = "inventory.id.required")
    @Pattern(regexp = "^[a-zA-Z0-9_-]+$", message = "inventory.id.invalid")
    @Size(min = 20, max = 255, message = "inventory.id.invalid")
    private String organizationId;

    @NotBlank(message = "inventory.name.required")
    @Size(min = 2, max = 100, message = "inventory.name.invalid")
    private String name;

    @Size(max = 500, message = "inventory.description.invalid")
    private String description;

    @Size(max = 255, message = "inventory.location.invalid")
    private String location;
}