package com.sajjadkademm.retail.inventory.dto;

import jakarta.validation.constraints.Size;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class UpdateInventoryRequest {

    @Size(min = 2, max = 100, message = "inventory.name.invalid")
    private String name;

    @Size(max = 500, message = "inventory.description.invalid")
    private String description;

    @Size(max = 255, message = "inventory.location.invalid")
    private String location;

    private Boolean isActive;
}