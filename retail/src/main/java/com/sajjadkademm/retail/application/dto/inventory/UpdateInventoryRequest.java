package com.sajjadkademm.retail.application.dto.inventory;

import com.sajjadkademm.retail.shared.constants.ValidationConstants;
import jakarta.validation.constraints.Size;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class UpdateInventoryRequest {

    @Size(min = ValidationConstants.MIN_ORGANIZATION_NAME_LENGTH, max = ValidationConstants.MAX_NAME_LENGTH, message = "inventory.name.invalid")
    private String name;

    @Size(max = ValidationConstants.MAX_DESCRIPTION_LENGTH, message = "inventory.description.invalid")
    private String description;

    @Size(max = ValidationConstants.MAX_ADDRESS_LENGTH, message = "inventory.location.invalid")
    private String location;

    private Boolean isActive;
}