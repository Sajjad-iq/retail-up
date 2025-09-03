package com.sajjadkademm.retail.application.dto.inventory;

import com.sajjadkademm.retail.shared.constants.ValidationConstants;
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
    @Pattern(regexp = ValidationConstants.ID_PATTERN, message = "inventory.id.invalid")
    @Size(min = ValidationConstants.MIN_ID_LENGTH, max = ValidationConstants.MAX_ID_LENGTH, message = "inventory.id.invalid")
    private String organizationId;

    @NotBlank(message = "inventory.name.required")
    @Size(min = ValidationConstants.MIN_ORGANIZATION_NAME_LENGTH, max = ValidationConstants.MAX_NAME_LENGTH, message = "inventory.name.invalid")
    private String name;

    @Size(max = ValidationConstants.MAX_DESCRIPTION_LENGTH, message = "inventory.description.invalid")
    private String description;

    @Size(max = ValidationConstants.MAX_ADDRESS_LENGTH, message = "inventory.location.invalid")
    private String location;
}