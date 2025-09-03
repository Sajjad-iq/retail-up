package com.sajjadkademm.retail.application.dto.organizations;

import com.sajjadkademm.retail.shared.constants.ValidationConstants;
import com.sajjadkademm.retail.shared.enums.OrganizationStatus;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Email;
import jakarta.validation.constraints.Size;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class UpdateOrganizationRequest {

    @NotBlank(message = "{organization.name.empty}")
    @Size(min = ValidationConstants.MIN_ORGANIZATION_NAME_LENGTH, max = ValidationConstants.MAX_ORGANIZATION_NAME_LENGTH, message = "{organization.name.invalid}")
    private String name;

    @NotBlank(message = "{organization.description.invalid}")
    @Size(max = ValidationConstants.MAX_DESCRIPTION_LENGTH, message = "{organization.description.invalid}")
    private String description;

    @NotBlank(message = "{organization.address.invalid}")
    @Size(max = ValidationConstants.MAX_ADDRESS_LENGTH, message = "{organization.address.invalid}")
    private String address;

    @Size(max = ValidationConstants.MAX_PHONE_LENGTH, message = "{auth.phone.too.short}")
    private String phone;

    @Size(min = ValidationConstants.MIN_DOMAIN_LENGTH, max = ValidationConstants.MAX_DOMAIN_LENGTH, message = "{invalid.domain.format}")
    private String domain;

    private OrganizationStatus status;

    public UpdateOrganizationRequest(String domain, String name, String description, String address) {
        this.name = name;
        this.description = description;
        this.address = address;
        this.domain = domain;
    }
}