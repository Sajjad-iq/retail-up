package com.sajjadkademm.retail.application.dto.organizations;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Size;
import jakarta.validation.constraints.Email;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import static com.sajjadkademm.retail.shared.constants.ValidationConstants.*;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class CreateOrganizationRequest {

    @NotBlank(message = "{organization.name.empty}")
    @Size(min = MIN_ORGANIZATION_NAME_LENGTH, max = MAX_ORGANIZATION_NAME_LENGTH, message = "{organization.name.invalid}")
    private String name;

    @NotBlank(message = "{invalid.domain.format}")
    @Size(min = MIN_DOMAIN_LENGTH, max = MAX_DOMAIN_LENGTH, message = "{invalid.domain.format}")
    private String domain;

    @Size(max = MAX_DESCRIPTION_LENGTH, message = "{organization.description.invalid}")
    private String description;

    @Size(max = MAX_ADDRESS_LENGTH, message = "{organization.address.invalid}")
    private String address;

    @NotBlank(message = "{auth.phone.format.wrong}")
    @Size(max = MAX_PHONE_LENGTH, message = "{auth.phone.too.short}")
    private String phone;
    
}