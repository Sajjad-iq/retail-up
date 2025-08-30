package com.sajjadkademm.retail.organizations.dto;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Size;
import jakarta.validation.constraints.Email;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class CreateOrganizationRequest {

    @NotBlank(message = "{organization.name.empty}")
    @Size(min = 2, max = 100, message = "{organization.name.invalid}")
    private String name;

    @NotBlank(message = "{invalid.domain.format}")
    @Size(min = 3, max = 255, message = "{invalid.domain.format}")
    private String domain;

    @Size(max = 500, message = "{organization.description.invalid}")
    private String description;

    @Size(max = 255, message = "{organization.address.invalid}")
    private String address;

    @NotBlank(message = "{auth.phone.format.wrong}")
    @Size(max = 20, message = "{auth.phone.too.short}")
    private String phone;
    
}