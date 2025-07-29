package com.sajjadkademm.retail.organizations.dto;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Size;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class CreateOrganizationRequest {

    @NotBlank(message = "Organization name is required")
    @Size(min = 2, max = 100, message = "Organization name must be between 2 and 100 characters")
    private String name;

    @NotBlank(message = "Organization domain is required")
    @Size(min = 3, max = 255, message = "Domain must be between 3 and 255 characters")
    private String domain;

}