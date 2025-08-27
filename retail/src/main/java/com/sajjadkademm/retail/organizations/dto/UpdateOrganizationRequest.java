package com.sajjadkademm.retail.organizations.dto;

import com.sajjadkademm.retail.shared.enums.OrganizationStatus;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Size;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class UpdateOrganizationRequest {

    @NotBlank(message = "Organization name is required")
    @Size(min = 2, max = 255, message = "Organization name must be between 2 and 255 characters")
    private String name;

    @NotBlank(message = "Description is required")
    @Size(max = 500, message = "Description must not exceed 500 characters")
    private String description;

    @NotBlank(message = "Address is required")
    @Size(max = 255, message = "Address must not exceed 255 characters")
    private String address;

    @Size(max = 20, message = "Phone must not exceed 20 characters")
    private String phone;

    @Size(min = 3, max = 255, message = "Domain must be between 3 and 255 characters")
    private String domain;

    private OrganizationStatus status;

    public UpdateOrganizationRequest(String domain, String name, String description, String address) {
        this.name = name;
        this.description = description;
        this.address = address;
        this.domain = domain;
    }
}