package com.sajjadkademm.retail.organizations.dto;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Size;
import jakarta.validation.constraints.Email;
import jakarta.validation.constraints.Pattern;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class CreateOrganizationRequest {

    @NotBlank(message = "User ID is required")
    @Pattern(regexp = "^[a-zA-Z0-9_-]+$", message = "User ID must contain only alphanumeric characters, hyphens, and underscores")
    @Size(min = 20, max = 255, message = "User ID must be between 20 and 255 characters")
    private String userId;

    @NotBlank(message = "Organization name is required")
    @Size(min = 2, max = 100, message = "Organization name must be between 2 and 100 characters")
    private String name;

    @NotBlank(message = "Organization domain is required")
    @Size(min = 3, max = 255, message = "Domain must be between 3 and 255 characters")
    private String domain;

    @Size(max = 500, message = "Description must not exceed 500 characters")
    private String description;

    @Size(max = 255, message = "Address must not exceed 255 characters")
    private String address;

    @NotBlank(message = "Phone is required")
    @Size(max = 20, message = "Phone must not exceed 20 characters")
    private String phone;

    @Email(message = "Email should be valid")
    @Size(max = 255, message = "Email must not exceed 255 characters")
    private String email;
}