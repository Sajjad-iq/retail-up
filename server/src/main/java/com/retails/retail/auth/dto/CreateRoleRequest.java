package com.retails.retail.auth.dto;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotEmpty;
import jakarta.validation.constraints.Pattern;
import jakarta.validation.constraints.Size;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;
import java.util.UUID;

/**
 * Create role request DTO
 * Maps to the frontend RoleFormData interface
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class CreateRoleRequest {

    @NotBlank(message = "Role name is required")
    @Size(min = 2, max = 30, message = "Role name must be between 2 and 30 characters")
    @Pattern(regexp = "^[a-zA-Z0-9\\s\\-]+$", message = "Role name can only contain letters, numbers, spaces, and hyphens")
    private String name;

    @Size(max = 200, message = "Description must not exceed 200 characters")
    private String description;

    @Pattern(regexp = "^#[0-9A-Fa-f]{6}$", message = "Please enter a valid hex color code")
    private String color;

    @NotEmpty(message = "At least one permission must be selected")
    private List<UUID> permissionIds;
}