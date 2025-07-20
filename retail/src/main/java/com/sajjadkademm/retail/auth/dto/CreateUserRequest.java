package com.sajjadkademm.retail.auth.dto;

import com.sajjadkademm.retail.auth.entities.User;
import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

/**
 * Create user request DTO for user creation.
 * Contains all required fields for creating a new user.
 * 
 * @author Sajjad Kadem
 * @version 1.0
 * @since 2024-12-19
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Schema(description = "Create user request containing user details")
public class CreateUserRequest {

    /**
     * User's display name
     */
    @NotBlank(message = "Name is required")
    @Size(min = 2, max = 50, message = "Name must be between 2 and 50 characters")
    @Schema(description = "User's display name", example = "John Doe")
    private String name;

    /**
     * User's email address
     */
    @NotBlank(message = "Email is required")
    @Email(message = "Please enter a valid email address")
    @Schema(description = "User's email address", example = "john.doe@retailup.com")
    private String email;

    /**
     * User's password
     */
    @NotBlank(message = "Password is required")
    @Schema(description = "User's password", example = "SecurePassword123!")
    private String password;

    /**
     * User's phone number
     */
    @Schema(description = "User's phone number", example = "+1-555-0123")
    private String phone;

    /**
     * User's department
     */
    @Size(max = 100, message = "Department must not exceed 100 characters")
    @Schema(description = "User's department", example = "Sales")
    private String department;

    /**
     * Employee ID
     */
    @Size(min = 2, max = 20, message = "Employee ID must be between 2 and 20 characters")
    @Schema(description = "Employee ID", example = "EMP001")
    private String employeeId;

    /**
     * Permission IDs to assign to the user
     */
    @NotNull(message = "Permission IDs are required")
    @Schema(description = "Permission IDs to assign to the user", example = "[\"perm-pos-001\", \"perm-inv-001\"]")
    private List<String> permissionIds;

    /**
     * User's status
     */
    @NotNull(message = "Status is required")
    @Schema(description = "User's status", example = "ACTIVE")
    private User.UserStatus status;

    /**
     * Whether user must change password on next login
     */
    @NotNull(message = "Must change password flag is required")
    @Schema(description = "Whether user must change password on next login", example = "false")
    private Boolean mustChangePassword;
}