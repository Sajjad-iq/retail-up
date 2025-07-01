package com.retails.retail.auth.dto;

import jakarta.validation.constraints.Email;
import jakarta.validation.constraints.Pattern;
import jakarta.validation.constraints.Size;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.UUID;

/**
 * Update user request DTO
 * Maps to the frontend UserFormData interface for editing
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class UpdateUserRequest {

    @Size(min = 2, max = 50, message = "Name must be between 2 and 50 characters")
    private String name;

    @Email(message = "Please enter a valid email address")
    private String email;

    @Pattern(regexp = "^\\+?[\\d\\s\\-\\(\\)]{10,}$", message = "Please enter a valid phone number")
    private String phone;

    @Size(max = 50, message = "Department must not exceed 50 characters")
    private String department;

    @Size(min = 2, max = 20, message = "Employee ID must be between 2 and 20 characters")
    private String employeeId;

    private UUID roleId;

    @Pattern(regexp = "^(active|inactive|suspended|pending)$", message = "Invalid status value")
    private String status;

    @Size(min = 8, message = "Password must be at least 8 characters")
    @Pattern(regexp = "^(?=.*[a-z])(?=.*[A-Z])(?=.*\\d)(?=.*[!@#$%^&*(),.?\":{}|<>]).{8,}$", message = "Password must contain uppercase, lowercase, number, and special character")
    private String password;

    private Boolean mustChangePassword;
}