package com.retails.retail.auth.dto;

import com.retails.retail.auth.lib.ValidationConstants;
import jakarta.validation.constraints.Email;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Pattern;
import jakarta.validation.constraints.Size;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.UUID;

/**
 * Create user request DTO
 * Maps to the frontend UserFormData interface for creation
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class CreateUserRequest {

    @NotBlank(message = ValidationConstants.NAME_REQUIRED)
    @Size(min = ValidationConstants.NAME_MIN_LENGTH, max = ValidationConstants.NAME_MAX_LENGTH, message = ValidationConstants.NAME_SIZE)
    private String name;

    @NotBlank(message = ValidationConstants.EMAIL_REQUIRED)
    @Email(message = ValidationConstants.EMAIL_INVALID)
    private String email;

    @Pattern(regexp = ValidationConstants.PHONE_PATTERN, message = ValidationConstants.PHONE_INVALID)
    private String phone;

    @Size(max = ValidationConstants.DEPARTMENT_MAX_LENGTH, message = ValidationConstants.DEPARTMENT_SIZE)
    private String department;

    @Size(min = ValidationConstants.EMPLOYEE_ID_MIN_LENGTH, max = ValidationConstants.EMPLOYEE_ID_MAX_LENGTH, message = ValidationConstants.EMPLOYEE_ID_SIZE)
    private String employeeId;

    @NotBlank(message = ValidationConstants.ROLE_REQUIRED)
    private UUID roleId;

    @Pattern(regexp = ValidationConstants.STATUS_PATTERN, message = ValidationConstants.STATUS_INVALID)
    private String status = "active";

    @NotBlank(message = ValidationConstants.PASSWORD_REQUIRED)
    @Size(min = ValidationConstants.PASSWORD_MIN_LENGTH, message = ValidationConstants.PASSWORD_SIZE)
    @Pattern(regexp = ValidationConstants.PASSWORD_PATTERN, message = ValidationConstants.PASSWORD_PATTERN_MESSAGE)
    private String password;

    private Boolean mustChangePassword = false;
}