package com.retails.retail.auth.dto;

import com.retails.retail.auth.lib.ValidationConstants;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Pattern;
import jakarta.validation.constraints.Size;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Change password request DTO
 * Maps to the frontend PasswordChangeFormData interface
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class ChangePasswordRequest {

    @NotBlank(message = ValidationConstants.CURRENT_PASSWORD_REQUIRED)
    private String currentPassword;

    @NotBlank(message = ValidationConstants.NEW_PASSWORD_REQUIRED)
    @Size(min = ValidationConstants.PASSWORD_MIN_LENGTH, message = ValidationConstants.NEW_PASSWORD_SIZE)
    @Pattern(regexp = ValidationConstants.PASSWORD_PATTERN, message = ValidationConstants.PASSWORD_PATTERN_MESSAGE)
    private String newPassword;

    @NotBlank(message = ValidationConstants.CONFIRM_PASSWORD_REQUIRED)
    private String confirmPassword;
}