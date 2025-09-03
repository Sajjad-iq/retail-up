package com.sajjadkademm.retail.application.dto.auth;

import com.sajjadkademm.retail.shared.constants.ValidationConstants;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Size;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class LoginRequest {
    @NotBlank(message = "auth.email.or.phone.required")
    private String emailOrPhone;
    @NotBlank(message = "auth.password.size")
    @Size(min = ValidationConstants.MIN_PASSWORD_LENGTH, max = ValidationConstants.MAX_PASSWORD_LENGTH, message = "Password must be between 8 and 32 characters")
    private String password;
}