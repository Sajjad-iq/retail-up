package com.sajjadkademm.retail.application.dto.auth;

import com.sajjadkademm.retail.shared.constants.ValidationConstants;
import jakarta.validation.constraints.Email;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Size;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class RegisterRequest {
    @NotBlank(message = "user.name.empty")
    @Size(min = ValidationConstants.MIN_ORGANIZATION_NAME_LENGTH, max = ValidationConstants.MAX_NAME_LENGTH, message = "user.name.size")
    private String name;

    @Email(message = "user.email.invalid.format")
    @Size(max = ValidationConstants.MAX_EMAIL_LENGTH, message = "user.email.size")
    private String email;

    @Size(min = ValidationConstants.MIN_PHONE_LENGTH, max = ValidationConstants.MAX_PHONE_LENGTH, message = "auth.phone.too.short")
    @NotBlank(message = "auth.phone.format.wrong")
    private String phone;
    @NotBlank(message = "auth.password.size")
    @Size(min = ValidationConstants.MIN_PASSWORD_LENGTH, max = ValidationConstants.MAX_PASSWORD_LENGTH, message = "auth.password.size")
    private String password;

}