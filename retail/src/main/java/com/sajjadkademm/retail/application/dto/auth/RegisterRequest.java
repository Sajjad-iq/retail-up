package com.sajjadkademm.retail.application.dto.auth;

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
    @Size(min = 3, max = 255, message = "user.name.size")
    private String name;

    @Email(message = "user.email.invalid.format")
    private String email;

    @Size(min = 10, max = 20, message = "auth.phone.too.short")
    @NotBlank(message = "auth.phone.format.wrong")
    private String phone;
    @NotBlank(message = "auth.password.size")
    @Size(min = 8, max = 32, message = "auth.password.size")
    private String password;

}