package com.sajjadkademm.retail.auth.dto;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Size;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class ChangePasswordRequest {
    @NotBlank(message = "auth.password.size")
    @Size(min = 8, max = 32, message = "auth.password.size")
    private String oldPassword;
    @NotBlank(message = "auth.password.size")
    @Size(min = 8, max = 32, message = "auth.password.size")
    private String newPassword;
}