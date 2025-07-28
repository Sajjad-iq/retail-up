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
    @NotBlank(message = "User ID is required")
    private String userId;
    @NotBlank(message = "Old password is required")
    @Size(min = 8, max = 32, message = "Password must be between 8 and 32 characters")
    private String oldPassword;
    @NotBlank(message = "New password is required")
    @Size(min = 8, max = 32, message = "Password must be between 8 and 32 characters")
    private String newPassword;
}