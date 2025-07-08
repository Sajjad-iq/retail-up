package com.sajjadkademm.retail.auth.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.Email;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Size;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Login request DTO for user authentication.
 * Used for validating login credentials.
 * 
 * @author Sajjad Kadem
 * @version 1.0
 * @since 2024-12-19
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Schema(description = "Login request containing user credentials")
public class LoginRequest {

    /**
     * User's email address
     */
    @NotBlank(message = "Email is required")
    @Email(message = "Please enter a valid email address")
    @Schema(description = "User's email address", example = "admin@retailup.com")
    private String email;

    /**
     * User's password
     */
    @NotBlank(message = "Password is required")
    @Size(min = 6, message = "Password must be at least 6 characters")
    @Schema(description = "User's password", example = "securePassword123!")
    private String password;

    /**
     * Remember me option
     */
    @Schema(description = "Remember me option for extended session", example = "false")
    private Boolean rememberMe = false;
}