package com.retails.retail.auth.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;

/**
 * Login response DTO
 * Contains authentication token and user information
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class LoginResponse {

    private String token;
    private String tokenType = "Bearer";
    private LocalDateTime expiresAt;
    private UserDto user;
    private boolean mustChangePassword;

    /**
     * Create successful login response
     */
    public static LoginResponse success(String token, LocalDateTime expiresAt, UserDto user,
            boolean mustChangePassword) {
        return LoginResponse.builder()
                .token(token)
                .expiresAt(expiresAt)
                .user(user)
                .mustChangePassword(mustChangePassword)
                .build();
    }
}