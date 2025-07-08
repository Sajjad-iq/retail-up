package com.sajjadkademm.retail.auth.dto;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;

/**
 * Login response DTO containing authentication results.
 * Returned after successful authentication.
 * 
 * @author Sajjad Kadem
 * @version 1.0
 * @since 2024-12-19
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Schema(description = "Login response containing authentication token and user details")
public class LoginResponse {

    /**
     * JWT access token
     */
    @Schema(description = "JWT access token", example = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9...")
    private String token;

    /**
     * Token type
     */
    @Schema(description = "Token type", example = "Bearer")
    private String tokenType = "Bearer";

    /**
     * Token expiration time
     */
    @JsonFormat(pattern = "yyyy-MM-dd'T'HH:mm:ss")
    @Schema(description = "Token expiration time", example = "2024-12-20T15:30:00")
    private LocalDateTime expiresAt;

    /**
     * Authenticated user details
     */
    @Schema(description = "Authenticated user details")
    private UserResponse user;

    /**
     * Session information
     */
    @Schema(description = "Session information")
    private SessionInfo session;

    /**
     * Session information class
     */
    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    @Schema(description = "Session information")
    public static class SessionInfo {

        @Schema(description = "Session ID", example = "sess-123456")
        private String sessionId;

        @JsonFormat(pattern = "yyyy-MM-dd'T'HH:mm:ss")
        @Schema(description = "Session creation time", example = "2024-12-19T10:30:00")
        private LocalDateTime createdAt;

        @JsonFormat(pattern = "yyyy-MM-dd'T'HH:mm:ss")
        @Schema(description = "Session expiration time", example = "2024-12-20T10:30:00")
        private LocalDateTime expiresAt;
    }
}