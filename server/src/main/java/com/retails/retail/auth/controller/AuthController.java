package com.retails.retail.auth.controller;

import com.retails.retail.auth.dto.*;
import com.retails.retail.auth.service.AuthService;
import com.retails.retail.common.dto.ApiResponse;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import java.util.UUID;

/**
 * Authentication Controller
 * Handles login, logout, password management, and session operations
 */
@RestController
@RequestMapping("/api/auth")
@RequiredArgsConstructor
@Slf4j
public class AuthController {

    private final AuthService authService;

    /**
     * User login endpoint
     */
    @PostMapping("/login")
    public ResponseEntity<ApiResponse<LoginResponse>> login(
            @Valid @RequestBody LoginRequest request,
            HttpServletRequest httpRequest) {

        log.info("Login request received for email: {}", request.getEmail());

        try {
            LoginResponse response = authService.login(request, httpRequest);
            return ResponseEntity.ok(ApiResponse.success(response, "Login successful"));
        } catch (Exception e) {
            log.error("Login failed for email: {}", request.getEmail(), e);
            return ResponseEntity.badRequest()
                    .body(ApiResponse.error("Invalid credentials", "AUTH_001"));
        }
    }

    /**
     * User logout endpoint
     */
    @PostMapping("/logout")
    @PreAuthorize("isAuthenticated()")
    public ResponseEntity<ApiResponse<String>> logout(HttpServletRequest httpRequest) {
        log.info("Logout request received");

        try {
            authService.logout(httpRequest);
            return ResponseEntity.ok(ApiResponse.success("Logout successful"));
        } catch (Exception e) {
            log.error("Logout failed", e);
            return ResponseEntity.badRequest()
                    .body(ApiResponse.error("Logout failed", "AUTH_002"));
        }
    }

    /**
     * Get current authenticated user
     */
    @GetMapping("/me")
    @PreAuthorize("isAuthenticated()")
    public ResponseEntity<ApiResponse<UserDto>> getCurrentUser() {
        try {
            UserDto user = authService.getCurrentUser();
            if (user != null) {
                return ResponseEntity.ok(ApiResponse.success(user));
            } else {
                return ResponseEntity.badRequest()
                        .body(ApiResponse.error("User not found", "AUTH_003"));
            }
        } catch (Exception e) {
            log.error("Failed to get current user", e);
            return ResponseEntity.badRequest()
                    .body(ApiResponse.error("Failed to get user information", "AUTH_004"));
        }
    }

    /**
     * Change password endpoint
     */
    @PostMapping("/change-password")
    @PreAuthorize("isAuthenticated()")
    public ResponseEntity<ApiResponse<String>> changePassword(
            @Valid @RequestBody ChangePasswordRequest request,
            HttpServletRequest httpRequest) {

        log.info("Change password request received");

        try {
            UserDto currentUser = authService.getCurrentUser();
            if (currentUser == null) {
                return ResponseEntity.badRequest()
                        .body(ApiResponse.error("User not authenticated", "AUTH_005"));
            }

            authService.changePassword(currentUser.getId(), request, httpRequest);
            return ResponseEntity.ok(ApiResponse.success("Password changed successfully"));
        } catch (Exception e) {
            log.error("Password change failed", e);
            return ResponseEntity.badRequest()
                    .body(ApiResponse.error(e.getMessage(), "AUTH_006"));
        }
    }

    /**
     * Reset user password (admin function)
     */
    @PostMapping("/reset-password/{userId}")
    @PreAuthorize("hasPermission('users', 'manage')")
    public ResponseEntity<ApiResponse<String>> resetPassword(
            @PathVariable UUID userId,
            HttpServletRequest httpRequest) {

        log.info("Password reset request for user: {}", userId);

        try {
            String tempPassword = authService.resetPassword(userId, httpRequest);
            return ResponseEntity.ok(ApiResponse.success(tempPassword, "Password reset successful"));
        } catch (Exception e) {
            log.error("Password reset failed for user: {}", userId, e);
            return ResponseEntity.badRequest()
                    .body(ApiResponse.error(e.getMessage(), "AUTH_007"));
        }
    }

    /**
     * Refresh JWT token
     */
    @PostMapping("/refresh")
    public ResponseEntity<ApiResponse<LoginResponse>> refreshToken(@RequestParam String token) {
        log.info("Token refresh request received");

        try {
            LoginResponse response = authService.refreshToken(token);
            return ResponseEntity.ok(ApiResponse.success(response, "Token refreshed successfully"));
        } catch (Exception e) {
            log.error("Token refresh failed", e);
            return ResponseEntity.badRequest()
                    .body(ApiResponse.error("Token refresh failed", "AUTH_008"));
        }
    }

    /**
     * Validate token endpoint
     */
    @GetMapping("/validate")
    @PreAuthorize("isAuthenticated()")
    public ResponseEntity<ApiResponse<String>> validateToken() {
        return ResponseEntity.ok(ApiResponse.success("Token is valid"));
    }
}