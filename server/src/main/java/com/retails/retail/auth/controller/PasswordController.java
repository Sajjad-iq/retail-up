package com.retails.retail.auth.controller;

import com.retails.retail.auth.dto.*;
import com.retails.retail.auth.service.PasswordService;
import com.retails.retail.common.dto.ApiResponse;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.Map;
import java.util.UUID;

/**
 * Password Management Controller
 * Handles password changes, resets, policies, and security operations
 */
@RestController
@RequestMapping("/api/auth/password")
@RequiredArgsConstructor
@Slf4j
public class PasswordController {

    private final PasswordService passwordService;

    /**
     * Change user password (authenticated user)
     */
    @PostMapping("/change")
    @PreAuthorize("isAuthenticated()")
    public ResponseEntity<ApiResponse<String>> changePassword(
            @Valid @RequestBody ChangePasswordRequest request,
            HttpServletRequest httpRequest) {

        log.info("Change password request received");

        try {
            // This would need proper implementation to get current user ID
            UUID currentUserId = UUID.randomUUID(); // Placeholder
            passwordService.changePassword(currentUserId, request, httpRequest);
            return ResponseEntity.ok(ApiResponse.success("Password changed successfully"));
        } catch (IllegalArgumentException e) {
            log.error("Password change failed: {}", e.getMessage());
            return ResponseEntity.badRequest()
                    .body(ApiResponse.error(e.getMessage(), "PASSWORD_001"));
        } catch (Exception e) {
            log.error("Password change failed", e);
            return ResponseEntity.badRequest()
                    .body(ApiResponse.error("Failed to change password", "PASSWORD_002"));
        }
    }

    /**
     * Reset user password (admin function)
     */
    @PostMapping("/reset/{userId}")
    @PreAuthorize("hasPermission('users', 'manage')")
    public ResponseEntity<ApiResponse<Map<String, String>>> resetPassword(
            @PathVariable UUID userId,
            HttpServletRequest httpRequest) {

        log.info("Password reset request for user: {}", userId);

        try {
            String tempPassword = passwordService.resetPassword(userId, httpRequest);
            Map<String, String> result = Map.of("temporaryPassword", tempPassword);
            return ResponseEntity.ok(ApiResponse.success(result, "Password reset successfully"));
        } catch (IllegalArgumentException e) {
            log.error("Password reset failed: {}", e.getMessage());
            return ResponseEntity.badRequest()
                    .body(ApiResponse.error(e.getMessage(), "PASSWORD_003"));
        } catch (Exception e) {
            log.error("Password reset failed for user: {}", userId, e);
            return ResponseEntity.badRequest()
                    .body(ApiResponse.error("Failed to reset password", "PASSWORD_004"));
        }
    }

    /**
     * Request password reset (forgot password)
     */
    @PostMapping("/forgot")
    public ResponseEntity<ApiResponse<String>> requestPasswordReset(
            @Valid @RequestBody PasswordResetRequestDto request,
            HttpServletRequest httpRequest) {

        log.info("Password reset request for email: {}", request.getEmail());

        try {
            passwordService.requestPasswordReset(request, httpRequest);
            return ResponseEntity.ok(ApiResponse.success("Password reset instructions sent to email"));
        } catch (Exception e) {
            log.error("Password reset request failed", e);
            // Always return success to prevent email enumeration
            return ResponseEntity.ok(ApiResponse.success("Password reset instructions sent to email"));
        }
    }

    /**
     * Confirm password reset with token
     */
    @PostMapping("/reset/confirm")
    public ResponseEntity<ApiResponse<String>> confirmPasswordReset(
            @Valid @RequestBody PasswordResetConfirmDto request,
            HttpServletRequest httpRequest) {

        log.info("Password reset confirmation request");

        try {
            passwordService.confirmPasswordReset(request, httpRequest);
            return ResponseEntity.ok(ApiResponse.success("Password reset successfully"));
        } catch (IllegalArgumentException e) {
            log.error("Password reset confirmation failed: {}", e.getMessage());
            return ResponseEntity.badRequest()
                    .body(ApiResponse.error(e.getMessage(), "PASSWORD_005"));
        } catch (Exception e) {
            log.error("Password reset confirmation failed", e);
            return ResponseEntity.badRequest()
                    .body(ApiResponse.error("Failed to reset password", "PASSWORD_006"));
        }
    }

    /**
     * Force password change for user (admin function)
     */
    @PostMapping("/force-change/{userId}")
    @PreAuthorize("hasPermission('users', 'manage')")
    public ResponseEntity<ApiResponse<String>> forcePasswordChange(
            @PathVariable UUID userId,
            HttpServletRequest httpRequest) {

        log.info("Force password change request for user: {}", userId);

        try {
            passwordService.forcePasswordChange(userId, httpRequest);
            return ResponseEntity.ok(ApiResponse.success("Password change forced successfully"));
        } catch (IllegalArgumentException e) {
            log.error("Force password change failed: {}", e.getMessage());
            return ResponseEntity.badRequest()
                    .body(ApiResponse.error(e.getMessage(), "PASSWORD_007"));
        } catch (Exception e) {
            log.error("Force password change failed for user: {}", userId, e);
            return ResponseEntity.badRequest()
                    .body(ApiResponse.error("Failed to force password change", "PASSWORD_008"));
        }
    }

    /**
     * Bulk force password change (admin function)
     */
    @PostMapping("/force-change/bulk")
    @PreAuthorize("hasPermission('users', 'manage')")
    public ResponseEntity<ApiResponse<Map<String, Object>>> bulkForcePasswordChange(
            @RequestBody List<UUID> userIds,
            HttpServletRequest httpRequest) {

        log.info("Bulk force password change request for {} users", userIds.size());

        try {
            Map<String, Object> result = passwordService.bulkForcePasswordChange(userIds, httpRequest);
            return ResponseEntity.ok(ApiResponse.success(result, "Bulk password change operation completed"));
        } catch (Exception e) {
            log.error("Bulk force password change failed", e);
            return ResponseEntity.badRequest()
                    .body(ApiResponse.error("Failed to perform bulk password change", "PASSWORD_009"));
        }
    }

    /**
     * Check password strength
     */
    @PostMapping("/check-strength")
    public ResponseEntity<ApiResponse<Map<String, Object>>> checkPasswordStrength(
            @RequestBody Map<String, String> request) {

        String password = request.get("password");
        if (password == null || password.isEmpty()) {
            return ResponseEntity.badRequest()
                    .body(ApiResponse.error("Password is required", "PASSWORD_010"));
        }

        try {
            Map<String, Object> result = passwordService.checkPasswordStrength(password);
            return ResponseEntity.ok(ApiResponse.success(result));
        } catch (Exception e) {
            log.error("Password strength check failed", e);
            return ResponseEntity.badRequest()
                    .body(ApiResponse.error("Failed to check password strength", "PASSWORD_011"));
        }
    }

    /**
     * Get password policy information
     */
    @GetMapping("/policy")
    public ResponseEntity<ApiResponse<Map<String, Object>>> getPasswordPolicy() {
        try {
            Map<String, Object> policy = passwordService.getPasswordPolicy();
            return ResponseEntity.ok(ApiResponse.success(policy));
        } catch (Exception e) {
            log.error("Error fetching password policy", e);
            return ResponseEntity.badRequest()
                    .body(ApiResponse.error("Failed to fetch password policy", "PASSWORD_012"));
        }
    }

    /**
     * Check if user's password has expired
     */
    @GetMapping("/expired/{userId}")
    @PreAuthorize("hasPermission('users', 'view') or #userId == authentication.principal.id")
    public ResponseEntity<ApiResponse<Map<String, Boolean>>> checkPasswordExpired(@PathVariable UUID userId) {
        try {
            boolean isExpired = passwordService.isPasswordExpired(userId);
            Map<String, Boolean> result = Map.of("isExpired", isExpired);
            return ResponseEntity.ok(ApiResponse.success(result));
        } catch (Exception e) {
            log.error("Error checking password expiration for user: {}", userId, e);
            return ResponseEntity.badRequest()
                    .body(ApiResponse.error("Failed to check password expiration", "PASSWORD_013"));
        }
    }
}