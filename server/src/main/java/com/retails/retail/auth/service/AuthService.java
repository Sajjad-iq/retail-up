package com.retails.retail.auth.service;

import com.retails.retail.auth.dto.*;
import com.retails.retail.auth.entity.User;
import com.retails.retail.auth.entity.UserActivity;
import com.retails.retail.auth.repository.UserRepository;
import com.retails.retail.auth.repository.UserActivityRepository;
import com.retails.retail.auth.security.JwtTokenProvider;
import jakarta.servlet.http.HttpServletRequest;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDateTime;
import java.util.UUID;

/**
 * Service for authentication operations
 * Handles login, logout, password reset, and token management
 */
@Service
@RequiredArgsConstructor
@Slf4j
@Transactional
public class AuthService extends BaseAuthService {

    private final UserRepository userRepository;
    private final UserActivityRepository userActivityRepository;
    private final AuthenticationManager authenticationManager;
    private final JwtTokenProvider jwtTokenProvider;
    private final PasswordEncoder passwordEncoder;

    /**
     * Authenticate user and generate JWT token
     */
    public LoginResponse login(LoginRequest request, HttpServletRequest httpRequest) {
        log.info("Login attempt for email: {}", request.getEmail());

        try {
            // Authenticate user
            Authentication authentication = authenticationManager.authenticate(
                    new UsernamePasswordAuthenticationToken(request.getEmail(), request.getPassword()));

            // Get user details
            User user = userRepository.findByEmailIgnoreCase(request.getEmail())
                    .orElseThrow(() -> new BadCredentialsException("Invalid credentials"));

            // Check if user is active
            if (!user.isActive()) {
                throw new BadCredentialsException("Account is not active");
            }

            // Generate JWT token
            String token = jwtTokenProvider.generateToken(authentication, request.getRememberMe());
            LocalDateTime expiresAt = jwtTokenProvider.getExpirationDateFromToken(token);

            // Update last login
            user.setLastLoginAt(LocalDateTime.now());
            userRepository.save(user);

            // Log activity
            logActivity(user.getId(), UserActivity.UserAction.LOGIN, null, "Successful login",
                    httpRequest);

            // Convert to DTO
            UserDto userDto = UserDto.fromEntity(user);

            log.info("Successful login for user: {} ({})", user.getName(), user.getEmail());

            return LoginResponse.success(token, expiresAt, userDto, user.getMustChangePassword());

        } catch (Exception e) {
            // Log failed login attempt
            userRepository.findByEmailIgnoreCase(request.getEmail())
                    .ifPresent(user -> logActivity(user.getId(), UserActivity.UserAction.ACCESS_DENIED,
                            null, "Failed login attempt", httpRequest));

            log.warn("Failed login attempt for email: {} - {}", request.getEmail(), e.getMessage());
            throw new BadCredentialsException("Invalid credentials");
        }
    }

    /**
     * Logout user and invalidate token
     */
    public void logout(HttpServletRequest httpRequest) {
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        if (authentication != null && authentication.isAuthenticated()) {
            String email = authentication.getName();
            userRepository.findByEmailIgnoreCase(email)
                    .ifPresent(user -> {
                        logActivity(user.getId(), UserActivity.UserAction.LOGOUT, null, "User logout",
                                httpRequest);
                        log.info("User logged out: {} ({})", user.getName(), user.getEmail());
                    });
        }

        SecurityContextHolder.clearContext();
    }

    /**
     * Change user password
     */
    public void changePassword(UUID userId, ChangePasswordRequest request, HttpServletRequest httpRequest) {
        User user = userRepository.findById(userId)
                .orElseThrow(() -> new IllegalArgumentException("User not found"));

        // Verify current password
        if (!passwordEncoder.matches(request.getCurrentPassword(), user.getPasswordHash())) {
            throw new BadCredentialsException("Current password is incorrect");
        }

        // Check if new password is different
        if (passwordEncoder.matches(request.getNewPassword(), user.getPasswordHash())) {
            throw new IllegalArgumentException("New password must be different from current password");
        }

        // Validate password confirmation
        if (!request.getNewPassword().equals(request.getConfirmPassword())) {
            throw new IllegalArgumentException("Password confirmation does not match");
        }

        // Update password
        user.setPasswordHash(passwordEncoder.encode(request.getNewPassword()));
        user.setMustChangePassword(false);
        userRepository.save(user);

        // Log activity
        logActivity(user.getId(), UserActivity.UserAction.CHANGE_PASSWORD, null, "Password changed",
                httpRequest);

        log.info("Password changed for user: {} ({})", user.getName(), user.getEmail());
    }

    /**
     * Reset user password (admin function)
     */
    public String resetPassword(UUID userId, HttpServletRequest httpRequest) {
        User user = userRepository.findById(userId)
                .orElseThrow(() -> new IllegalArgumentException("User not found"));

        // Generate temporary password
        String tempPassword = generateTemporaryPassword();

        // Update user
        user.setPasswordHash(passwordEncoder.encode(tempPassword));
        user.setMustChangePassword(true);
        userRepository.save(user);

        // Log activity
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        String adminEmail = authentication != null ? authentication.getName() : "system";

        logActivity(user.getId(), UserActivity.UserAction.RESET_PASSWORD, null,
                "Password reset by: " + adminEmail, httpRequest);

        log.info("Password reset for user: {} ({}) by: {}", user.getName(), user.getEmail(), adminEmail);

        return tempPassword;
    }

    /**
     * Get current authenticated user
     */
    public UserDto getCurrentUser() {
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        if (authentication == null || !authentication.isAuthenticated()) {
            return null;
        }

        String email = authentication.getName();
        User user = userRepository.findByEmailIgnoreCase(email)
                .orElse(null);

        return user != null ? UserDto.fromEntity(user) : null;
    }

    /**
     * Refresh JWT token
     */
    public LoginResponse refreshToken(String token) {
        if (jwtTokenProvider.canTokenBeRefreshed(token)) {
            String newToken = jwtTokenProvider.refreshToken(token);
            LocalDateTime expiresAt = jwtTokenProvider.getExpirationDateFromToken(newToken);

            String email = jwtTokenProvider.getUsernameFromToken(token);
            User user = userRepository.findByEmailIgnoreCase(email)
                    .orElseThrow(() -> new IllegalArgumentException("User not found"));

            UserDto userDto = UserDto.fromEntity(user);

            return LoginResponse.success(newToken, expiresAt, userDto, user.getMustChangePassword());
        }

        throw new IllegalArgumentException("Token cannot be refreshed");
    }

    /**
     * Generate temporary password
     */
    private String generateTemporaryPassword() {
        String chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789!@#$%^&*";
        StringBuilder password = new StringBuilder();

        for (int i = 0; i < 12; i++) {
            password.append(chars.charAt((int) (Math.random() * chars.length())));
        }

        return password.toString();
    }
}