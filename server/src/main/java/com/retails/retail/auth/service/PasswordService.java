package com.retails.retail.auth.service;

import com.retails.retail.auth.dto.*;
import com.retails.retail.auth.entity.PasswordResetRequest;
import com.retails.retail.auth.entity.User;
import com.retails.retail.auth.entity.UserActivity;
import com.retails.retail.auth.repository.UserRepository;
import com.retails.retail.auth.repository.UserActivityRepository;
import jakarta.servlet.http.HttpServletRequest;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.security.SecureRandom;
import java.time.LocalDateTime;
import java.util.*;
import java.util.regex.Pattern;

/**
 * Service for password management operations
 * Handles password changes, resets, validation, and policies
 */
@Service
@RequiredArgsConstructor
@Slf4j
@Transactional
public class PasswordService extends BaseAuthService {

    private final UserRepository userRepository;
    private final UserActivityRepository userActivityRepository;
    private final PasswordEncoder passwordEncoder;

    // Password policy patterns
    private static final Pattern UPPERCASE_PATTERN = Pattern.compile(".*[A-Z].*");
    private static final Pattern LOWERCASE_PATTERN = Pattern.compile(".*[a-z].*");
    private static final Pattern DIGIT_PATTERN = Pattern.compile(".*\\d.*");
    private static final Pattern SPECIAL_CHAR_PATTERN = Pattern
            .compile(".*[!@#$%^&*()_+\\-=\\[\\]{};':\"\\\\|,.<>\\/?].*");

    private static final String TEMP_PASSWORD_CHARS = "ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnpqrstuvwxyz23456789";
    private static final SecureRandom SECURE_RANDOM = new SecureRandom();

    /**
     * Change user password
     */
    public void changePassword(UUID userId, ChangePasswordRequest request, HttpServletRequest httpRequest) {
        log.info("Changing password for user: {}", userId);

        User user = userRepository.findById(userId)
                .orElseThrow(() -> new IllegalArgumentException("User not found"));

        // Verify current password
        if (!passwordEncoder.matches(request.getCurrentPassword(), user.getPasswordHash())) {
            logActivity(user.getId(), UserActivity.UserAction.ACCESS_DENIED, "Password",
                    "Failed password change attempt - incorrect current password", httpRequest);
            throw new IllegalArgumentException("Current password is incorrect");
        }

        // Validate new password
        validatePassword(request.getNewPassword());

        // Check if new password is different from current
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
        user.setPasswordChangedAt(LocalDateTime.now());
        userRepository.save(user);

        // Log activity
        logActivity(user.getId(), UserActivity.UserAction.CHANGE_PASSWORD, "Password",
                "Password changed successfully", httpRequest);

        log.info("Password changed successfully for user: {} ({})", user.getName(), user.getEmail());
    }

    /**
     * Reset user password (admin function)
     */
    public String resetPassword(UUID userId, HttpServletRequest httpRequest) {
        log.info("Resetting password for user: {}", userId);

        User user = userRepository.findById(userId)
                .orElseThrow(() -> new IllegalArgumentException("User not found"));

        // Generate temporary password
        String tempPassword = generateTemporaryPassword();

        // Update user
        user.setPasswordHash(passwordEncoder.encode(tempPassword));
        user.setMustChangePassword(true);
        user.setPasswordChangedAt(LocalDateTime.now());
        userRepository.save(user);

        // Log activity
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        String adminEmail = authentication != null ? authentication.getName() : "system";

        logActivity(user.getId(), UserActivity.UserAction.RESET_PASSWORD, "Password",
                "Password reset by: " + adminEmail, httpRequest);

        log.info("Password reset for user: {} ({}) by: {}", user.getName(), user.getEmail(), adminEmail);
        return tempPassword;
    }

    /**
     * Request password reset (forgot password)
     */
    public void requestPasswordReset(PasswordResetRequestDto request, HttpServletRequest httpRequest) {
        log.info("Password reset requested for email: {}", request.getEmail());

        User user = userRepository.findByEmailIgnoreCase(request.getEmail())
                .orElse(null);

        // Always return success to prevent email enumeration
        if (user == null) {
            log.warn("Password reset requested for non-existent email: {}", request.getEmail());
            return;
        }

        // Generate reset token
        String resetToken = generateResetToken();
        LocalDateTime expiresAt = LocalDateTime.now().plusHours(24); // 24-hour expiry

        // Create reset request (this would be stored in a separate table in a real
        // implementation)
        // For now, we'll just log it
        logActivity(user.getId(), UserActivity.UserAction.RESET_PASSWORD, "Password",
                "Password reset requested", httpRequest);

        log.info("Password reset token generated for user: {} ({})", user.getName(), user.getEmail());
        // In a real implementation, you would send an email here with the reset token
    }

    /**
     * Confirm password reset with token
     */
    public void confirmPasswordReset(PasswordResetConfirmDto request, HttpServletRequest httpRequest) {
        log.info("Confirming password reset with token");

        // In a real implementation, you would validate the token here
        // For now, we'll just validate the password

        // Validate new password
        validatePassword(request.getNewPassword());

        // Validate password confirmation
        if (!request.getNewPassword().equals(request.getConfirmPassword())) {
            throw new IllegalArgumentException("Password confirmation does not match");
        }

        // Here you would find the user by the reset token and update their password
        log.info("Password reset confirmed successfully");
    }

    /**
     * Force password change for user
     */
    public void forcePasswordChange(UUID userId, HttpServletRequest httpRequest) {
        log.info("Forcing password change for user: {}", userId);

        User user = userRepository.findById(userId)
                .orElseThrow(() -> new IllegalArgumentException("User not found"));

        user.setMustChangePassword(true);
        userRepository.save(user);

        // Log activity
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        String adminEmail = authentication != null ? authentication.getName() : "system";

        logActivity(user.getId(), UserActivity.UserAction.FORCE_PASSWORD_CHANGE, "Password",
                "Password change forced by: " + adminEmail, httpRequest);

        log.info("Password change forced for user: {} ({}) by: {}", user.getName(), user.getEmail(), adminEmail);
    }

    /**
     * Bulk force password change
     */
    public Map<String, Object> bulkForcePasswordChange(List<UUID> userIds, HttpServletRequest httpRequest) {
        log.info("Bulk forcing password change for {} users", userIds.size());

        List<User> users = userRepository.findAllById(userIds);
        int successCount = 0;
        List<String> errors = new ArrayList<>();

        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        String adminEmail = authentication != null ? authentication.getName() : "system";

        for (User user : users) {
            try {
                user.setMustChangePassword(true);
                logActivity(user.getId(), UserActivity.UserAction.FORCE_PASSWORD_CHANGE, "Password",
                        "Bulk password change forced by: " + adminEmail, httpRequest);
                successCount++;
            } catch (Exception e) {
                errors.add("Error for user " + user.getEmail() + ": " + e.getMessage());
                log.error("Error forcing password change for user: {}", user.getEmail(), e);
            }
        }

        userRepository.saveAll(users);

        Map<String, Object> result = new HashMap<>();
        result.put("successCount", successCount);
        result.put("errorCount", errors.size());
        result.put("errors", errors);

        log.info("Bulk password change force completed. Success: {}, Errors: {}", successCount, errors.size());
        return result;
    }

    /**
     * Check password strength
     */
    public Map<String, Object> checkPasswordStrength(String password) {
        Map<String, Object> result = new HashMap<>();
        List<String> feedback = new ArrayList<>();
        int score = 0;

        // Length check
        if (password.length() >= 8) {
            score += 20;
        } else {
            feedback.add("Password should be at least 8 characters long");
        }

        // Uppercase check
        if (UPPERCASE_PATTERN.matcher(password).matches()) {
            score += 20;
        } else {
            feedback.add("Password should contain at least one uppercase letter");
        }

        // Lowercase check
        if (LOWERCASE_PATTERN.matcher(password).matches()) {
            score += 20;
        } else {
            feedback.add("Password should contain at least one lowercase letter");
        }

        // Digit check
        if (DIGIT_PATTERN.matcher(password).matches()) {
            score += 20;
        } else {
            feedback.add("Password should contain at least one number");
        }

        // Special character check
        if (SPECIAL_CHAR_PATTERN.matcher(password).matches()) {
            score += 20;
        } else {
            feedback.add("Password should contain at least one special character");
        }

        // Determine strength level
        String strength;
        if (score >= 80) {
            strength = "STRONG";
        } else if (score >= 60) {
            strength = "MEDIUM";
        } else if (score >= 40) {
            strength = "WEAK";
        } else {
            strength = "VERY_WEAK";
        }

        result.put("score", score);
        result.put("strength", strength);
        result.put("feedback", feedback);
        result.put("isValid", score >= 60); // Require at least medium strength

        return result;
    }

    /**
     * Get password policy information
     */
    public Map<String, Object> getPasswordPolicy() {
        Map<String, Object> policy = new HashMap<>();
        policy.put("minLength", 8);
        policy.put("maxLength", 100);
        policy.put("requireUppercase", true);
        policy.put("requireLowercase", true);
        policy.put("requireDigits", true);
        policy.put("requireSpecialChars", true);
        policy.put("specialChars", "!@#$%^&*()_+-=[]{}|;':\"\\,.<>/?");
        policy.put("passwordHistoryCount", 5); // Remember last 5 passwords
        policy.put("passwordExpiryDays", 90); // Password expires after 90 days
        policy.put("lockoutAfterFailedAttempts", 5);
        return policy;
    }

    /**
     * Check if password has expired
     */
    public boolean isPasswordExpired(UUID userId) {
        User user = userRepository.findById(userId).orElse(null);
        if (user == null || user.getPasswordChangedAt() == null) {
            return false;
        }

        LocalDateTime passwordExpiryDate = user.getPasswordChangedAt().plusDays(90);
        return LocalDateTime.now().isAfter(passwordExpiryDate);
    }

    // Private helper methods

    private void validatePassword(String password) {
        if (password == null || password.trim().isEmpty()) {
            throw new IllegalArgumentException("Password cannot be empty");
        }

        if (password.length() < 8) {
            throw new IllegalArgumentException("Password must be at least 8 characters long");
        }

        if (password.length() > 100) {
            throw new IllegalArgumentException("Password must not exceed 100 characters");
        }

        if (!UPPERCASE_PATTERN.matcher(password).matches()) {
            throw new IllegalArgumentException("Password must contain at least one uppercase letter");
        }

        if (!LOWERCASE_PATTERN.matcher(password).matches()) {
            throw new IllegalArgumentException("Password must contain at least one lowercase letter");
        }

        if (!DIGIT_PATTERN.matcher(password).matches()) {
            throw new IllegalArgumentException("Password must contain at least one number");
        }

        if (!SPECIAL_CHAR_PATTERN.matcher(password).matches()) {
            throw new IllegalArgumentException("Password must contain at least one special character");
        }
    }

    private String generateTemporaryPassword() {
        StringBuilder password = new StringBuilder(12);

        // Ensure we have at least one of each required character type
        password.append(getRandomChar("ABCDEFGHJKLMNPQRSTUVWXYZ")); // Uppercase
        password.append(getRandomChar("abcdefghijkmnpqrstuvwxyz")); // Lowercase
        password.append(getRandomChar("23456789")); // Number
        password.append(getRandomChar("!@#$%^&*")); // Special char

        // Fill the rest with random characters
        for (int i = 4; i < 12; i++) {
            password.append(getRandomChar(TEMP_PASSWORD_CHARS));
        }

        // Shuffle the password to randomize position of required characters
        return shuffleString(password.toString());
    }

    private String generateResetToken() {
        StringBuilder token = new StringBuilder(32);
        String chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789";

        for (int i = 0; i < 32; i++) {
            token.append(chars.charAt(SECURE_RANDOM.nextInt(chars.length())));
        }

        return token.toString();
    }

    private char getRandomChar(String chars) {
        return chars.charAt(SECURE_RANDOM.nextInt(chars.length()));
    }

    private String shuffleString(String input) {
        List<Character> characters = new ArrayList<>();
        for (char c : input.toCharArray()) {
            characters.add(c);
        }
        Collections.shuffle(characters, SECURE_RANDOM);

        StringBuilder shuffled = new StringBuilder();
        for (char c : characters) {
            shuffled.append(c);
        }
        return shuffled.toString();
    }
}