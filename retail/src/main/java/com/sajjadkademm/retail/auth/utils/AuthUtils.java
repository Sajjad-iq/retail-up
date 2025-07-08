package com.sajjadkademm.retail.auth.utils;

import jakarta.servlet.http.HttpServletRequest;
import lombok.experimental.UtilityClass;

import java.security.SecureRandom;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.UUID;

/**
 * Utility class for authentication-related helper methods.
 * Provides common utilities for auth operations.
 * 
 * @author Sajjad Kadem
 * @version 1.0
 * @since 2024-12-19
 */
@UtilityClass
public class AuthUtils {

    private static final SecureRandom SECURE_RANDOM = new SecureRandom();
    private static final String ALPHANUMERIC = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789";

    /**
     * Generate unique user ID
     */
    public static String generateUserId() {
        return "user-" + UUID.randomUUID().toString().substring(0, 8);
    }

    /**
     * Generate unique permission ID
     */
    public static String generatePermissionId() {
        return "perm-" + UUID.randomUUID().toString().substring(0, 8);
    }

    /**
     * Generate unique session ID
     */
    public static String generateSessionId() {
        return "sess-" + UUID.randomUUID().toString();
    }

    /**
     * Generate unique activity ID
     */
    public static String generateActivityId() {
        return "act-" + UUID.randomUUID().toString().substring(0, 12);
    }

    /**
     * Generate random token
     */
    public static String generateRandomToken(int length) {
        StringBuilder token = new StringBuilder();
        for (int i = 0; i < length; i++) {
            token.append(ALPHANUMERIC.charAt(SECURE_RANDOM.nextInt(ALPHANUMERIC.length())));
        }
        return token.toString();
    }

    /**
     * Get client IP address from request
     */
    public static String getClientIpAddress(HttpServletRequest request) {
        String xForwardedFor = request.getHeader("X-Forwarded-For");
        if (xForwardedFor != null && !xForwardedFor.isEmpty() && !"unknown".equalsIgnoreCase(xForwardedFor)) {
            return xForwardedFor.split(",")[0].trim();
        }

        String xRealIp = request.getHeader("X-Real-IP");
        if (xRealIp != null && !xRealIp.isEmpty() && !"unknown".equalsIgnoreCase(xRealIp)) {
            return xRealIp;
        }

        String xForwarded = request.getHeader("X-Forwarded");
        if (xForwarded != null && !xForwarded.isEmpty() && !"unknown".equalsIgnoreCase(xForwarded)) {
            return xForwarded;
        }

        String forwarded = request.getHeader("Forwarded");
        if (forwarded != null && !forwarded.isEmpty() && !"unknown".equalsIgnoreCase(forwarded)) {
            return forwarded;
        }

        return request.getRemoteAddr();
    }

    /**
     * Check if IP address is valid
     */
    public static boolean isValidIpAddress(String ipAddress) {
        if (ipAddress == null || ipAddress.isEmpty()) {
            return false;
        }

        // IPv4 pattern
        String ipv4Pattern = "^((25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\\.){3}(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)$";

        // IPv6 pattern (simplified)
        String ipv6Pattern = "^([0-9a-fA-F]{1,4}:){7}[0-9a-fA-F]{1,4}$";

        return ipAddress.matches(ipv4Pattern) || ipAddress.matches(ipv6Pattern);
    }

    /**
     * Mask sensitive data for logging
     */
    public static String maskEmail(String email) {
        if (email == null || email.isEmpty()) {
            return "***";
        }

        int atIndex = email.indexOf('@');
        if (atIndex <= 1) {
            return "***@***";
        }

        String username = email.substring(0, atIndex);
        String domain = email.substring(atIndex);

        if (username.length() <= 2) {
            return "***" + domain;
        }

        return username.charAt(0) + "***" + username.charAt(username.length() - 1) + domain;
    }

    /**
     * Format timestamp for logging
     */
    public static String formatTimestamp(LocalDateTime timestamp) {
        return timestamp.format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"));
    }

    /**
     * Check if string contains only alphanumeric characters
     */
    public static boolean isAlphanumeric(String str) {
        return str != null && str.matches("^[a-zA-Z0-9]+$");
    }

    /**
     * Generate secure random string
     */
    public static String generateSecureRandomString(int length) {
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < length; i++) {
            sb.append(ALPHANUMERIC.charAt(SECURE_RANDOM.nextInt(ALPHANUMERIC.length())));
        }
        return sb.toString();
    }

    /**
     * Check if user agent indicates mobile device
     */
    public static boolean isMobileDevice(String userAgent) {
        if (userAgent == null) {
            return false;
        }

        String userAgentLower = userAgent.toLowerCase();
        return userAgentLower.contains("mobile") ||
                userAgentLower.contains("android") ||
                userAgentLower.contains("iphone") ||
                userAgentLower.contains("ipad") ||
                userAgentLower.contains("blackberry") ||
                userAgentLower.contains("windows phone");
    }

    /**
     * Extract browser name from user agent
     */
    public static String extractBrowserName(String userAgent) {
        if (userAgent == null || userAgent.isEmpty()) {
            return "Unknown";
        }

        if (userAgent.contains("Chrome"))
            return "Chrome";
        if (userAgent.contains("Firefox"))
            return "Firefox";
        if (userAgent.contains("Safari"))
            return "Safari";
        if (userAgent.contains("Edge"))
            return "Edge";
        if (userAgent.contains("Opera"))
            return "Opera";
        if (userAgent.contains("Internet Explorer"))
            return "Internet Explorer";

        return "Unknown";
    }

    /**
     * Check if timestamp is within time window
     */
    public static boolean isWithinTimeWindow(LocalDateTime timestamp, int minutes) {
        LocalDateTime cutoff = LocalDateTime.now().minusMinutes(minutes);
        return timestamp.isAfter(cutoff);
    }

    /**
     * Calculate session duration in minutes
     */
    public static long calculateSessionDurationMinutes(LocalDateTime startTime, LocalDateTime endTime) {
        return java.time.Duration.between(startTime, endTime).toMinutes();
    }
}