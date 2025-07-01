package com.retails.retail.auth.service;

import com.retails.retail.auth.entity.User;
import com.retails.retail.auth.entity.UserActivity;
import com.retails.retail.auth.repository.UserActivityRepository;
import com.retails.retail.auth.repository.UserRepository;
import jakarta.servlet.http.HttpServletRequest;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;

import java.time.LocalDateTime;
import java.util.UUID;

/**
 * Base service class for auth services
 * Contains common utility methods to avoid code duplication
 */
@Slf4j
public abstract class BaseAuthService {

    @Autowired
    protected UserActivityRepository userActivityRepository;

    @Autowired
    protected UserRepository userRepository;

    /**
     * Log user activity
     */
    protected void logActivity(UUID userId, UserActivity.UserAction action, String resource,
            String details, HttpServletRequest request) {
        try {
            UserActivity activity = UserActivity.builder()
                    .userId(userId)
                    .action(action)
                    .resource(resource)
                    .details(details)
                    .ipAddress(getClientIpAddress(request))
                    .userAgent(getUserAgent(request))
                    .timestamp(LocalDateTime.now())
                    .build();

            userActivityRepository.save(activity);
        } catch (Exception e) {
            log.error("Failed to log user activity", e);
        }
    }

    /**
     * Log activity with current authenticated user
     */
    protected void logActivity(UserActivity.UserAction action, String resource, String details,
            HttpServletRequest request) {
        try {
            Authentication auth = SecurityContextHolder.getContext().getAuthentication();
            if (auth != null && auth.isAuthenticated()) {
                String email = auth.getName();
                User user = userRepository.findByEmailIgnoreCase(email).orElse(null);
                if (user != null) {
                    logActivity(user.getId(), action, resource, details, request);
                }
            }
        } catch (Exception e) {
            log.error("Failed to log activity", e);
        }
    }

    /**
     * Extract client IP address from request
     */
    protected String getClientIpAddress(HttpServletRequest request) {
        String xForwardedFor = request.getHeader("X-Forwarded-For");
        if (xForwardedFor != null && !xForwardedFor.isEmpty()) {
            return xForwardedFor.split(",")[0].trim();
        }

        String xRealIp = request.getHeader("X-Real-IP");
        if (xRealIp != null && !xRealIp.isEmpty()) {
            return xRealIp;
        }

        return request.getRemoteAddr();
    }

    /**
     * Extract user agent from request
     */
    protected String getUserAgent(HttpServletRequest request) {
        return request.getHeader("User-Agent");
    }
}