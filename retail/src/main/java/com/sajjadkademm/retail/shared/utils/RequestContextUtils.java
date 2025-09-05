package com.sajjadkademm.retail.shared.utils;

import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;
import lombok.experimental.UtilityClass;
import lombok.extern.slf4j.Slf4j;

import jakarta.servlet.http.HttpServletRequest;

/**
 * Utility class for extracting information from HTTP request context
 */
@Slf4j
@UtilityClass
public class RequestContextUtils {

    /**
     * Extract client IP address from current HTTP request
     * Handles X-Forwarded-For header for proxy scenarios
     * 
     * @return Client IP address or "unknown" if not available
     */
    public static String getClientIp() {
        try {
            ServletRequestAttributes attributes = (ServletRequestAttributes) RequestContextHolder
                    .getRequestAttributes();
            if (attributes != null) {
                HttpServletRequest request = attributes.getRequest();
                String xForwardedFor = request.getHeader("X-Forwarded-For");
                if (xForwardedFor != null && !xForwardedFor.isEmpty()) {
                    return xForwardedFor.split(",")[0].trim();
                }
                return request.getRemoteAddr();
            }
        } catch (Exception e) {
            log.debug("Could not extract client IP from request context: {}", e.getMessage());
        }
        return "unknown";
    }

    /**
     * Extract User-Agent header from current HTTP request
     * 
     * @return User-Agent string or "unknown" if not available
     */
    public static String getUserAgent() {
        try {
            ServletRequestAttributes attributes = (ServletRequestAttributes) RequestContextHolder
                    .getRequestAttributes();
            if (attributes != null) {
                HttpServletRequest request = attributes.getRequest();
                String userAgent = request.getHeader("User-Agent");
                return userAgent != null ? userAgent : "unknown";
            }
        } catch (Exception e) {
            log.debug("Could not extract user agent from request context: {}", e.getMessage());
        }
        return "unknown";
    }
}