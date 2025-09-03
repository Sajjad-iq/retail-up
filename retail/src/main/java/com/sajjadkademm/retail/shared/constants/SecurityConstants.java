package com.sajjadkademm.retail.shared.constants;

/**
 * Security-related constants used throughout the application.
 * Centralizes security configuration values for consistency.
 */
public final class SecurityConstants {
    
    private SecurityConstants() {
        // Utility class - prevent instantiation
    }
    
    // JWT Constants
    public static final long JWT_EXPIRATION = 86400000L; // 24 hours in milliseconds
    public static final String JWT_SECRET_DEFAULT = "retail-app-jwt-secret-key-2024";
    public static final String JWT_HEADER = "Authorization";
    public static final String JWT_PREFIX = "Bearer ";
    
    // Security Headers
    public static final String USER_ID_HEADER = "X-User-ID";
    public static final String ORGANIZATION_ID_HEADER = "X-Organization-ID";
    
    
    // Rate Limiting
    public static final int MAX_LOGIN_ATTEMPTS = 5;
    public static final long LOCKOUT_DURATION_MINUTES = 30;
}