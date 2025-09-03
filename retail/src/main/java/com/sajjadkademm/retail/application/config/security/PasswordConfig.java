package com.sajjadkademm.retail.application.config.security;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.security.crypto.password.PasswordEncoder;

/**
 * Password encoding configuration.
 * Handles password encoding/hashing for authentication.
 * 
 * Separation of Concerns:
 * - Only handles password encoding configuration
 * - Separated from main security configuration for better maintainability
 * - Allows easy switching of password encoding strategies
 * 
 * @author Sajjad Kadem
 * @version 1.0
 * @since 2024-12-19
 */
@Configuration
public class PasswordConfig {

    /**
     * Configure BCrypt password encoder with default strength (10 rounds).
     * BCrypt is recommended for secure password hashing due to:
     * - Built-in salt generation
     * - Adaptive cost (configurable work factor)
     * - Resistance to rainbow table attacks
     * 
     * @return BCryptPasswordEncoder instance
     */
    @Bean
    public BCryptPasswordEncoder bCryptPasswordEncoder() {
        return new BCryptPasswordEncoder();
    }

    /**
     * Provide PasswordEncoder interface implementation using BCrypt.
     * This allows Spring Security to use the encoder automatically.
     * 
     * @return PasswordEncoder instance
     */
    @Bean
    public PasswordEncoder passwordEncoder() {
        return bCryptPasswordEncoder();
    }

    /**
     * Alternative: Configure BCrypt with custom strength
     * Higher values = more secure but slower
     * 
     * @return BCryptPasswordEncoder with custom strength
     */
    // @Bean
    // public PasswordEncoder strongPasswordEncoder() {
    //     return new BCryptPasswordEncoder(12); // Higher strength for sensitive applications
    // }
}