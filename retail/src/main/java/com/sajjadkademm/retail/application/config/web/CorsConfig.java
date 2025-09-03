package com.sajjadkademm.retail.application.config.web;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.cors.CorsConfiguration;
import org.springframework.web.cors.CorsConfigurationSource;
import org.springframework.web.cors.UrlBasedCorsConfigurationSource;

import java.util.Arrays;

/**
 * CORS (Cross-Origin Resource Sharing) configuration.
 * Handles cross-origin requests for the web API.
 * 
 * Separation of Concerns:
 * - Only handles CORS-related configurations
 * - Separated from security configuration for better maintainability
 * 
 * @author Sajjad Kadem
 * @version 1.0
 * @since 2024-12-19
 */
@Configuration
public class CorsConfig {

    /**
     * Configure CORS to allow cross-origin requests
     * 
     * @return CorsConfigurationSource configured for development and production
     */
    @Bean
    public CorsConfigurationSource corsConfigurationSource() {
        CorsConfiguration configuration = new CorsConfiguration();

        // Allow all origins during development
        // TODO: Restrict to specific domains in production
        configuration.setAllowedOriginPatterns(Arrays.asList("*"));

        // Allow common HTTP methods
        configuration.setAllowedMethods(Arrays.asList("GET", "POST", "PUT", "DELETE", "OPTIONS", "PATCH"));

        // Allow all headers
        configuration.setAllowedHeaders(Arrays.asList("*"));

        // Allow credentials for authentication
        configuration.setAllowCredentials(true);

        // Expose necessary headers to client
        configuration.setExposedHeaders(Arrays.asList(
                "Authorization",
                "Content-Type", 
                "X-Requested-With", 
                "Access-Control-Allow-Origin", 
                "Access-Control-Allow-Credentials",
                "Allowed-Origins"));

        // Max age for preflight requests
        configuration.setMaxAge(3600L);

        UrlBasedCorsConfigurationSource source = new UrlBasedCorsConfigurationSource();
        source.registerCorsConfiguration("/**", configuration);
        return source;
    }
}