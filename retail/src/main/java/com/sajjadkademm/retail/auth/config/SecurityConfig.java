package com.sajjadkademm.retail.auth.config;

import com.sajjadkademm.retail.auth.security.JwtAuthenticationEntryPoint;
import com.sajjadkademm.retail.auth.security.JwtAuthenticationFilter;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.authentication.dao.DaoAuthenticationProvider;
import org.springframework.security.config.annotation.authentication.configuration.AuthenticationConfiguration;
import org.springframework.security.config.annotation.method.configuration.EnableMethodSecurity;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.builders.WebSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.config.annotation.web.configuration.WebSecurityCustomizer;
import org.springframework.security.config.annotation.web.configurers.AbstractHttpConfigurer;
import org.springframework.security.config.http.SessionCreationPolicy;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.security.web.SecurityFilterChain;
import org.springframework.security.web.authentication.UsernamePasswordAuthenticationFilter;
import org.springframework.web.cors.CorsConfiguration;
import org.springframework.web.cors.CorsConfigurationSource;
import org.springframework.web.cors.UrlBasedCorsConfigurationSource;

import java.util.Arrays;
import java.util.List;

/**
 * Security configuration for JWT-based authentication.
 * Configures Spring Security with JWT tokens and cookie support.
 * 
 * @author Sajjad Kadem
 * @version 1.0
 * @since 2024-12-19
 */
@Configuration
@EnableWebSecurity
@EnableMethodSecurity(prePostEnabled = true)
@RequiredArgsConstructor
public class SecurityConfig {

    private final UserDetailsService userDetailsService;
    private final JwtAuthenticationEntryPoint jwtAuthenticationEntryPoint;
    private final JwtAuthenticationFilter jwtAuthenticationFilter;

    @Value("${security.cors.allowed-origins}")
    private List<String> allowedOrigins;

    @Value("${security.cors.allowed-methods}")
    private List<String> allowedMethods;

    @Value("${security.cors.allowed-headers}")
    private String allowedHeaders;

    @Value("${security.cors.allow-credentials}")
    private boolean allowCredentials;

    /**
     * Configure security filter chain
     */
    @Bean
    public SecurityFilterChain filterChain(HttpSecurity http) throws Exception {
        http
                // Disable CSRF for API usage
                .csrf(AbstractHttpConfigurer::disable)

                // Configure CORS
                .cors(cors -> cors.configurationSource(corsConfigurationSource()))

                // Configure headers for H2 console (development only)
                .headers(headers -> headers.frameOptions().disable())

                // Configure session management
                .sessionManagement(session -> session.sessionCreationPolicy(SessionCreationPolicy.STATELESS))

                // Configure authorization rules
                .authorizeHttpRequests(auth -> auth
                        // Public endpoints - MUST BE FIRST!
                        .requestMatchers(
                                "/api/auth/**",
                                "/api/health",
                                "/actuator/**",
                                "/h2-console/**",
                                // Swagger endpoints
                                "/swagger-ui/**",
                                "/swagger-ui.html",
                                "/swagger-ui.htm", // Handle both .htm and .html
                                "/swagger-ui/index.html",
                                "/v3/api-docs/**",
                                "/v3/api-docs",
                                "/api-docs/**",
                                "/swagger-resources/**",
                                "/webjars/**",
                                "/configuration/**",
                                // Static resources
                                "/favicon.ico",
                                "/*.css",
                                "/*.js",
                                "/*.png",
                                "/*.ico",
                                "/*.html")
                        .permitAll()

                        // Admin endpoints
                        .requestMatchers("/api/admin/**").hasAuthority("admin.full_access")

                        // User management endpoints
                        .requestMatchers("/api/users/**").hasAnyAuthority("users.view", "users.manage")

                        // Permission management endpoints
                        .requestMatchers("/api/permissions/**").hasAnyAuthority("admin.full_access", "users.manage")

                        // Analytics endpoints
                        .requestMatchers("/api/analytics/**").hasAnyAuthority("reports.view", "admin.full_access")

                        // All other requests require authentication
                        .anyRequest().authenticated());

        // Don't add JWT filter for now to test basic access
        // http.addFilterBefore(jwtAuthenticationFilter,
        // UsernamePasswordAuthenticationFilter.class);

        return http.build();
    }

    /**
     * Configure CORS
     */
    @Bean
    public CorsConfigurationSource corsConfigurationSource() {
        CorsConfiguration configuration = new CorsConfiguration();
        configuration.setAllowedOriginPatterns(allowedOrigins);
        configuration.setAllowedMethods(allowedMethods);
        configuration.setAllowedHeaders(Arrays.asList(allowedHeaders.split(",")));
        configuration.setAllowCredentials(allowCredentials);
        configuration.setExposedHeaders(Arrays.asList("Authorization", "Set-Cookie"));

        UrlBasedCorsConfigurationSource source = new UrlBasedCorsConfigurationSource();
        source.registerCorsConfiguration("/**", configuration);
        return source;
    }

    /**
     * Password encoder bean
     */
    @Bean
    public PasswordEncoder passwordEncoder() {
        return new BCryptPasswordEncoder(12);
    }

    /**
     * Authentication manager bean
     */
    @Bean
    public AuthenticationManager authenticationManager(AuthenticationConfiguration config) throws Exception {
        return config.getAuthenticationManager();
    }

    /**
     * DAO authentication provider
     */
    @Bean
    public DaoAuthenticationProvider authenticationProvider() {
        DaoAuthenticationProvider authProvider = new DaoAuthenticationProvider();
        authProvider.setUserDetailsService(userDetailsService);
        authProvider.setPasswordEncoder(passwordEncoder());
        return authProvider;
    }
}