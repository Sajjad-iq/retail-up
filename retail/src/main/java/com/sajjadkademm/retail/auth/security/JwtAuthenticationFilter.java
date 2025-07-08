package com.sajjadkademm.retail.auth.security;

import com.sajjadkademm.retail.auth.services.JwtService;
import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.Cookie;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.web.authentication.WebAuthenticationDetailsSource;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;
import org.springframework.web.filter.OncePerRequestFilter;

import java.io.IOException;
import java.util.List;
import java.util.stream.Collectors;

/**
 * JWT Authentication Filter for processing JWT tokens in requests.
 * Supports both Authorization header and cookie-based authentication.
 * 
 * @author Sajjad Kadem
 * @version 1.0
 * @since 2024-12-19
 */
@Slf4j
@Component
@RequiredArgsConstructor
public class JwtAuthenticationFilter extends OncePerRequestFilter {

    private final JwtService jwtService;
    private final UserDetailsService userDetailsService;

    @Override
    protected void doFilterInternal(HttpServletRequest request,
            HttpServletResponse response,
            FilterChain filterChain) throws ServletException, IOException {

        try {
            String jwt = extractJwtFromRequest(request);

            if (StringUtils.hasText(jwt) && jwtService.validateToken(jwt)) {
                String email = jwtService.extractEmail(jwt);

                if (email != null && SecurityContextHolder.getContext().getAuthentication() == null) {
                    UserDetails userDetails = userDetailsService.loadUserByUsername(email);

                    if (jwtService.validateToken(jwt, (com.sajjadkademm.retail.auth.entities.User) userDetails)) {
                        // Extract permissions from JWT token
                        List<String> permissions = jwtService.extractPermissions(jwt);
                        List<SimpleGrantedAuthority> authorities = permissions.stream()
                                .map(SimpleGrantedAuthority::new)
                                .collect(Collectors.toList());

                        UsernamePasswordAuthenticationToken authToken = new UsernamePasswordAuthenticationToken(
                                userDetails,
                                null,
                                authorities);

                        authToken.setDetails(new WebAuthenticationDetailsSource().buildDetails(request));
                        SecurityContextHolder.getContext().setAuthentication(authToken);

                        log.debug("Successfully authenticated user: {}", email);
                    }
                }
            }
        } catch (Exception e) {
            log.error("Cannot set user authentication: {}", e.getMessage());
        }

        filterChain.doFilter(request, response);
    }

    /**
     * Extract JWT token from request (header or cookie)
     */
    private String extractJwtFromRequest(HttpServletRequest request) {
        // First, try to get token from Authorization header
        String bearerToken = request.getHeader("Authorization");
        if (StringUtils.hasText(bearerToken) && bearerToken.startsWith("Bearer ")) {
            return bearerToken.substring(7);
        }

        // If not found in header, try to get from cookie (for development)
        if (request.getCookies() != null) {
            for (Cookie cookie : request.getCookies()) {
                if ("auth_token".equals(cookie.getName())) {
                    return cookie.getValue();
                }
            }
        }

        return null;
    }

    /**
     * Check if request should be filtered
     */
    @Override
    protected boolean shouldNotFilter(HttpServletRequest request) {
        String path = request.getRequestURI();

        log.debug("Checking if should filter path: {}", path);

        // Skip filter for public endpoints
        boolean shouldSkip = path.startsWith("/api/auth/login") ||
                path.startsWith("/api/auth/logout") ||
                path.startsWith("/api/auth/refresh") ||
                path.startsWith("/api/health") ||
                path.startsWith("/actuator") ||
                path.startsWith("/swagger-ui") ||
                path.contains("/swagger-ui") ||
                path.equals("/swagger-ui.html") ||
                path.startsWith("/v3/api-docs") ||
                path.startsWith("/api-docs") ||
                path.startsWith("/swagger-resources") ||
                path.startsWith("/webjars") ||
                path.startsWith("/configuration") ||
                path.startsWith("/h2-console") ||
                path.endsWith(".css") ||
                path.endsWith(".js") ||
                path.endsWith(".png") ||
                path.endsWith(".ico") ||
                path.endsWith(".html") ||
                path.equals("/favicon.ico");

        log.debug("Path: {} - Should skip filter: {}", path, shouldSkip);
        return shouldSkip;
    }
}