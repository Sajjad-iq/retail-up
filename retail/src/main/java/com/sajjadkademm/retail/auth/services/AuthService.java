package com.sajjadkademm.retail.auth.services;

import com.sajjadkademm.retail.auth.dto.LoginRequest;
import com.sajjadkademm.retail.auth.dto.LoginResponse;
import com.sajjadkademm.retail.auth.entities.AuthSession;
import com.sajjadkademm.retail.auth.entities.User;
import com.sajjadkademm.retail.auth.entities.UserActivity;
import com.sajjadkademm.retail.auth.exceptions.AuthenticationFailedException;
import com.sajjadkademm.retail.auth.exceptions.UserNotFoundException;
import com.sajjadkademm.retail.auth.repositories.AuthSessionRepository;
import com.sajjadkademm.retail.auth.repositories.UserRepository;
import com.sajjadkademm.retail.auth.utils.AuthUtils;
import jakarta.servlet.http.Cookie;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.AuthenticationException;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDateTime;
import java.util.List;

/**
 * Authentication service for handling login, logout, and session management.
 * Provides core authentication functionality with JWT tokens and cookies.
 * 
 * @author Sajjad Kadem
 * @version 1.0
 * @since 2024-12-19
 */
@Slf4j
@Service
@RequiredArgsConstructor
public class AuthService {

    private final AuthenticationManager authenticationManager;
    private final UserRepository userRepository;
    private final AuthSessionRepository authSessionRepository;
    private final JwtService jwtService;
    private final PasswordEncoder passwordEncoder;
    private final UserActivityService userActivityService;
    private final UserService userService;

    @Value("${session.max-sessions:5}")
    private int maxSessions;

    @Value("${jwt.expiration}")
    private long jwtExpirationMs;

    /**
     * Authenticate user and create session
     */
    @Transactional
    public LoginResponse login(LoginRequest loginRequest, HttpServletRequest request, HttpServletResponse response) {
        log.info("Login attempt for email: {}", loginRequest.getEmail());

        try {
            // Authenticate user
            Authentication authentication = authenticationManager.authenticate(
                    new UsernamePasswordAuthenticationToken(
                            loginRequest.getEmail(),
                            loginRequest.getPassword()));

            // Get user details
            User user = userRepository.findByEmail(loginRequest.getEmail())
                    .orElseThrow(() -> new UserNotFoundException("User not found"));

            // Check user status
            if (user.getStatus() != User.UserStatus.ACTIVE) {
                throw new AuthenticationFailedException("User account is not active");
            }

            // Generate JWT token
            long expirationMs = loginRequest.getRememberMe() != null && loginRequest.getRememberMe()
                    ? jwtExpirationMs * 7 // 7 days for remember me
                    : jwtExpirationMs; // Default expiration

            String jwtToken = jwtService.generateToken(user, expirationMs);
            LocalDateTime expiresAt = LocalDateTime.now().plusSeconds(expirationMs / 1000);

            // Create auth session
            AuthSession authSession = createAuthSession(user, jwtToken, expiresAt, request);

            // Set cookie for development
            setCookieToken(response, jwtToken, loginRequest.getRememberMe());

            // Update user last login
            user.setLastLoginAt(LocalDateTime.now());
            userRepository.save(user);

            // Log activity
            userActivityService.logActivity(
                    user.getId(),
                    UserActivity.UserAction.LOGIN,
                    "login",
                    "User logged in successfully",
                    AuthUtils.getClientIpAddress(request),
                    request.getHeader("User-Agent"));

            // Clean up old sessions
            cleanupOldSessions(user.getId());

            log.info("User {} logged in successfully", user.getEmail());

            // Build response
            return LoginResponse.builder()
                    .token(jwtToken)
                    .tokenType("Bearer")
                    .expiresAt(expiresAt)
                    .user(userService.mapToUserResponse(user))
                    .session(LoginResponse.SessionInfo.builder()
                            .sessionId(authSession.getId())
                            .createdAt(authSession.getCreatedAt())
                            .expiresAt(authSession.getExpiresAt())
                            .build())
                    .build();

        } catch (AuthenticationException e) {
            log.warn("Login failed for email: {} - {}", loginRequest.getEmail(), e.getMessage());

            // Log failed attempt
            userActivityService.logActivity(
                    null, // No user ID for failed login
                    UserActivity.UserAction.ACCESS_DENIED,
                    "login_attempt",
                    "Failed login attempt for email: " + loginRequest.getEmail(),
                    AuthUtils.getClientIpAddress(request),
                    request.getHeader("User-Agent"));

            throw new AuthenticationFailedException("Invalid email or password");
        }
    }

    /**
     * Logout user and invalidate session
     */
    @Transactional
    public void logout(HttpServletRequest request, HttpServletResponse response) {
        String token = extractTokenFromRequest(request);

        if (token != null) {
            try {
                String email = jwtService.extractEmail(token);
                User user = userRepository.findByEmail(email).orElse(null);

                // Invalidate session
                authSessionRepository.findByToken(token).ifPresent(session -> {
                    session.terminate("User logout");
                    authSessionRepository.save(session);
                });

                // Clear cookie
                clearCookieToken(response);

                // Clear security context
                SecurityContextHolder.clearContext();

                // Log activity
                if (user != null) {
                    userActivityService.logActivity(
                            user.getId(),
                            UserActivity.UserAction.LOGOUT,
                            "logout",
                            "User logged out successfully",
                            AuthUtils.getClientIpAddress(request),
                            request.getHeader("User-Agent"));
                }

                log.info("User {} logged out successfully", email);
            } catch (Exception e) {
                log.warn("Error during logout: {}", e.getMessage());
            }
        }
    }

    /**
     * Logout from all devices
     */
    @Transactional
    public void logoutFromAllDevices(String userId, HttpServletRequest request, HttpServletResponse response) {
        // Terminate all active sessions for user
        authSessionRepository.terminateAllUserSessions(
                userId,
                LocalDateTime.now(),
                "Logout from all devices");

        // Clear current session cookie
        clearCookieToken(response);
        SecurityContextHolder.clearContext();

        // Log activity
        userActivityService.logActivity(
                userId,
                UserActivity.UserAction.LOGOUT,
                "logout_all_devices",
                "User logged out from all devices",
                AuthUtils.getClientIpAddress(request),
                request.getHeader("User-Agent"));

        log.info("User {} logged out from all devices", userId);
    }

    /**
     * Validate session and refresh if needed
     */
    @Transactional
    public boolean validateSession(String token) {
        try {
            if (!jwtService.validateToken(token)) {
                return false;
            }

            // Check if session exists and is valid
            AuthSession session = authSessionRepository.findByToken(token).orElse(null);
            if (session == null || !session.isValid()) {
                return false;
            }

            // Update last accessed time
            session.updateLastAccessed();
            authSessionRepository.save(session);

            return true;
        } catch (Exception e) {
            log.warn("Session validation failed: {}", e.getMessage());
            return false;
        }
    }

    /**
     * Get current authenticated user
     */
    public User getCurrentUser() {
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        if (authentication != null && authentication.isAuthenticated()) {
            String email = authentication.getName();
            return userRepository.findByEmail(email).orElse(null);
        }
        return null;
    }

    /**
     * Check if user has permission
     */
    public boolean hasPermission(String permissionName) {
        User currentUser = getCurrentUser();
        return currentUser != null && currentUser.hasPermission(permissionName);
    }

    /**
     * Create auth session
     */
    private AuthSession createAuthSession(User user, String token, LocalDateTime expiresAt,
            HttpServletRequest request) {
        AuthSession session = AuthSession.builder()
                .id(AuthUtils.generateSessionId())
                .token(token)
                .userId(user.getId())
                .expiresAt(expiresAt)
                .ipAddress(AuthUtils.getClientIpAddress(request))
                .userAgent(request.getHeader("User-Agent"))
                .isActive(true)
                .build();

        return authSessionRepository.save(session);
    }

    /**
     * Set JWT token as HTTP-only cookie for development
     */
    private void setCookieToken(HttpServletResponse response, String token, Boolean rememberMe) {
        Cookie cookie = new Cookie("auth_token", token);
        cookie.setHttpOnly(true);
        cookie.setSecure(false); // Set to true in production with HTTPS
        cookie.setPath("/");

        if (rememberMe != null && rememberMe) {
            cookie.setMaxAge((int) (jwtExpirationMs * 7 / 1000)); // 7 days
        } else {
            cookie.setMaxAge((int) (jwtExpirationMs / 1000)); // Default session
        }

        response.addCookie(cookie);
    }

    /**
     * Clear auth token cookie
     */
    private void clearCookieToken(HttpServletResponse response) {
        Cookie cookie = new Cookie("auth_token", "");
        cookie.setHttpOnly(true);
        cookie.setSecure(false); // Set to true in production
        cookie.setPath("/");
        cookie.setMaxAge(0);
        response.addCookie(cookie);
    }

    /**
     * Extract token from request
     */
    private String extractTokenFromRequest(HttpServletRequest request) {
        // Try Authorization header first
        String bearerToken = request.getHeader("Authorization");
        if (bearerToken != null && bearerToken.startsWith("Bearer ")) {
            return bearerToken.substring(7);
        }

        // Try cookie
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
     * Clean up old sessions for user
     */
    private void cleanupOldSessions(String userId) {
        List<AuthSession> userSessions = authSessionRepository.findByUserIdAndIsActiveTrue(userId);

        if (userSessions.size() > maxSessions) {
            // Sort by creation date and terminate oldest sessions
            userSessions.sort((a, b) -> a.getCreatedAt().compareTo(b.getCreatedAt()));

            int sessionsToTerminate = userSessions.size() - maxSessions;
            for (int i = 0; i < sessionsToTerminate; i++) {
                AuthSession session = userSessions.get(i);
                session.terminate("Exceeded maximum sessions");
                authSessionRepository.save(session);
            }
        }
    }
}