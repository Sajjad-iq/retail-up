package com.sajjadkademm.retail.application.services.auth;

import com.sajjadkademm.retail.application.dto.auth.AuthResponse;
import com.sajjadkademm.retail.application.dto.auth.ChangePasswordRequest;
import com.sajjadkademm.retail.application.dto.auth.LoginRequest;
import com.sajjadkademm.retail.application.dto.auth.LoginResponse;
import com.sajjadkademm.retail.application.dto.auth.RegisterRequest;
import com.sajjadkademm.retail.domain.user.model.User;
import com.sajjadkademm.retail.shared.cqrs.CommandBus;
import com.sajjadkademm.retail.shared.cqrs.QueryBus;
import com.sajjadkademm.retail.domain.auth.commands.*;
import com.sajjadkademm.retail.domain.auth.queries.*;
import com.sajjadkademm.retail.application.config.security.SecurityUtils;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import jakarta.servlet.http.HttpServletRequest;

/**
 * Service class for authentication operations.
 * Handles business logic for user authentication, registration, and authorization.
 * 
 * @author Sajjad Kadem
 * @version 1.0
 * @since 2024-12-19
 */
@Slf4j
@Service
@RequiredArgsConstructor
public class AuthService {

    private final CommandBus commandBus;
    private final QueryBus queryBus;

    /**
     * Authenticate user with email/phone and password
     * 
     * @param request Login credentials
     * @param httpRequest HTTP request for IP and user agent extraction
     * @return LoginResponse with JWT token and user information
     * @throws Exception if authentication fails
     */
    public LoginResponse login(LoginRequest request, HttpServletRequest httpRequest) throws Exception {
        log.debug("Processing login request for: {}", request.getEmailOrPhone());
        
        LoginCommand command = LoginCommand.builder()
                .request(request)
                .clientIp(getClientIpAddress(httpRequest))
                .userAgent(httpRequest.getHeader("User-Agent"))
                .build();
        
        return commandBus.execute(command);
    }

    /**
     * Register a new user account
     * 
     * @param request Registration information
     * @param httpRequest HTTP request for IP and user agent extraction
     * @return LoginResponse with JWT token and user information
     * @throws Exception if registration fails
     */
    public LoginResponse register(RegisterRequest request, HttpServletRequest httpRequest) throws Exception {
        log.debug("Processing registration request for: {}", request.getName());
        
        RegisterCommand command = RegisterCommand.builder()
                .request(request)
                .clientIp(getClientIpAddress(httpRequest))
                .userAgent(httpRequest.getHeader("User-Agent"))
                .build();
        
        return commandBus.execute(command);
    }

    /**
     * Get current authenticated user's profile
     * 
     * @return User profile information
     * @throws Exception if user retrieval fails
     */
    public User getCurrentUserProfile() throws Exception {
        log.debug("Getting current user profile for userId: {}", SecurityUtils.getCurrentUserId());
        
        GetCurrentUserQuery query = GetCurrentUserQuery.builder()
                .userId(SecurityUtils.getCurrentUserId())
                .build();
        
        return queryBus.execute(query);
    }

    /**
     * Check if a user exists by email or phone number
     * 
     * @param emailOrPhone Email or phone number to check
     * @return true if user exists, false otherwise
     * @throws Exception if check fails
     */
    public boolean userExists(String emailOrPhone) throws Exception {
        log.debug("Checking user existence for: {}", emailOrPhone);
        
        UserExistsQuery query = UserExistsQuery.builder()
                .emailOrPhone(emailOrPhone)
                .build();
        
        return queryBus.execute(query);
    }

    /**
     * Change current authenticated user's password
     * 
     * @param request Password change request containing old and new passwords
     * @param httpRequest HTTP request for IP and user agent extraction
     * @return AuthResponse confirming password change
     * @throws Exception if password change fails
     */
    public AuthResponse changePassword(ChangePasswordRequest request, HttpServletRequest httpRequest) throws Exception {
        log.debug("Processing password change request for userId: {}", SecurityUtils.getCurrentUserId());
        
        ChangePasswordCommand command = ChangePasswordCommand.builder()
                .userId(SecurityUtils.getCurrentUserId())
                .oldPassword(request.getOldPassword())
                .newPassword(request.getNewPassword())
                .clientIp(getClientIpAddress(httpRequest))
                .userAgent(httpRequest.getHeader("User-Agent"))
                .build();
        
        return commandBus.execute(command);
    }

    /**
     * Validate JWT token and return user information
     * 
     * @param httpRequest HTTP request for IP and user agent extraction
     * @return LoginResponse with validated token information
     * @throws Exception if token validation fails
     */
    public LoginResponse validateToken(HttpServletRequest httpRequest) throws Exception {
        log.debug("Validating token for userId: {}", SecurityUtils.getCurrentUserId());
        
        ValidateTokenQuery query = ValidateTokenQuery.builder()
                .userId(SecurityUtils.getCurrentUserId())
                .clientIp(getClientIpAddress(httpRequest))
                .userAgent(httpRequest.getHeader("User-Agent"))
                .build();
        
        return queryBus.execute(query);
    }

    /**
     * Extract client IP address from HTTP request
     * Handles X-Forwarded-For and X-Real-IP headers for proxy scenarios
     * 
     * @param request HTTP request
     * @return Client IP address
     */
    private String getClientIpAddress(HttpServletRequest request) {
        String xForwardedFor = request.getHeader("X-Forwarded-For");
        if (xForwardedFor != null && !xForwardedFor.isEmpty() && !"unknown".equalsIgnoreCase(xForwardedFor)) {
            return xForwardedFor.split(",")[0].trim();
        }
        
        String xRealIp = request.getHeader("X-Real-IP");
        if (xRealIp != null && !xRealIp.isEmpty() && !"unknown".equalsIgnoreCase(xRealIp)) {
            return xRealIp;
        }
        
        return request.getRemoteAddr();
    }
}