package com.sajjadkademm.retail.application.services.auth;

import com.sajjadkademm.retail.application.dto.auth.AuthResponse;
import com.sajjadkademm.retail.application.dto.auth.LoginRequest;
import com.sajjadkademm.retail.application.dto.auth.LoginResponse;
import com.sajjadkademm.retail.application.dto.auth.RegisterRequest;

/**
 * Interface for authentication service operations.
 * Provides contract for authentication-related use cases.
 * This abstraction supports better testing and future implementation changes.
 */
public interface AuthServiceInterface {
    
    /**
     * Authenticate user with email/phone and password
     */
    LoginResponse login(LoginRequest request);
    
    /**
     * Register a new user account
     */
    LoginResponse register(RegisterRequest request);
    
    /**
     * Change current user password
     */
    AuthResponse changePasswordWithResponse(String oldPassword, String newPassword);
    
    /**
     * Refresh JWT token for current user
     */
    LoginResponse refreshToken();
    
    /**
     * Check if user exists by email or phone
     */
    boolean userExists(String emailOrPhone);
}