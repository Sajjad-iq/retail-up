package com.sajjadkademm.retail.domain.auth.commands;

import com.sajjadkademm.retail.application.dto.auth.LoginRequest;
import com.sajjadkademm.retail.application.dto.auth.LoginResponse;
import com.sajjadkademm.retail.shared.cqrs.Command;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Command to authenticate a user with email/phone and password
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class LoginCommand implements Command<LoginResponse> {
    
    /**
     * Login request containing credentials
     */
    private LoginRequest request;
    
    /**
     * Client IP address for audit logging
     */
    private String clientIp;
    
    /**
     * User agent for audit logging
     */
    private String userAgent;
    
    /**
     * Get the user ID - for login, we don't have a user ID yet, so return null
     */
    @Override
    public String getUserId() {
        return null; // No user context for login command
    }
}