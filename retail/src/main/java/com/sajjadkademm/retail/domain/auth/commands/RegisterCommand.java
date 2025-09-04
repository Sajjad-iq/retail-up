package com.sajjadkademm.retail.domain.auth.commands;

import com.sajjadkademm.retail.application.dto.auth.RegisterRequest;
import com.sajjadkademm.retail.application.dto.auth.LoginResponse;
import com.sajjadkademm.retail.shared.cqrs.Command;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Command to register a new user account
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class RegisterCommand implements Command<LoginResponse> {
    
    /**
     * Registration request containing user details
     */
    private RegisterRequest request;
    
    /**
     * Client IP address for audit logging
     */
    private String clientIp;
    
    /**
     * User agent for audit logging
     */
    private String userAgent;
    
    /**
     * Get the user ID - for registration, we don't have a user ID yet, so return null
     */
    @Override
    public String getUserId() {
        return null; // No user context for registration command
    }
}