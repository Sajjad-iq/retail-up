package com.sajjadkademm.retail.domain.auth.commands;

import com.sajjadkademm.retail.application.dto.auth.AuthResponse;
import com.sajjadkademm.retail.shared.cqrs.Command;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Command to change the current user's password
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class ChangePasswordCommand implements Command<AuthResponse> {
    
    /**
     * User ID of the current authenticated user
     */
    private String userId;
    
    /**
     * Current password for verification
     */
    private String oldPassword;
    
    /**
     * New password to set
     */
    private String newPassword;
    
    /**
     * Client IP address for audit logging
     */
    private String clientIp;
    
    /**
     * User agent for audit logging
     */
    private String userAgent;
    
    /**
     * Get the user ID who initiated this command
     */
    @Override
    public String getUserId() {
        return userId;
    }
}