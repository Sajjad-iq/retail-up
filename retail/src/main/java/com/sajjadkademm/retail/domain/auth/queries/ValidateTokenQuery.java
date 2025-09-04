package com.sajjadkademm.retail.domain.auth.queries;

import com.sajjadkademm.retail.application.dto.auth.LoginResponse;
import com.sajjadkademm.retail.shared.cqrs.Query;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Query to validate JWT token and refresh user information
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class ValidateTokenQuery implements Query<LoginResponse> {
    
    /**
     * User ID of the current authenticated user
     */
    private String userId;
    
    /**
     * Client IP address for audit logging
     */
    private String clientIp;
    
    /**
     * User agent for audit logging
     */
    private String userAgent;
    
    /**
     * Get the user ID who initiated this query
     */
    @Override
    public String getUserId() {
        return userId;
    }
}