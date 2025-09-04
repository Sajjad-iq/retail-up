package com.sajjadkademm.retail.domain.auth.queries;

import com.sajjadkademm.retail.shared.cqrs.Query;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Query to check if a user exists by email or phone number
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class UserExistsQuery implements Query<Boolean> {
    
    /**
     * Email or phone number to check for existence
     */
    private String emailOrPhone;
    
    /**
     * Get the user ID - for user existence check, there's no user context, so return null
     */
    @Override
    public String getUserId() {
        return null; // No user context for user existence query
    }
}