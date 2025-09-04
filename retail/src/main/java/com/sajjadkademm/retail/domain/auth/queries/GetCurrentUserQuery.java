package com.sajjadkademm.retail.domain.auth.queries;

import com.sajjadkademm.retail.domain.auth.model.User;
import com.sajjadkademm.retail.shared.cqrs.Query;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Query to get the current authenticated user's profile
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class GetCurrentUserQuery implements Query<User> {
    
    /**
     * User ID of the current authenticated user
     */
    private String userId;
    
    /**
     * Get the user ID who initiated this query
     */
    @Override
    public String getUserId() {
        return userId;
    }
}