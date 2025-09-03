package com.sajjadkademm.retail.config;

import com.sajjadkademm.retail.shared.common.exceptions.BadRequestException;
import com.sajjadkademm.retail.shared.common.exceptions.UnauthorizedException;
import com.sajjadkademm.retail.users.User;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Component;

/**
 * Utility class for security-related operations
 * Provides methods to access the current authenticated user
 * 
 * @author Sajjad Kadem
 * @version 1.0
 * @since 2024-12-19
 */
@Component
public class SecurityUtils {

    /**
     * Get the current authenticated user from the security context
     * 
     * @return the current authenticated user
     * @throws IllegalStateException if no user is authenticated
     */
    public static User getCurrentUser() {
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();

        if (authentication == null || !authentication.isAuthenticated()) {
            throw new BadRequestException("No authenticated user found");
        }

        Object principal = authentication.getPrincipal();

        if (principal instanceof User) {
            return (User) principal;
        } else {
            throw new BadRequestException("Principal is not a User instance");
        }
    }

    /**
     * Get the current authenticated user ID from the security context
     * 
     * @return the current authenticated user ID
     * @throws IllegalStateException if no user is authenticated
     */
    public static String getCurrentUserId() {
        return getCurrentUser().getId();
    }

    /**
     * Check if a user is currently authenticated
     * 
     * @return true if a user is authenticated, false otherwise
     */
    public static boolean isAuthenticated() {
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        return authentication != null && authentication.isAuthenticated();
    }
}
