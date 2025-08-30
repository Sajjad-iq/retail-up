package com.sajjadkademm.retail.shared.validators;

import com.sajjadkademm.retail.config.locales.errorCode.AuthErrorCode;
import com.sajjadkademm.retail.exceptions.BadRequestException;
import com.sajjadkademm.retail.exceptions.NotFoundException;
import com.sajjadkademm.retail.exceptions.UnauthorizedException;
import com.sajjadkademm.retail.users.User;
import com.sajjadkademm.retail.shared.enums.AccountType;
import com.sajjadkademm.retail.shared.enums.UserStatus;
import com.sajjadkademm.retail.config.locales.LocalizedErrorService;
import com.sajjadkademm.retail.config.locales.errorCode.UserErrorCode;
import com.sajjadkademm.retail.users.UserRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.Optional;

/**
 * Centralized validation helpers for User domain rules.
 * Provides reusable checks to keep services/utilities consistent and DRY.
 */
@Component
public class UserValidator {

    private final LocalizedErrorService localizedErrorService;
    private final UserRepository userRepository;

    @Autowired
    public UserValidator(LocalizedErrorService localizedErrorService, UserRepository userRepository) {
        this.localizedErrorService = localizedErrorService;
        this.userRepository = userRepository;
    }

    /**
     * Validates that the user is active.
     * Checks if user status is ACTIVE.
     *
     * @param user the user to validate
     * @throws UnauthorizedException when user is not active
     */
    public void assertUserIsHasActiveStatus(User user) {
        if (user.getStatus() != UserStatus.ACTIVE) {
            throw new UnauthorizedException(localizedErrorService
                    .getLocalizedMessage(UserErrorCode.USER_NOT_ACTIVE.getMessage()));
        }
    }

    /**
     * Validates that the user has the specified account type.
     * Checks if user account type matches the expected type.
     *
     * @param user                the user to validate
     * @param expectedAccountType the expected account type
     * @throws UnauthorizedException when user account type doesn't match
     */
    public void assertUserAccountType(User user, AccountType expectedAccountType) {
        if (user.getAccountType() != expectedAccountType) {
            throw new UnauthorizedException(localizedErrorService
                    .getLocalizedMessage(UserErrorCode.INSUFFICIENT_PERMISSIONS.getMessage()));
        }
    }


    /**
     * Validates JWT token and returns user information if valid.
     * Checks if user still exists and is active.
     *
     * @param userId the user ID from token
     * @return the validated user
     * @throws NotFoundException     when user is not found
     * @throws UnauthorizedException when user is not active
     */
    public User validateUserActive(String userId) {
        Optional<User> userOpt = userRepository.findById(userId);
        if (userOpt.isEmpty()) {
            throw new NotFoundException(localizedErrorService
                    .getLocalizedMessage(AuthErrorCode.AUTH_USER_NOT_FOUND.getMessage(), userId));
        }

        User user = userOpt.get();

        // Verify user is still active
        if (user.getStatus() != UserStatus.ACTIVE) {
            throw new BadRequestException(
                    localizedErrorService.getLocalizedMessage(AuthErrorCode.AUTH_ACCOUNT_NOT_ACTIVE.getMessage()));
        }

        return user;
    }
}
