package com.sajjadkademm.retail.shared.validators;

import com.sajjadkademm.retail.exceptions.UnauthorizedException;
import com.sajjadkademm.retail.users.User;
import com.sajjadkademm.retail.shared.enums.AccountType;
import com.sajjadkademm.retail.shared.enums.UserStatus;
import com.sajjadkademm.retail.config.locales.LocalizedErrorService;
import com.sajjadkademm.retail.config.locales.errorCode.UserErrorCode;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * Centralized validation helpers for User domain rules.
 * Provides reusable checks to keep services/utilities consistent and DRY.
 */
@Component
public class UserValidator {

    private final LocalizedErrorService localizedErrorService;

    @Autowired
    public UserValidator(LocalizedErrorService localizedErrorService) {
        this.localizedErrorService = localizedErrorService;
    }

    /**
     * Validates that the user is active.
     * Checks if user status is ACTIVE.
     *
     * @param user the user to validate
     * @throws UnauthorizedException when user is not active
     */
    public void assertUserIsActive(User user) {
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
     * Validates that the user is active and has the specified account type.
     * Combines both validations for convenience.
     *
     * @param user                the user to validate
     * @param expectedAccountType the expected account type
     * @throws UnauthorizedException when user validation fails
     */
    public void assertUserIsActiveAndHasAccountType(User user, AccountType expectedAccountType) {
        assertUserIsActive(user);
        assertUserAccountType(user, expectedAccountType);
    }
}
