package com.sajjadkademm.retail.shared.common.validators;

import com.sajjadkademm.retail.shared.common.exceptions.BadRequestException;
import com.sajjadkademm.retail.shared.common.exceptions.ConflictException;
import com.sajjadkademm.retail.shared.localization.LocalizedErrorService;
import com.sajjadkademm.retail.shared.localization.errorCode.UserErrorCode;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * Centralized validation helpers for email rules.
 * Provides reusable checks to keep services/utilities consistent and DRY.
 */
@Component
public class EmailValidator {

    private final LocalizedErrorService localizedErrorService;

    @Autowired
    public EmailValidator(LocalizedErrorService localizedErrorService) {
        this.localizedErrorService = localizedErrorService;
    }

    /**
     * Validates email format according to business rules.
     * Checks if email is valid format and within length constraints.
     *
     * @param email the email to validate
     * @throws BadRequestException when email validation fails
     */
    public void validateEmailFormat(String email) {
        if (email == null || email.trim().isEmpty()) {
            throw new BadRequestException(localizedErrorService
                    .getLocalizedMessage(UserErrorCode.USER_EMAIL_EMPTY.getMessage()));
        }

        if (email.trim().length() > 255) {
            throw new BadRequestException(localizedErrorService
                    .getLocalizedMessage(UserErrorCode.USER_EMAIL_INVALID_FORMAT.getMessage()));
        }

        // Basic email format validation
        if (!email.matches("^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$")) {
            throw new BadRequestException(localizedErrorService
                    .getLocalizedMessage(UserErrorCode.USER_EMAIL_INVALID_FORMAT.getMessage()));
        }
    }

    /**
     * Validates email format and checks for uniqueness.
     * Combines format validation with existence check.
     *
     * @param email         the email to validate
     * @param existsChecker functional interface to check if email exists
     * @throws BadRequestException when email validation fails
     * @throws ConflictException   when email already exists
     */
    public void validateEmailFormatAndUniqueness(String email, EmailExistsChecker existsChecker) {
        validateEmailFormat(email);

        if (email != null && !email.trim().isEmpty() && existsChecker.exists(email)) {
            throw new ConflictException(localizedErrorService
                    .getLocalizedMessage(UserErrorCode.USER_EMAIL_ALREADY_EXISTS.getMessage(), email));
        }
    }

    /**
     * Functional interface for checking if an email exists.
     * This allows the validator to be decoupled from specific repository
     * implementations.
     */
    @FunctionalInterface
    public interface EmailExistsChecker {
        boolean exists(String email);
    }
}
