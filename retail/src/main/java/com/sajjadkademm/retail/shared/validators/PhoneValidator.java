package com.sajjadkademm.retail.shared.validators;

import com.sajjadkademm.retail.config.locales.errorCode.AuthErrorCode;
import com.sajjadkademm.retail.exceptions.BadRequestException;
import com.sajjadkademm.retail.exceptions.ConflictException;
import com.sajjadkademm.retail.config.locales.LocalizedErrorService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * Centralized validation helpers for phone number rules.
 * Provides reusable checks to keep services/utilities consistent and DRY.
 */
@Component
public class PhoneValidator {

    private final LocalizedErrorService localizedErrorService;

    @Autowired
    public PhoneValidator(LocalizedErrorService localizedErrorService) {
        this.localizedErrorService = localizedErrorService;
    }

    /**
     * Validates phone format according to business rules.
     * Checks if phone is within length constraints and has valid format.
     *
     * @param phone the phone number to validate
     * @throws BadRequestException when phone validation fails
     */
    public void validatePhoneFormat(String phone) {
        if (phone.trim().length() < 10 || phone.trim().length() > 20) {
            throw new BadRequestException(localizedErrorService
                    .getLocalizedMessage(AuthErrorCode.AUTH_PHONE_TO_SHORT.getMessage()));
        }

        if (!phone.matches("^[\\d\\s\\-\\(\\)\\+]+$")) {
            throw new BadRequestException(localizedErrorService
                    .getLocalizedMessage(AuthErrorCode.AUTH_PHONE_FORMAT_WRONG.getMessage()));
        }
    }

    /**
     * Validates phone format and checks for uniqueness.
     * Combines format validation with existence check.
     *
     * @param phone         the phone number to validate
     * @param existsChecker functional interface to check if phone exists
     * @throws BadRequestException when phone validation fails
     * @throws ConflictException   when phone already exists
     */
    public void validatePhoneFormatAndUniqueness(String phone, PhoneExistsChecker existsChecker) {
        validatePhoneFormat(phone);

        if (existsChecker.exists(phone)) {
            throw new ConflictException(localizedErrorService
                    .getLocalizedMessage(AuthErrorCode.AUTH_PHONE_ALREADY_EXISTS.getMessage(), phone));
        }
    }

    /**
     * Functional interface for checking if a phone number exists.
     * This allows the validator to be decoupled from specific repository
     * implementations.
     */
    @FunctionalInterface
    public interface PhoneExistsChecker {
        boolean exists(String phone);
    }
}
