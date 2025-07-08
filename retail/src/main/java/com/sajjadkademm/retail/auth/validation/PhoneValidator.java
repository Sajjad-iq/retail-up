package com.sajjadkademm.retail.auth.validation;

import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;
import org.springframework.stereotype.Component;

import java.util.regex.Pattern;

/**
 * Phone validator implementation for phone number validation.
 * Validates phone number format using regex patterns.
 * 
 * @author Sajjad Kadem
 * @version 1.0
 * @since 2024-12-19
 */
@Component
public class PhoneValidator implements ConstraintValidator<PhoneConstraint, String> {

    private static final Pattern PHONE_PATTERN = Pattern.compile("^\\+?[\\d\\s\\-\\(\\)]{10,}$");

    @Override
    public void initialize(PhoneConstraint constraintAnnotation) {
        // Initialization logic if needed
    }

    @Override
    public boolean isValid(String phone, ConstraintValidatorContext context) {
        // Allow null or empty values (use @NotNull/@NotBlank for required validation)
        if (phone == null || phone.trim().isEmpty()) {
            return true;
        }

        // Validate phone format
        return PHONE_PATTERN.matcher(phone.trim()).matches();
    }
}