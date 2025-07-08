package com.sajjadkademm.retail.auth.validation;

import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

/**
 * Password validator implementation for strong password validation.
 * Validates password strength based on configurable criteria.
 * 
 * @author Sajjad Kadem
 * @version 1.0
 * @since 2024-12-19
 */
@Component
public class PasswordValidator implements ConstraintValidator<PasswordConstraint, String> {

    @Value("${password.min-length:8}")
    private int minLength;

    @Value("${password.require-uppercase:true}")
    private boolean requireUppercase;

    @Value("${password.require-lowercase:true}")
    private boolean requireLowercase;

    @Value("${password.require-digit:true}")
    private boolean requireDigit;

    @Value("${password.require-special-char:true}")
    private boolean requireSpecialChar;

    @Override
    public void initialize(PasswordConstraint constraintAnnotation) {
        // Initialization logic if needed
    }

    @Override
    public boolean isValid(String password, ConstraintValidatorContext context) {
        if (password == null || password.trim().isEmpty()) {
            return false;
        }

        boolean isValid = true;
        StringBuilder errorMessage = new StringBuilder("Password must contain: ");

        // Check minimum length
        if (password.length() < minLength) {
            isValid = false;
            errorMessage.append("at least ").append(minLength).append(" characters, ");
        }

        // Check uppercase requirement
        if (requireUppercase && !password.matches(".*[A-Z].*")) {
            isValid = false;
            errorMessage.append("uppercase letter, ");
        }

        // Check lowercase requirement
        if (requireLowercase && !password.matches(".*[a-z].*")) {
            isValid = false;
            errorMessage.append("lowercase letter, ");
        }

        // Check digit requirement
        if (requireDigit && !password.matches(".*\\d.*")) {
            isValid = false;
            errorMessage.append("number, ");
        }

        // Check special character requirement
        if (requireSpecialChar && !password.matches(".*[!@#$%^&*(),.?\":{}|<>].*")) {
            isValid = false;
            errorMessage.append("special character, ");
        }

        if (!isValid) {
            // Remove trailing comma and space
            String finalMessage = errorMessage.toString().replaceAll(", $", "");
            context.disableDefaultConstraintViolation();
            context.buildConstraintViolationWithTemplate(finalMessage)
                    .addConstraintViolation();
        }

        return isValid;
    }
}