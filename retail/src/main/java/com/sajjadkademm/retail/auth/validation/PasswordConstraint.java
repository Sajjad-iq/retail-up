package com.sajjadkademm.retail.auth.validation;

import jakarta.validation.Constraint;
import jakarta.validation.Payload;

import java.lang.annotation.*;

/**
 * Custom validation annotation for password strength validation.
 * Validates that password meets security requirements.
 * 
 * @author Sajjad Kadem
 * @version 1.0
 * @since 2024-12-19
 */
@Documented
@Constraint(validatedBy = PasswordValidator.class)
@Target({ ElementType.FIELD, ElementType.PARAMETER })
@Retention(RetentionPolicy.RUNTIME)
public @interface PasswordConstraint {

    String message() default "Password must contain uppercase, lowercase, number, and special character";

    Class<?>[] groups() default {};

    Class<? extends Payload>[] payload() default {};
}