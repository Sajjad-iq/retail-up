package com.sajjadkademm.retail.auth.validation;

import jakarta.validation.Constraint;
import jakarta.validation.Payload;

import java.lang.annotation.*;

/**
 * Custom validation annotation for phone number validation.
 * Validates phone number format.
 * 
 * @author Sajjad Kadem
 * @version 1.0
 * @since 2024-12-19
 */
@Documented
@Constraint(validatedBy = PhoneValidator.class)
@Target({ ElementType.FIELD, ElementType.PARAMETER })
@Retention(RetentionPolicy.RUNTIME)
public @interface PhoneConstraint {

    String message() default "Please enter a valid phone number";

    Class<?>[] groups() default {};

    Class<? extends Payload>[] payload() default {};
}