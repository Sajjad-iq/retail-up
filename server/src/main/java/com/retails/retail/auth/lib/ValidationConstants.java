package com.retails.retail.auth.lib;

/**
 * Validation constants for auth DTOs
 * Centralizes all validation patterns and messages to avoid duplication
 */
public final class ValidationConstants {

    private ValidationConstants() {
        // Utility class - prevent instantiation
    }

    // Validation Patterns
    public static final String EMAIL_PATTERN = "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$";
    public static final String PHONE_PATTERN = "^\\+?[\\d\\s\\-\\(\\)]{10,}$";
    public static final String PASSWORD_PATTERN = "^(?=.*[a-z])(?=.*[A-Z])(?=.*\\d)(?=.*[!@#$%^&*(),.?\":{}|<>]).{8,}$";
    public static final String STATUS_PATTERN = "^(active|inactive|locked|suspended|pending)$";

    // Field Size Constraints
    public static final int NAME_MIN_LENGTH = 2;
    public static final int NAME_MAX_LENGTH = 50;
    public static final int EMAIL_MAX_LENGTH = 100;
    public static final int PHONE_MAX_LENGTH = 20;
    public static final int DEPARTMENT_MAX_LENGTH = 50;
    public static final int EMPLOYEE_ID_MIN_LENGTH = 2;
    public static final int EMPLOYEE_ID_MAX_LENGTH = 20;
    public static final int PASSWORD_MIN_LENGTH = 8;
    public static final int PASSWORD_MAX_LENGTH = 100;

    // Validation Messages
    public static final String NAME_REQUIRED = "Name is required";
    public static final String NAME_SIZE = "Name must be between " + NAME_MIN_LENGTH + " and " + NAME_MAX_LENGTH
            + " characters";
    public static final String EMAIL_REQUIRED = "Email is required";
    public static final String EMAIL_INVALID = "Please enter a valid email address";
    public static final String PHONE_INVALID = "Please enter a valid phone number";
    public static final String DEPARTMENT_SIZE = "Department must not exceed " + DEPARTMENT_MAX_LENGTH + " characters";
    public static final String EMPLOYEE_ID_SIZE = "Employee ID must be between " + EMPLOYEE_ID_MIN_LENGTH + " and "
            + EMPLOYEE_ID_MAX_LENGTH + " characters";
    public static final String PASSWORD_REQUIRED = "Password is required";
    public static final String PASSWORD_SIZE = "Password must be at least " + PASSWORD_MIN_LENGTH + " characters";
    public static final String PASSWORD_PATTERN_MESSAGE = "Password must contain uppercase, lowercase, number, and special character";
    public static final String STATUS_INVALID = "Invalid status value";
    public static final String ROLE_REQUIRED = "Role is required";
    public static final String CURRENT_PASSWORD_REQUIRED = "Current password is required";
    public static final String NEW_PASSWORD_REQUIRED = "New password is required";
    public static final String NEW_PASSWORD_SIZE = "New password must be at least " + PASSWORD_MIN_LENGTH
            + " characters";
    public static final String CONFIRM_PASSWORD_REQUIRED = "Please confirm your new password";
}