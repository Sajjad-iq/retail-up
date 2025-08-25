package com.sajjadkademm.retail.users;

/**
 * Centralized error codes for user operations.
 * Provides consistent error handling across all user-related services.
 * 
 * @author Sajjad Kadem
 * @version 1.0
 * @since 2024-12-19
 */
public enum UserErrorCode {

    // User Errors (USR001-USR099)
    USER_NOT_FOUND("USR001", "User not found"),
    USER_DISABLED("USR002", "User is disabled"),
    USER_INACTIVE("USR003", "User is inactive"),
    USER_NOT_ACTIVE("USR004", "Only Active Users Can Update Inventory Items"),
    USER_ALREADY_EXISTS("USR005", "User already exists"),
    INVALID_USER_DATA("USR006", "Invalid user data"),
    USER_DELETION_FAILED("USR007", "Failed to delete user"),
    USER_UPDATE_FAILED("USR008", "Failed to update user"),
    USER_CREATION_FAILED("USR009", "Failed to create user"),
    INSUFFICIENT_PERMISSIONS("USR010", "Insufficient permissions to perform operation");

    private final String code;
    private final String message;

    UserErrorCode(String code, String message) {
        this.code = code;
        this.message = message;
    }

    public String getCode() {
        return code;
    }

    public String getMessage() {
        return message;
    }

    /**
     * Get error code by code string
     */
    public static UserErrorCode fromCode(String code) {
        for (UserErrorCode errorCode : values()) {
            if (errorCode.code.equals(code)) {
                return errorCode;
            }
        }
        throw new IllegalArgumentException("Unknown error code: " + code);
    }

    /**
     * Check if error code exists
     */
    public static boolean isValidCode(String code) {
        try {
            fromCode(code);
            return true;
        } catch (IllegalArgumentException e) {
            return false;
        }
    }

    @Override
    public String toString() {
        return String.format("%s: %s", code, message);
    }
}
