package com.sajjadkademm.retail.config.locales.errorCode;

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
    USER_NOT_FOUND("USR001", "user.not.found"),
    USER_DISABLED("USR002", "user.disabled"),
    USER_NOT_ACTIVE("USR003", "user.not.active"),
    USER_ALREADY_EXISTS("USR005", "user.already.exists"),
    INVALID_USER_DATA("USR006", "invalid.user.data"),
    USER_DELETION_FAILED("USR007", "user.deletion.failed"),
    USER_UPDATE_FAILED("USR008", "user.update.failed"),
    USER_CREATION_FAILED("USR009", "user.creation.failed"),
    INSUFFICIENT_PERMISSIONS("USR010", "insufficient.permissions"),
    USER_NOT_ORGANIZATION_CREATOR("USR011", "user.not.organization.creator"),
    USER_NOT_AUTHENTICATED("USR012", "user.not.authenticated"),
    USER_UNAUTHORIZED("USR013", "user.unauthorized"),
    CANNOT_DELETE_SELF("USR014", "cannot.delete.self");

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
