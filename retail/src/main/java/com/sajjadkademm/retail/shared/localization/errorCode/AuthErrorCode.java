package com.sajjadkademm.retail.shared.localization.errorCode;

/**
 * Simplified error codes for authentication operations
 * Only includes errors actually used in the current implementation
 */
public enum AuthErrorCode {

    // Core Authentication Errors
    AUTH_INVALID_CREDENTIALS("AUTH001", "auth.invalid.credentials"),
    AUTH_USER_NOT_FOUND("AUTH002", "auth.user.not.found"),
    AUTH_ACCOUNT_NOT_ACTIVE("AUTH003", "auth.account.not.active"),

    // Registration Errors
    AUTH_EMAIL_ALREADY_EXISTS("AUTH004", "auth.email.already.exists"),
    AUTH_PHONE_ALREADY_EXISTS("AUTH005", "auth.phone.already.exists"),
    AUTH_PHONE_FORMAT_WRONG("AUTH0013", "auth.phone.format.wrong"),
    AUTH_PHONE_TO_SHORT("AUTH0014", "auth.phone.too.short"),

    // Password Change Errors
    AUTH_OLD_PASSWORD_INCORRECT("AUTH006", "auth.old.password.incorrect"),

    // Token Validation Errors
    AUTH_TOKEN_INVALID("AUTH007", "auth.token.invalid"),
    AUTH_INVALID_TOKEN("AUTH015", "auth.invalid.token"),
    AUTH_HEADER_INVALID("AUTH008", "auth.header.invalid"),

    // Success Messages
    AUTH_LOGIN_SUCCESSFUL("AUTH009", "auth.login.successful"),
    AUTH_REGISTRATION_SUCCESSFUL("AUTH010", "auth.registration.successful"),
    AUTH_TOKEN_VALID("AUTH011", "auth.token.valid"),
    AUTH_PASSWORD_CHANGED_SUCCESSFULLY("AUTH012", "auth.password.changed.successfully"),;

    private final String code;
    private final String message;

    AuthErrorCode(String code, String message) {
        this.code = code;
        this.message = message;
    }

    public String getCode() {
        return code;
    }

    public String getMessage() {
        return message;
    }

    @Override
    public String toString() {
        return code + ": " + message;
    }
}
