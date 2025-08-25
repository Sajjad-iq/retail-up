package com.sajjadkademm.retail.organizations;

/**
 * Centralized error codes for organization operations.
 * Provides consistent error handling across all organization-related services.
 * 
 * @author Sajjad Kadem
 * @version 1.0
 * @since 2024-12-19
 */
public enum OrganizationErrorCode {

    // Organization Errors (ORG001-ORG099)
    ORGANIZATION_NOT_FOUND("ORG001", "organization.not.found"),
    ORGANIZATION_DISABLED("ORG002", "organization.disabled"),
    ORGANIZATION_INACTIVE("ORG003", "organization.inactive"),
    ORGANIZATION_VALIDATION_FAILED("ORG004", "organization.validation.failed"),
    ORGANIZATION_ALREADY_EXISTS("ORG005", "organization.already.exists"),
    INVALID_ORGANIZATION_DATA("ORG006", "invalid.organization.data"),
    ORGANIZATION_DELETION_FAILED("ORG007", "organization.deletion.failed"),
    ORGANIZATION_UPDATE_FAILED("ORG008", "organization.update.failed"),
    ORGANIZATION_CREATION_FAILED("ORG009", "organization.creation.failed");

    private final String code;
    private final String message;

    OrganizationErrorCode(String code, String message) {
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
    public static OrganizationErrorCode fromCode(String code) {
        for (OrganizationErrorCode errorCode : values()) {
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
