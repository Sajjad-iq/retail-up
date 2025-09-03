package com.sajjadkademm.retail.shared.localization.errorCode;

/**
 * Centralized error codes for inventory operations.
 * Provides consistent error handling across all inventory-related services.
 *
 * @author Sajjad Kadem
 * @version 1.0
 * @since 2024-12-19
 */
public enum InventoryErrorCode {

    // Inventory Errors (INV001-INV099)
    INVENTORY_NOT_FOUND("INV001", "inventory.not.found"),
    INVENTORY_ALREADY_EXISTS("INV002", "inventory.already.exists"),
    INVENTORY_VALIDATION_FAILED("INV003", "inventory.validation.failed"),
    INVENTORY_CREATION_FAILED("INV004", "inventory.creation.failed"),
    INVENTORY_UPDATE_FAILED("INV005", "inventory.update.failed"),
    INVENTORY_DELETION_FAILED("INV006", "inventory.deletion.failed"),
    INVENTORY_NAME_DUPLICATE("INV007", "inventory.name.duplicate"),
    INVENTORY_INACTIVE("INV008", "inventory.inactive"),

    // Validation Errors
    INVALID_INVENTORY_DATA("INV014", "invalid.inventory.data"),
    INVALID_SEARCH_TERM("INV015", "invalid.search.term");

    private final String code;
    private final String message;

    InventoryErrorCode(String code, String message) {
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
    public static InventoryErrorCode fromCode(String code) {
        for (InventoryErrorCode errorCode : values()) {
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
