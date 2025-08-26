package com.sajjadkademm.retail.inventory.InventoryItem.utils;

/**
 * Centralized error codes for inventory operations.
 * Provides consistent error handling across all inventory-related services.
 * 
 * @author Sajjad Kadem
 * @version 1.0
 * @since 2024-12-19
 */
public enum InventoryErrorCode {

    // Validation Errors (INV001-INV099)
    INVALID_SKU_FORMAT("INV001", "invalid.sku.format"),
    DUPLICATE_SKU("INV002", "sku.already.exists"),
    DUPLICATE_BARCODE("INV003", "barcode.already.exists"),
    DUPLICATE_PRODUCT_CODE("INV004", "product.code.already.exists"),
    INVALID_STOCK_LEVEL("INV005", "stock.cannot.be.negative"),
    INVALID_MIN_MAX_STOCK("INV006", "max.stock.less.than.min"),
    INVALID_CURRENT_STOCK("INV007", "current.stock.exceeds.max"),
    INVALID_PRICE_RANGE("INV008", "selling.price.less.than.cost"),
    INVALID_DISCOUNT_PRICE("INV009", "discount.price.exceeds.selling"),
    INVALID_DISCOUNT_DATES("INV010", "discount.start.after.end"),
    INVALID_EXPIRY_DATE("INV011", "expiry.date.must.be.future"),
    INVALID_EXPIRY_FOR_NON_PERISHABLE("INV012", "expiry.date.must.be.null.for.non.perishable"),
    MISSING_REQUIRED_FIELD("INV013", "missing.required.field"),
    INVALID_UNIT_VALUE("INV014", "invalid.unit.value"),
    INVALID_CURRENCY("INV015", "invalid.currency"),
    NAME_REQUIRED("INV016", "name.required"),
    UNIT_REQUIRED("INV017", "unit.required"),
    STOCK_CANNOT_BE_NEGATIVE("INV018", "stock.cannot.be.negative"),
    MAX_STOCK_LESS_THAN_MIN("INV019", "max.stock.less.than.min"),
    CURRENT_STOCK_EXCEEDS_MAX("INV020", "current.stock.exceeds.max"),
    CURRENCY_REQUIRED_FOR_COST_PRICE("INV021", "currency.required.for.cost.price"),
    CURRENCY_REQUIRED_FOR_SELLING_PRICE("INV022", "currency.required.for.selling.price"),
    SELLING_PRICE_LESS_THAN_COST("INV023", "selling.price.less.than.cost"),
    SELLING_PRICE_REQUIRED_FOR_DISCOUNT("INV024", "selling.price.required.for.discount"),
    DISCOUNT_PRICE_EXCEEDS_SELLING("INV025", "discount.price.exceeds.selling"),
    DISCOUNT_DATES_REQUIRED("INV026", "discount.dates.required"),
    DISCOUNT_START_AFTER_END("INV027", "discount.start.after.end"),
    EXPIRY_DATE_REQUIRED_FOR_PERISHABLE("INV028", "expiry.date.required.for.perishable"),
    EXPIRY_DATE_MUST_BE_FUTURE("INV029", "expiry.date.must.be.future"),
    EXPIRY_DATE_MUST_BE_NULL_FOR_NON_PERISHABLE("INV030", "expiry.date.must.be.null.for.non.perishable"),
    SKU_ALREADY_EXISTS("INV031", "sku.already.exists"),
    BARCODE_ALREADY_EXISTS("INV032", "barcode.already.exists"),
    PRODUCT_CODE_ALREADY_EXISTS("INV033", "product.code.already.exists"),

    // Business Rule Errors (INV100-INV199)
    INVENTORY_NOT_FOUND("INV100", "inventory.not.found"),
    INVENTORY_DISABLED("INV101", "inventory.disabled"),

    // Data Processing Errors (INV200-INV299)
    INVALID_FILE_FORMAT("INV200", "Invalid file format"),
    FILE_TOO_LARGE("INV201", "File size exceeds maximum limit"),
    INVALID_CSV_STRUCTURE("INV202", "Invalid CSV structure or missing columns"),
    PARSE_ERROR("INV203", "Error parsing data from file"),
    BATCH_PROCESSING_ERROR("INV204", "Error during batch processing"),

    // Database Errors (INV300-INV399)
    DATABASE_CONNECTION_ERROR("INV300", "Database connection error"),
    TRANSACTION_FAILED("INV301", "Transaction failed"),
    CONSTRAINT_VIOLATION("INV302", "Database constraint violation"),
    OPTIMISTIC_LOCK_ERROR("INV303", "Concurrent modification detected"),

    // System Errors (INV400-INV499)
    INTERNAL_SERVER_ERROR("INV400", "Internal server error"),
    SERVICE_UNAVAILABLE("INV401", "Service temporarily unavailable"),
    TIMEOUT_ERROR("INV402", "Operation timed out"),
    RESOURCE_NOT_FOUND("INV403", "Resource not found"),

    // Excel Upload Specific Errors (INV500-INV599)
    EXCEL_PARSE_ERROR("INV500", "Error parsing Excel file"),
    INVALID_ROW_DATA("INV501", "Invalid data in row"),
    MISSING_REQUIRED_COLUMNS("INV502", "Required columns are missing"),
    INVALID_DATA_TYPE("INV503", "Invalid data type for field"),
    ROW_VALIDATION_FAILED("INV504", "Row validation failed"),
    BATCH_VALIDATION_FAILED("INV505", "Batch validation failed"),

    // Inventory Movement Errors (INV600-INV699)
    INSUFFICIENT_STOCK("INV600", "insufficient.stock"),
    INVALID_MOVEMENT_TYPE("INV601", "invalid.movement.type"),
    MOVEMENT_VALIDATION_FAILED("INV602", "movement.validation.failed"),
    STOCK_ADJUSTMENT_ERROR("INV603", "stock.adjustment.error"),

    // User Access Control Errors (INV700-INV799)
    USER_NOT_ORGANIZATION_CREATOR("INV700", "user.not.organization.creator"),
    USER_NOT_AUTHENTICATED("INV701", "user.not.authenticated"),

    // Inventory Item Operation Errors (INV800-INV899)
    INVENTORY_ITEM_NOT_FOUND("INV800", "inventory.item.not.found"),
    ITEM_CREATION_FAILED("INV801", "item.creation.failed");

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
