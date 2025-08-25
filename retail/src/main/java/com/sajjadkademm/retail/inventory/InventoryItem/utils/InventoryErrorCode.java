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
    INVALID_SKU_FORMAT("INV001", "Invalid SKU format"),
    DUPLICATE_SKU("INV002", "SKU already exists in this inventory"),
    DUPLICATE_BARCODE("INV003", "Barcode already exists in this inventory"),
    DUPLICATE_PRODUCT_CODE("INV004", "Product code already exists in this inventory"),
    INVALID_STOCK_LEVEL("INV005", "Stock level cannot be negative"),
    INVALID_MIN_MAX_STOCK("INV006", "Maximum stock cannot be less than minimum stock"),
    INVALID_CURRENT_STOCK("INV007", "Current stock cannot exceed maximum stock"),
    INVALID_PRICE_RANGE("INV008", "Selling price cannot be less than cost price"),
    INVALID_DISCOUNT_PRICE("INV009", "Discount price cannot exceed selling price"),
    INVALID_DISCOUNT_DATES("INV010", "Discount start date cannot be after end date"),
    INVALID_EXPIRY_DATE("INV011", "Expiry date must be in the future for perishable items"),
    INVALID_EXPIRY_FOR_NON_PERISHABLE("INV012", "Expiry date must be null for non-perishable items"),
    MISSING_REQUIRED_FIELD("INV013", "Required field is missing"),
    INVALID_UNIT_VALUE("INV014", "Invalid unit value"),
    INVALID_CURRENCY("INV015", "Invalid or missing currency"),
    NAME_REQUIRED("INV016", "Name is required"),
    UNIT_REQUIRED("INV017", "Unit is required"),
    STOCK_CANNOT_BE_NEGATIVE("INV018", "Stock cannot be negative"),
    MAX_STOCK_LESS_THAN_MIN("INV019", "Maximum stock cannot be less than minimum stock"),
    CURRENT_STOCK_EXCEEDS_MAX("INV020", "Current stock cannot exceed maximum stock"),
    CURRENCY_REQUIRED_FOR_COST_PRICE("INV021", "Currency is required for cost price"),
    CURRENCY_REQUIRED_FOR_SELLING_PRICE("INV022", "Currency is required for selling price"),
    SELLING_PRICE_LESS_THAN_COST("INV023", "Selling price cannot be less than cost price"),
    SELLING_PRICE_REQUIRED_FOR_DISCOUNT("INV024", "Selling price is required when discount price is provided"),
    DISCOUNT_PRICE_EXCEEDS_SELLING("INV025", "Discount price cannot exceed selling price"),
    DISCOUNT_DATES_REQUIRED("INV026", "Discount start and end dates are required when discount price is provided"),
    DISCOUNT_START_AFTER_END("INV027", "Discount start date cannot be after discount end date"),
    EXPIRY_DATE_REQUIRED_FOR_PERISHABLE("INV028", "Expiry date is required for perishable items"),
    EXPIRY_DATE_MUST_BE_FUTURE("INV029", "Expiry date must be in the future for perishable items"),
    EXPIRY_DATE_MUST_BE_NULL_FOR_NON_PERISHABLE("INV030", "Expiry date must be null for non-perishable items"),
    SKU_ALREADY_EXISTS("INV031", "Item with SKU already exists in this inventory"),
    BARCODE_ALREADY_EXISTS("INV032", "Item with barcode already exists in this inventory"),
    PRODUCT_CODE_ALREADY_EXISTS("INV033", "Item with product code already exists in this inventory"),

    // Business Rule Errors (INV100-INV199)
    INVENTORY_NOT_FOUND("INV100", "Inventory not found"),
    INVENTORY_DISABLED("INV101", "Inventory is disabled"),

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
    INSUFFICIENT_STOCK("INV600", "Insufficient stock for operation"),
    INVALID_MOVEMENT_TYPE("INV601", "Invalid movement type"),
    MOVEMENT_VALIDATION_FAILED("INV602", "Movement validation failed"),
    STOCK_ADJUSTMENT_ERROR("INV603", "Error adjusting stock levels");

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
