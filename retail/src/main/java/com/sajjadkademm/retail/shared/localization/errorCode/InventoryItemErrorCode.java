package com.sajjadkademm.retail.shared.localization.errorCode;

/**
 * Centralized error codes for inventory operations.
 * Provides consistent error handling across all inventory-related services.
 * 
 * @author Sajjad Kadem
 * @version 1.0
 * @since 2024-12-19
 */
public enum InventoryItemErrorCode {

    // Validation Errors (INV001-INV099)
    NAME_REQUIRED("INV001", "name.required"),
    UNIT_REQUIRED("INV002", "unit.required"),
    STOCK_CANNOT_BE_NEGATIVE("INV003", "stock.cannot.be.negative"),
    MAX_STOCK_LESS_THAN_MIN("INV004", "max.stock.less.than.min"),
    CURRENT_STOCK_EXCEEDS_MAX("INV005", "current.stock.exceeds.max"),
    CURRENCY_REQUIRED_FOR_COST_PRICE("INV006", "currency.required.for.cost.price"),
    CURRENCY_REQUIRED_FOR_SELLING_PRICE("INV007", "currency.required.for.selling.price"),
    SELLING_PRICE_LESS_THAN_COST("INV008", "selling.price.less.than.cost"),
    SELLING_PRICE_REQUIRED_FOR_DISCOUNT("INV009", "selling.price.required.for.discount"),
    DISCOUNT_PRICE_EXCEEDS_SELLING("INV010", "discount.price.exceeds.selling"),
    DISCOUNT_DATES_REQUIRED("INV011", "discount.dates.required"),
    DISCOUNT_START_AFTER_END("INV012", "discount.start.after.end"),
    EXPIRY_DATE_REQUIRED_FOR_PERISHABLE("INV013", "expiry.date.required.for.perishable"),
    EXPIRY_DATE_MUST_BE_FUTURE("INV014", "expiry.date.must.be.future"),
    EXPIRY_DATE_MUST_BE_NULL_FOR_NON_PERISHABLE("INV015", "expiry.date.must.be.null.for.non.perishable"),
    SKU_ALREADY_EXISTS("INV016", "sku.already.exists"),
    BARCODE_ALREADY_EXISTS("INV017", "barcode.already.exists"),
    PRODUCT_CODE_ALREADY_EXISTS("INV018", "product.code.already.exists"),

    // Inventory Item Operation Errors (INV800-INV899)
    INVENTORY_ITEM_NOT_FOUND("INV800", "inventory.item.not.found"),
    ITEM_CREATION_FAILED("INV801", "item.creation.failed");

    private final String code;
    private final String message;

    InventoryItemErrorCode(String code, String message) {
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
    public static InventoryItemErrorCode fromCode(String code) {
        for (InventoryItemErrorCode errorCode : values()) {
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
