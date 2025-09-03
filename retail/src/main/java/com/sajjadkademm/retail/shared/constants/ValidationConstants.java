package com.sajjadkademm.retail.shared.constants;

/**
 * Validation-related constants used throughout the application.
 * Centralizes validation rules and constraints for consistency.
 */
public final class ValidationConstants {
    
    private ValidationConstants() {
        // Utility class - prevent instantiation
    }
    
    // String Length Constraints
    public static final int MAX_NAME_LENGTH = 100;
    public static final int MAX_DESCRIPTION_LENGTH = 500;
    public static final int MAX_EMAIL_LENGTH = 255;
    public static final int MAX_PHONE_LENGTH = 20;
    public static final int MAX_ADDRESS_LENGTH = 255;
    public static final int MAX_BARCODE_LENGTH = 50;
    public static final int MAX_PRODUCT_CODE_LENGTH = 50;
    public static final int MAX_CATEGORY_LENGTH = 100;
    public static final int MAX_BRAND_LENGTH = 100;
    public static final int MAX_SUPPLIER_LENGTH = 100;
    
    // Numeric Constraints
    public static final int MIN_STOCK_VALUE = 0;
    public static final int MAX_STOCK_VALUE = 999999;
    public static final int MIN_PRICE_SCALE = 2;
    public static final int MAX_PRICE_PRECISION = 10;
    
    // Pagination Constraints
    public static final int DEFAULT_PAGE_SIZE = 20;
    public static final int MAX_PAGE_SIZE = 100;
    public static final int MIN_PAGE_NUMBER = 0;
    
    // Business Rules
    public static final int MIN_ORGANIZATION_NAME_LENGTH = 2;
    public static final int MAX_ORGANIZATION_NAME_LENGTH = 100;
    public static final int MAX_FILE_UPLOAD_SIZE_MB = 10;
    
    // Regex Patterns
    public static final String EMAIL_PATTERN = "^[A-Za-z0-9+_.-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}$";
    public static final String PHONE_PATTERN = "^[+]?[0-9]{10,15}$";
    public static final String BARCODE_PATTERN = "^[0-9]{8,13}$";
}