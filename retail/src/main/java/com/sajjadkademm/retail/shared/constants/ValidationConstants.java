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


    // Language and Currency Patterns
    public static final String LANGUAGE_PATTERN = "^[a-z]{2}$";
    public static final String CURRENCY_PATTERN = "^[A-Z]{3}$";
    public static final String TIMEZONE_PATTERN = "^[A-Za-z_]+/[A-Za-z_]+$";
    
    // POS Method Pattern
    public static final String CHANGE_CALCULATION_METHOD_PATTERN = "^(automatic|manual)$";
    
    // Domain Pattern
    public static final String DOMAIN_PATTERN = "^(?!-)[A-Za-z0-9-]+(\\.[A-Za-z0-9-]+)*(?!-)$";
    
    // Regex Patterns
    public static final String EMAIL_PATTERN = "^[A-Za-z0-9+_.-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}$";
    public static final String PHONE_PATTERN = "^[+]?[0-9]{10,15}$";
    public static final String BARCODE_PATTERN = "^[0-9]{8,13}$";
    
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
    
    // Excel Upload Constants
    public static final int EXCEL_BATCH_SIZE = 50;
    public static final int EXCEL_EXPECTED_COLUMNS = 25;
    public static final int AUDIT_SIGNIFICANT_CHANGE_THRESHOLD = 100;
    
    // Domain Constraints
    public static final int MIN_DOMAIN_LENGTH = 3;
    public static final int MAX_DOMAIN_LENGTH = 255;
    
    // Product Physical Attributes
    public static final int MAX_DIMENSIONS_LENGTH = 50;
    public static final int MAX_COLOR_LENGTH = 50;
    public static final int MAX_SIZE_LENGTH = 20;
    
    // Phone Constraints
    public static final int MIN_PHONE_LENGTH = 10;
    
    // Password Constraints
    public static final int MIN_PASSWORD_LENGTH = 8;
    public static final int MAX_PASSWORD_LENGTH = 32;
    
    // System Settings Constraints
    public static final int MIN_BACKUP_RETENTION_DAYS = 1;
    public static final int MAX_BACKUP_RETENTION_DAYS = 365;
    public static final int MIN_EXPIRY_ALERT_DAYS = 1;
    public static final int MAX_EXPIRY_ALERT_DAYS = 365;
    
    // POS Settings Constraints
    public static final int MIN_RECEIPT_PAPER_WIDTH = 58;
    public static final int MAX_RECEIPT_PAPER_WIDTH = 112;
    
    // Inventory Settings Constraints
    public static final int MIN_LOW_STOCK_THRESHOLD = 1;
    public static final int MAX_LOW_STOCK_THRESHOLD = 1000;
    
    // ID Pattern (for user IDs, inventory IDs, etc.)
    public static final String ID_PATTERN = "^[a-zA-Z0-9_-]+$";
    public static final int MIN_ID_LENGTH = 20;
    public static final int MAX_ID_LENGTH = 255;
    

}