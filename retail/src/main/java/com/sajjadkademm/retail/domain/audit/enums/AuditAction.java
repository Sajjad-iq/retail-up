package com.sajjadkademm.retail.domain.audit.enums;

/**
 * AUDIT ACTIONS: Types of actions that can be audited
 * 
 * DESIGN: Comprehensive but focused set of actions
 * Covers both generic CRUD operations and business-specific actions
 */
public enum AuditAction {
    // STANDARD CRUD OPERATIONS: Basic entity lifecycle
    CREATE("Create", "Entity was created"),
    UPDATE("Update", "Entity was modified"),
    DELETE("Delete", "Entity was deleted"),
    VIEW("View", "Entity was accessed/viewed"),

    // INVENTORY SPECIFIC ACTIONS: Stock management
    STOCK_IN("Stock In", "Inventory increased"),
    STOCK_OUT("Stock Out", "Inventory decreased"),
    STOCK_ADJUST("Stock Adjust", "Inventory quantity corrected"),
    STOCK_TRANSFER("Stock Transfer", "Inventory moved between locations"),
    STOCK_COUNT("Stock Count", "Physical inventory count performed"),

    // USER AND SECURITY ACTIONS: Access control and authentication
    USER_LOGIN("User Login", "User logged into system"),
    USER_LOGOUT("User Logout", "User logged out of system"),
    USER_CREATE("User Create", "New user account created"),
    USER_UPDATE("User Update", "User account modified"),
    USER_DELETE("User Delete", "User account deleted"),
    USER_ACTIVATE("User Activate", "User account activated"),
    USER_DEACTIVATE("User Deactivate", "User account deactivated"),
    PASSWORD_CHANGE("Password Change", "User password changed"),
    PERMISSION_GRANT("Permission Grant", "Permission granted to user"),
    PERMISSION_REVOKE("Permission Revoke", "Permission revoked from user"),

    // BUSINESS TRANSACTION ACTIONS: Core business processes
    SALE_CREATE("Sale Create", "Sale transaction created"),
    SALE_COMPLETE("Sale Complete", "Sale transaction completed"),
    SALE_CANCEL("Sale Cancel", "Sale transaction cancelled"),
    PAYMENT_PROCESS("Payment Process", "Payment processed"),
    PAYMENT_REFUND("Payment Refund", "Payment refunded"),
    PURCHASE_CREATE("Purchase Create", "Purchase order created"),
    PURCHASE_RECEIVE("Purchase Receive", "Purchase order received"),

    // DATA OPERATIONS: Import/Export and bulk operations
    BULK_CREATE("Bulk Create", "Multiple entities created"),
    BULK_UPDATE("Bulk Update", "Multiple entities updated"),
    BULK_DELETE("Bulk Delete", "Multiple entities deleted"),
    EXCEL_IMPORT("Excel Import", "Data imported from Excel file"),
    EXCEL_EXPORT("Excel Export", "Data exported to Excel file"),
    DATA_EXPORT("Data Export", "Data exported"),
    DATA_IMPORT("Data Import", "Data imported"),

    // SYSTEM OPERATIONS: Configuration and maintenance
    SETTING_UPDATE("Setting Update", "System setting changed"),
    BACKUP_CREATE("Backup Create", "System backup created"),
    BACKUP_RESTORE("Backup Restore", "System backup restored"),
    SYSTEM_MAINTENANCE("System Maintenance", "System maintenance performed"),

    // PROCESS TRACKING: Business workflow tracking
    PROCESS_START("Process Start", "Business process started"),
    PROCESS_EXECUTE("Process Execute", "Business process executed"),
    PROCESS_COMPLETE("Process Complete", "Business process completed"),
    PROCESS_FAIL("Process Fail", "Business process failed"),

    // SECURITY EVENTS: Security-related activities
    FAILED_LOGIN("Failed Login", "Login attempt failed"),
    SUSPICIOUS_ACTIVITY("Suspicious Activity", "Potentially suspicious action detected"),
    ACCESS_DENIED("Access Denied", "Access to resource denied"),

    // ORGANIZATION ACTIONS: Organizational changes
    ORG_CREATE("Organization Create", "Organization created"),
    ORG_UPDATE("Organization Update", "Organization updated"),
    ORG_ACTIVATE("Organization Activate", "Organization activated"),
    ORG_DEACTIVATE("Organization Deactivate", "Organization deactivated");

    private final String displayName;
    private final String description;

    AuditAction(String displayName, String description) {
        this.displayName = displayName;
        this.description = description;
    }

    public String getDisplayName() {
        return displayName;
    }

    public String getDescription() {
        return description;
    }

    /**
     * HELPER METHODS: Categorize actions for different use cases
     */

    // Check if this action represents a data change
    public boolean isDataChange() {
        return this == CREATE || this == UPDATE || this == DELETE ||
                this == BULK_CREATE || this == BULK_UPDATE || this == BULK_DELETE;
    }

    // Check if this action represents an inventory movement
    public boolean isInventoryMovement() {
        return this == STOCK_IN || this == STOCK_OUT || this == STOCK_ADJUST ||
                this == STOCK_TRANSFER || this == STOCK_COUNT;
    }

    // Check if this action represents a security event
    public boolean isSecurityEvent() {
        return this == USER_LOGIN || this == USER_LOGOUT || this == FAILED_LOGIN ||
                this == SUSPICIOUS_ACTIVITY || this == ACCESS_DENIED ||
                this == PERMISSION_GRANT || this == PERMISSION_REVOKE;
    }

    // Check if this action should be considered high-risk
    public boolean isHighRisk() {
        return this == DELETE || this == BULK_DELETE || this == USER_DELETE ||
                this == ORG_DEACTIVATE || this == BACKUP_RESTORE ||
                this == SUSPICIOUS_ACTIVITY || this == PERMISSION_GRANT;
    }

    // Check if this action represents a business transaction
    public boolean isBusinessTransaction() {
        return this == SALE_CREATE || this == SALE_COMPLETE || this == PAYMENT_PROCESS ||
                this == PURCHASE_CREATE || this == PURCHASE_RECEIVE;
    }

    // Map inventory movement types to audit actions
    public static AuditAction fromInventoryMovementType(String movementType) {
        if (movementType == null)
            return STOCK_ADJUST;

        switch (movementType.toUpperCase()) {
            case "STOCK_IN":
            case "PURCHASE":
            case "RETURN":
            case "ADJUSTMENT_IN":
            case "TRANSFER_IN":
                return STOCK_IN;

            case "STOCK_OUT":
            case "SALE":
            case "DAMAGE":
            case "THEFT":
            case "EXPIRED":
            case "ADJUSTMENT_OUT":
            case "TRANSFER_OUT":
                return STOCK_OUT;

            case "TRANSFER":
                return STOCK_TRANSFER;

            case "STOCK_TAKE":
                return STOCK_COUNT;

            default:
                return STOCK_ADJUST;
        }
    }
}
