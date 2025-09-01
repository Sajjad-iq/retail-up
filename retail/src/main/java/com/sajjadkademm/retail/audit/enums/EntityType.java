package com.sajjadkademm.retail.audit.enums;

/**
 * ENTITY TYPES: Categories of entities that can be audited
 * 
 * DESIGN: Simple enum covering all major business entities
 * Keep this list focused on entities that need audit tracking
 */
public enum EntityType {
    // INVENTORY ENTITIES: Core inventory management
    INVENTORY_ITEM("Inventory Item", "Product or item in inventory"),
    INVENTORY("Inventory", "Inventory location or warehouse"),

    // ORGANIZATION ENTITIES: Business structure
    ORGANIZATION("Organization", "Business organization or company"),
    USER("User", "System user or employee"),

    // TRANSACTION ENTITIES: Business processes
    SALE("Sale", "Sales transaction"),
    PURCHASE("Purchase", "Purchase order or transaction"),
    PAYMENT("Payment", "Payment transaction"),

    // MASTER DATA: Reference data
    CUSTOMER("Customer", "Customer information"),
    SUPPLIER("Supplier", "Supplier information"),

    // CONFIGURATION: System settings
    SYSTEM_SETTING("System Setting", "System configuration"),
    USER_SETTING("User Setting", "User preferences"),

    // SECURITY: Access control
    ROLE("Role", "User role or permission group"),
    PERMISSION("Permission", "System permission"),

    // PROCESS TRACKING: Business workflows
    BUSINESS_PROCESS("Business Process", "Business workflow or operation"),

    // FILES: Document management
    DOCUMENT("Document", "Uploaded file or document"),

    // INTEGRATION: External systems
    API_CALL("API Call", "External API interaction"),
    EXPORT("Export", "Data export operation"),
    IMPORT("Import", "Data import operation");

    private final String displayName;
    private final String description;

    EntityType(String displayName, String description) {
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
     * HELPER METHODS: Make it easier to work with entity types
     */

    // Check if this entity type relates to inventory
    public boolean isInventoryRelated() {
        return this == INVENTORY_ITEM || this == INVENTORY;
    }

    // Check if this entity type relates to security
    public boolean isSecurityRelated() {
        return this == USER || this == ROLE || this == PERMISSION;
    }

    // Check if this entity type relates to transactions
    public boolean isTransactionRelated() {
        return this == SALE || this == PURCHASE || this == PAYMENT;
    }

    // Check if changes to this entity type should be considered sensitive
    public boolean isSensitiveByDefault() {
        return isSecurityRelated() || this == ORGANIZATION || this == SYSTEM_SETTING;
    }
}
