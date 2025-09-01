package com.sajjadkademm.retail.audit;

import jakarta.persistence.*;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Size;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.hibernate.annotations.CreationTimestamp;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.sajjadkademm.retail.audit.enums.AuditAction;
import com.sajjadkademm.retail.audit.enums.EntityType;
import com.sajjadkademm.retail.users.User;

import java.time.LocalDateTime;

/**
 * GLOBAL AUDIT LOG: Simple and effective organization-wide audit tracking
 * 
 * PURPOSE:
 * - Track ALL important changes across the organization
 * - Provide compliance audit trail
 * - Enable change history and forensic analysis
 * - Replace complex entity-specific logging
 * 
 * DESIGN PRINCIPLES:
 * - Simple: Few fields, easy to use
 * - Effective: Captures essential audit information
 * - Clean: Well-indexed and performant
 */
@Entity
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Table(name = "global_audit_logs", indexes = {
        // PERFORMANCE INDEXES: Optimized for common query patterns
        @Index(name = "idx_audit_org_time", columnList = "organization_id,created_at"), // Main queries
        @Index(name = "idx_audit_entity", columnList = "entity_type,entity_id"), // Entity history
        @Index(name = "idx_audit_action", columnList = "action"), // Action filtering
        @Index(name = "idx_audit_user", columnList = "performed_by"), // User activity
        @Index(name = "idx_audit_reference", columnList = "reference_type,reference_id") // Business process tracking
})
public class GlobalAuditLog {

    // PRIMARY KEY: Unique identifier for each audit record
    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    private String id;

    // ORGANIZATION SCOPE: Which organization this audit belongs to
    @Column(name = "organization_id", nullable = false, length = 50)
    @NotNull
    private String organizationId;

    // ENTITY IDENTIFICATION: What was changed
    @Column(name = "entity_type", nullable = false)
    @Enumerated(EnumType.STRING)
    @NotNull
    private EntityType entityType; // INVENTORY_ITEM, USER, ORGANIZATION, etc.

    @Column(name = "entity_id", nullable = false, length = 50)
    @NotNull
    private String entityId; // ID of the entity that was changed

    @Column(name = "entity_name", length = 255)
    private String entityName; // Human-readable name (e.g., "iPhone 13", "John Doe")

    // ACTION DETAILS: What happened
    @Column(name = "action", nullable = false)
    @Enumerated(EnumType.STRING)
    @NotNull
    private AuditAction action; // CREATE, UPDATE, DELETE, STOCK_IN, STOCK_OUT, etc.

    @Column(name = "description", length = 500)
    @Size(max = 500)
    private String description; // Human-readable description of what happened

    // CHANGE TRACKING: What changed (simple approach)
    @Column(name = "old_value", length = 1000)
    private String oldValue; // Previous value (for simple fields) or JSON for complex

    @Column(name = "new_value", length = 1000)
    private String newValue; // New value (for simple fields) or JSON for complex

    @Column(name = "field_name", length = 100)
    private String fieldName; // Name of the field that changed (for single field changes)

    // BUSINESS CONTEXT: Why it happened
    @Column(name = "business_process", length = 100)
    private String businessProcess; // "Excel Upload", "Manual Edit", "POS Sale", "API Call"

    @Column(name = "reference_type", length = 50)
    private String referenceType; // "SALE", "PURCHASE", "STOCK_TAKE", "USER_ACTION"

    @Column(name = "reference_id", length = 100)
    private String referenceId; // ID of the business process that triggered this change

    // QUANTITY TRACKING: For inventory-related changes
    @Column(name = "quantity_change")
    private Integer quantityChange; // +10, -5, etc. (null for non-quantity changes)

    @Column(name = "quantity_before")
    private Integer quantityBefore; // Stock level before change (for inventory)

    @Column(name = "quantity_after")
    private Integer quantityAfter; // Stock level after change (for inventory)

    // AUDIT METADATA: When and who
    @CreationTimestamp
    @Column(name = "created_at", nullable = false)
    private LocalDateTime createdAt;

    @ManyToOne(optional = false, fetch = FetchType.LAZY)
    @JsonIgnoreProperties({ "password", "phone", "lastLoginAt" })
    @JoinColumn(name = "performed_by", referencedColumnName = "id", nullable = false)
    private User performedBy;

    // TECHNICAL CONTEXT: Additional tracking info
    @Column(name = "source_ip", length = 45)
    private String sourceIp; // IP address where the action originated

    @Column(name = "user_agent", length = 500)
    private String userAgent; // Browser/application information

    // SIMPLE SEVERITY: Basic risk classification
    @Column(name = "is_sensitive")
    private Boolean isSensitive; // true for important/risky operations, false for routine

    /**
     * HELPER METHODS: Make audit logging easier
     */

    // Factory method for inventory changes
    public static GlobalAuditLog forInventoryChange(String organizationId, String itemId, String itemName,
            AuditAction action, Integer quantityChange,
            Integer stockBefore, Integer stockAfter,
            String reason, User user) {
        return GlobalAuditLog.builder()
                .organizationId(organizationId)
                .entityType(EntityType.INVENTORY_ITEM)
                .entityId(itemId)
                .entityName(itemName)
                .action(action)
                .description(reason)
                .quantityChange(quantityChange)
                .quantityBefore(stockBefore)
                .quantityAfter(stockAfter)
                .businessProcess("Inventory Management")
                .performedBy(user)
                .isSensitive(Math.abs(quantityChange != null ? quantityChange : 0) > 100) // Large changes are sensitive
                .build();
    }

    // Factory method for general entity changes
    public static GlobalAuditLog forEntityChange(String organizationId, EntityType entityType, String entityId,
            String entityName, AuditAction action, String description,
            String oldValue, String newValue, String fieldName, User user) {
        return GlobalAuditLog.builder()
                .organizationId(organizationId)
                .entityType(entityType)
                .entityId(entityId)
                .entityName(entityName)
                .action(action)
                .description(description)
                .oldValue(oldValue)
                .newValue(newValue)
                .fieldName(fieldName)
                .performedBy(user)
                .isSensitive(action == AuditAction.DELETE || action == AuditAction.USER_DELETE)
                .build();
    }

    // Factory method for business process tracking
    public static GlobalAuditLog forBusinessProcess(String organizationId, String processName, String description,
            String referenceType, String referenceId, User user) {
        return GlobalAuditLog.builder()
                .organizationId(organizationId)
                .entityType(EntityType.BUSINESS_PROCESS)
                .entityId(referenceId)
                .entityName(processName)
                .action(AuditAction.PROCESS_EXECUTE)
                .description(description)
                .businessProcess(processName)
                .referenceType(referenceType)
                .referenceId(referenceId)
                .performedBy(user)
                .isSensitive(false)
                .build();
    }
}
