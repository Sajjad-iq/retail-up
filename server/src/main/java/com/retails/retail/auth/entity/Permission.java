package com.retails.retail.auth.entity;

import jakarta.persistence.*;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Size;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.UUID;

/**
 * Permission entity representing specific system permissions
 * Maps to the frontend Permission interface
 */
@Entity
@Table(name = "permissions", indexes = {
        @Index(name = "idx_permission_name", columnList = "name", unique = true),
        @Index(name = "idx_permission_category", columnList = "category"),
        @Index(name = "idx_permission_system", columnList = "isSystem")
})
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class Permission {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    @Column(name = "id", updatable = false, nullable = false)
    private UUID id;

    @NotBlank(message = "Permission name is required")
    @Size(max = 50, message = "Permission name must not exceed 50 characters")
    @Column(name = "name", nullable = false, unique = true, length = 50)
    private String name;

    @NotBlank(message = "Permission label is required")
    @Size(max = 100, message = "Permission label must not exceed 100 characters")
    @Column(name = "label", nullable = false, length = 100)
    private String label;

    @Size(max = 255, message = "Description must not exceed 255 characters")
    @Column(name = "description")
    private String description;

    @Enumerated(EnumType.STRING)
    @Column(name = "category", nullable = false)
    private PermissionCategory category;

    @Column(name = "is_system", nullable = false)
    @Builder.Default
    private Boolean isSystem = false;

    /**
     * Permission category enumeration
     * Maps to the frontend PermissionCategory type
     */
    public enum PermissionCategory {
        POS("pos"),
        INVENTORY("inventory"),
        USERS("users"),
        REPORTS("reports"),
        SETTINGS("settings"),
        ADMIN("admin");

        private final String value;

        PermissionCategory(String value) {
            this.value = value;
        }

        public String getValue() {
            return value;
        }

        public static PermissionCategory fromString(String value) {
            for (PermissionCategory category : PermissionCategory.values()) {
                if (category.value.equalsIgnoreCase(value)) {
                    return category;
                }
            }
            throw new IllegalArgumentException("Unknown permission category: " + value);
        }
    }

    /**
     * Check if permission can be deleted (not system permission)
     */
    public boolean canBeDeleted() {
        return !isSystem;
    }

    /**
     * Get display text for UI
     */
    public String getDisplayText() {
        return this.label != null ? this.label : this.name;
    }
}