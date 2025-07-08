package com.sajjadkademm.retail.auth.entities;

import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.hibernate.annotations.CreationTimestamp;
import org.hibernate.annotations.UpdateTimestamp;

import java.time.LocalDateTime;

/**
 * Permission entity representing specific system permissions.
 * Provides granular access control for different system features.
 * 
 * @author Sajjad Kadem
 * @version 1.0
 * @since 2024-12-19
 */
@Entity
@Table(name = "permissions", indexes = {
        @Index(name = "idx_permission_name", columnList = "name"),
        @Index(name = "idx_permission_category", columnList = "category")
})
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class Permission {

    /**
     * Unique identifier for the permission
     */
    @Id
    @Column(name = "id", length = 50)
    private String id;

    /**
     * Permission name/code (e.g., "pos.view", "users.manage")
     */
    @Column(name = "name", nullable = false, unique = true, length = 100)
    private String name;

    /**
     * Human-readable permission label
     */
    @Column(name = "label", nullable = false, length = 100)
    private String label;

    /**
     * Detailed permission description
     */
    @Column(name = "description", length = 500)
    private String description;

    /**
     * Permission category/module
     */
    @Enumerated(EnumType.STRING)
    @Column(name = "category", nullable = false, length = 20)
    private PermissionCategory category;

    /**
     * Whether this is a system permission (cannot be deleted)
     */
    @Column(name = "is_system", nullable = false)
    private Boolean isSystem = false;

    /**
     * When the permission was created
     */
    @CreationTimestamp
    @Column(name = "created_at", nullable = false, updatable = false)
    private LocalDateTime createdAt;

    /**
     * When the permission was last updated
     */
    @UpdateTimestamp
    @Column(name = "updated_at", nullable = false)
    private LocalDateTime updatedAt;

    /**
     * Permission category enumeration
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

        public static PermissionCategory fromValue(String value) {
            for (PermissionCategory category : values()) {
                if (category.value.equals(value)) {
                    return category;
                }
            }
            throw new IllegalArgumentException("Unknown permission category: " + value);
        }
    }
}