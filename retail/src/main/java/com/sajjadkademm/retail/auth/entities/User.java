package com.sajjadkademm.retail.auth.entities;

import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.hibernate.annotations.CreationTimestamp;
import org.hibernate.annotations.UpdateTimestamp;

import java.time.LocalDateTime;
import java.util.Set;
import java.util.HashSet;

/**
 * User entity representing system users.
 * Users have direct permissions without intermediate roles.
 * 
 * @author Sajjad Kadem
 * @version 1.0
 * @since 2024-12-19
 */
@Entity
@Table(name = "users", indexes = {
        @Index(name = "idx_user_email", columnList = "email"),
        @Index(name = "idx_user_employee_id", columnList = "employee_id"),
        @Index(name = "idx_user_status", columnList = "status"),
        @Index(name = "idx_user_department", columnList = "department")
})
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class User {

    /**
     * Unique identifier for the user
     */
    @Id
    @Column(name = "id", length = 50)
    private String id;

    /**
     * User's display name
     */
    @Column(name = "name", nullable = false, length = 100)
    private String name;

    /**
     * User's email address (unique)
     */
    @Column(name = "email", nullable = false, unique = true, length = 150)
    private String email;

    /**
     * User's encrypted password
     */
    @Column(name = "password", nullable = false, length = 255)
    private String password;

    /**
     * User's phone number
     */
    @Column(name = "phone", length = 20)
    private String phone;

    /**
     * User's avatar URL
     */
    @Column(name = "avatar", length = 500)
    private String avatar;

    /**
     * User's status
     */
    @Enumerated(EnumType.STRING)
    @Column(name = "status", nullable = false, length = 20)
    private UserStatus status;

    /**
     * User's department
     */
    @Column(name = "department", length = 100)
    private String department;

    /**
     * Employee ID
     */
    @Column(name = "employee_id", length = 50)
    private String employeeId;

    /**
     * Whether user must change password on next login
     */
    @Column(name = "must_change_password", nullable = false)
    private Boolean mustChangePassword = false;

    /**
     * When the user was created
     */
    @CreationTimestamp
    @Column(name = "created_at", nullable = false, updatable = false)
    private LocalDateTime createdAt;

    /**
     * When the user was last updated
     */
    @UpdateTimestamp
    @Column(name = "updated_at", nullable = false)
    private LocalDateTime updatedAt;

    /**
     * Date when user last logged in
     */
    @Column(name = "last_login_at")
    private LocalDateTime lastLoginAt;

    /**
     * Direct permissions assigned to this user
     */
    @ManyToMany(fetch = FetchType.LAZY, cascade = { CascadeType.MERGE })
    @JoinTable(name = "user_permissions", joinColumns = @JoinColumn(name = "user_id"), inverseJoinColumns = @JoinColumn(name = "permission_id"), indexes = {
            @Index(name = "idx_user_permissions_user", columnList = "user_id"),
            @Index(name = "idx_user_permissions_permission", columnList = "permission_id")
    })
    @Builder.Default
    private Set<Permission> permissions = new HashSet<>();

    /**
     * User status enumeration
     */
    public enum UserStatus {
        ACTIVE("active"),
        INACTIVE("inactive"),
        SUSPENDED("suspended"),
        PENDING("pending");

        private final String value;

        UserStatus(String value) {
            this.value = value;
        }

        public String getValue() {
            return value;
        }

        public static UserStatus fromValue(String value) {
            for (UserStatus status : values()) {
                if (status.value.equals(value)) {
                    return status;
                }
            }
            throw new IllegalArgumentException("Unknown user status: " + value);
        }
    }

    /**
     * Helper method to add a permission
     */
    public void addPermission(Permission permission) {
        this.permissions.add(permission);
    }

    /**
     * Helper method to remove a permission
     */
    public void removePermission(Permission permission) {
        this.permissions.remove(permission);
    }

    /**
     * Helper method to check if user has a specific permission
     */
    public boolean hasPermission(String permissionName) {
        return this.permissions.stream()
                .anyMatch(permission -> permission.getName().equals(permissionName));
    }

    /**
     * Helper method to check if user has any permission in a category
     */
    public boolean hasPermissionInCategory(Permission.PermissionCategory category) {
        return this.permissions.stream()
                .anyMatch(permission -> permission.getCategory() == category);
    }
}